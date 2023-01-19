library('ggplot2')
library('mlr')
data_train <- data.frame(read.table(gzfile("zip.train.gz")))
labels<- which(data_train[, 1] == 2 | data_train[, 1] == 3)
data_train2 <- data_train[labels, ]
X_train <- data_train[labels, -1]
y_train <- data_train[labels, 1]

data_test <- data.frame(read.table(gzfile("zip.test.gz")))
labels<- which(data_test[, 1] == 2 | data_test[, 1] == 3)
X_test  <- data_test [labels, -1]
y_test <- data_test [labels, 1]

linear_task<- makeRegrTask(data=data_train2,target='V1')
linear_learner <-makeLearner('regr.lm')
linear_model <- train(linear_learner,linear_task)
prediciton_train<-predict(linear_model , newdata = X_train)$data$response
prediciton_test<-predict(linear_model , newdata = X_test)$data$response
#Set threshold as 2.5
prediciton_label_train<-ifelse(prediciton_train>2.5,3,2)
prediciton_label_test<-ifelse(prediciton_test>2.5,3,2)

error_train_lm <- mean(prediciton_label_train != y_train)
error_test_lm <- mean(prediciton_label_test != y_test)


#knn
K = c(1,3,5,7,15)
error_train_knn = list()
error_test_knn = list()
for (i in 1:5) {
data_train2[,1]<-as.factor(data_train2[,1])
knn_task<-makeClassifTask(data=data_train2,target='V1')
knn_learner <-makeLearner('classif.knn',k=K[i])
knn_model <- train(knn_learner,knn_task)
prediciton_train<-predict(knn_model, newdata = X_train)$data$response
prediciton_test<-predict(knn_model, newdata = X_test)$data$response
error_train_knn[i] <- mean(prediciton_train != y_train)
error_test_knn[i] <- mean(prediciton_test != y_test) }

error_train_knn = as.numeric((unlist(error_train_knn)))
error_test_knn = as.numeric((unlist(error_test_knn)))

ggplot() + geom_line(aes(x = K, y = error_train_knn,col="K-nearest-models"))+
  geom_line(aes(x = K, error_train_lm,col="Linear regression"))+
  geom_point(aes(x = K, error_train_knn))+
  guides(color=guide_legend(title = "Methods"))+
  xlab("K values for knn")+
  ylab('Training set error')

ggplot() + geom_line(aes(x = K, y = error_test_knn,col="K-nearest-models"))+
  geom_line(aes(x = K, y=error_test_lm,col="Linear regression"))+
  geom_point(aes(x = K, error_test_knn))+
  guides(color=guide_legend(title = "Methods"))+
  xlab("K values for knn")+
  ylab('Test set error')
