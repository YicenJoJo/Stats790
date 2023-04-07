#Load the packages
library(dplyr) # joining, organizing
library(magrittr) # pip operators
library(readr)# reading data
library(tidyr) # gather and spread
library(stringr) # regular expression
library(lubridate)# date and time
library(ggplot2)
library(GGally)
library(gplots)
library(RColorBrewer)
library(corrplot)
library(caret)
library(ggpubr)
library(MASS)
library(class)
library(ISLR2)
library(boot)
library(rpart)
library(randomForest)
library(adabag)
library(glmnet)
library(leaps)
library(ROCR)
library(pROC)
library(ROCit)
library(plotROC)
library(reghelper)

#Load the dataset
#url <- 'https://www.kaggle.com/datasets/deepcontractor/smoke-detection-dataset'

SDD = read.csv("smoke_detection_iot.csv")

#Summary of the dataset including data types and number of observations
head(SDD)
summary(SDD)
str(SDD)
names(SDD)

#Drop the redundant Index column
SDD <- SDD[, !names(SDD) %in% c('X')]

#Missing values detection 
SDD2 <- dplyr::filter(SDD, !is.na(SDD$Fire.Alarm))
# label <- factor(SDD2$Fire.Alarm,labels =c('No','Yes'),order=TRUE)


#Outliers detection
p3 <- ggplot(data = SDD2, mapping = aes(y = Temperature.C., x = factor(Fire.Alarm,labels=c('No','Yes')), fill = factor(Fire.Alarm,labels=c('No','Yes')))) + 
  geom_boxplot(outlier.colour='red', outlier.shape=15)+
  theme(legend.position = "none") +
  theme_bw() +
  scale_fill_manual(values = c("blue2", "orange"))+
  labs(x="Fire alarm",fill = "Fire alarm")

p3

QL <- quantile(SDD2$Temperature.C., probs = 0.25)
QU <- quantile(SDD2$Temperature.C., probs = 0.75)
QU_QL <- QU-QL
QL;QU;QU_QL

which(SDD2$Temperature.C. > QU + 2*QU_QL & SDD2$Fire.Alarm==0)
SDD2 <- SDD2[-which(SDD2$Temperature.C. > QU + 2*QU_QL & SDD2$Fire.Alarm==0),]



# Overall plot for alarm counts
plot_title = 'Fire alarm counts'
plot_title2 = 'Fire alarm counts across different temperatures'

p1<-ggplot(data = SDD2) +
  geom_bar(
    aes(x = factor(Fire.Alarm,labels=c("No","Yes"))
    ),
    stat = 'count',width=0.5, color='red',fill='steelblue'
  )+ labs(x="Fire alarm", y = "Counts",title=plot_title)

p1



p2 <- ggplot(data = SDD2,
            mapping = aes(x = Temperature.C., fill = factor(Fire.Alarm,labels=c("No","Yes")
                         )))+
      geom_histogram(binwidth = 8)+
      labs(x="Temperature.C.", y = "Counts",title=plot_title2, fill = "Fire alarm")
p2

set.seed(1)
SDD2_subset <- bind_rows(
  dplyr::filter(SDD2, Fire.Alarm == 0),
  dplyr::filter(SDD2, Fire.Alarm == 1) %>% slice(sample(1:dim(SDD2)[1], size = 500)))


sp1<-ggplot(data = SDD2_subset ) +
  geom_point(aes(y = PM2.5, x = Humidity..., col = factor(Fire.Alarm,labels = c('No','Yes')), shape = factor(Fire.Alarm,labels = c('No','Yes')))) +
  scale_color_manual(values = c("blue2", "orange"))+theme_bw()+
  theme(legend.position="none")+
  labs(x = 'Humidity',y = 'PM2.5',color = "Fire Alarm",shape = 'Fire Alarm')

sp2<-ggplot(data = SDD2_subset ) +
  geom_point(aes(y = NC2.5, x = Humidity..., col = factor(Fire.Alarm,labels = c('No','Yes')), shape = factor(Fire.Alarm,labels = c('No','Yes')))) +
  scale_color_manual(values = c("blue2", "orange"))+theme_bw()+
  theme(legend.title = element_text(size = rel(1)),
        legend.key.size = unit(0.5,"cm"),
        legend.position=c(0.7,0.7),
        legend.background = element_rect(fill = 'white', colour = 'black'))+
  labs(x = 'Humidity',y = 'NC2.5',color = "Fire Alarm",shape = 'Fire Alarm')

sp3<-ggplot(data = SDD2_subset ) +
  geom_point(aes(y = eCO2.ppm., x = Humidity..., col = factor(Fire.Alarm,labels = c('No','Yes')), shape = factor(Fire.Alarm,labels = c('No','Yes')))) +
  scale_color_manual(values = c("blue2", "orange"))+theme_bw()+
  theme(legend.position="none")+
  labs(x = 'Humidity',y = 'CO2',color = "Fire Alarm",shape = 'Fire Alarm')


sp4<-ggplot(data = SDD2_subset ) +
  geom_point(aes(y = Raw.Ethanol, x = Humidity..., col = factor(Fire.Alarm,labels = c('No','Yes')), shape = factor(Fire.Alarm,labels = c('No','Yes')))) +
  scale_color_manual(values = c("blue2", "orange"))+theme_bw()+
  theme(legend.position="none")+
  labs(x = 'Humidity',y = 'Raw ethanol gas')

sp5<- ggarrange(sp1,sp2,sp3,sp4,ncol=2,nrow=2)


# Heatmap for Smoke detection data
SDD_cor <- cor(SDD2,use="everything",method ='pearson')

corrplot(cor(SDD2), method = 'square',type = 'lower',tl.cex = 0.6,
         title='Heatmap for Smoke detection data',mar=c(0, 0, 1, 0),
         tl.col = 'black')


#Best subset
regfit.full <- regsubsets(Fire.Alarm ~ ., data = SDD2, nvmax = 19)
reg.summary <- summary(regfit.full)
reg.summary$which[which.min(reg.summary$bic), ]




#Split the dataset
index <- caret::createDataPartition(
  SDD2$Fire.Alarm, p = 0.7, list = FALSE
)
SDD2_train <- slice(SDD2, index)
SDD2_test <- slice(SDD2, -index)
SDD2_train <- dplyr::select(SDD2_train, -PM1.0,-PM2.5,-CNT)
SDD2_test <- dplyr::select(SDD2_test, -PM1.0,-PM2.5,-CNT)

SDD2_x_train <- dplyr::select(SDD2_train, -Fire.Alarm)
SDD2_y_train <- dplyr::pull(SDD2_train, Fire.Alarm)
SDD2_x_test <- dplyr::select(SDD2_test, -Fire.Alarm)
SDD2_y_test <- dplyr::pull(SDD2_test, Fire.Alarm)

SDD2_train_st <- data.frame(cbind(scale(SDD2_x_train, center = TRUE, scale = TRUE),SDD2_y_train))
colnames(SDD2_train_st)[12] <- 'Fire.Alarm'

SDD2_test_st <- data.frame(cbind(scale(SDD2_x_test, center = TRUE, scale = TRUE),SDD2_y_test))
colnames(SDD2_test_st)[12] <- 'Fire.Alarm'

SDD2_x_train<-data.frame(scale(SDD2_x_train, center = TRUE, scale = TRUE))
SDD2_x_test<-data.frame(scale(SDD2_x_test, center = TRUE, scale = TRUE))


#Random Forest
forest <- randomForest(factor(Fire.Alarm) ~ ., data = SDD2_train_st,importance=TRUE,cutoff=c(1/3,2/3),maxdepth=30)
test_predict <- predict(forest, SDD2_x_test)
compare_test <- table(SDD2_y_test, test_predict, dnn = c('Actual', 'Predicted'))
mr_rf = 1- sum(diag(compare_test)/sum(compare_test))
print(mr_rf)
print(forest$importance)
varimp <- forest$importance
varimpdf <- data.frame(var = row.names(varimp),
                       impor = varimp[,4])
ggplot(varimpdf,aes(x = reorder(var,-impor), y = impor))+
  geom_col(colour = "lightblue",fill = "lightblue")+
  labs(x = "Variables", y = "Importances")

