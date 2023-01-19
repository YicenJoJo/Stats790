library("mlr")
library("ggplot2")
library('ElemStatLearn')

#The ElemStatLearn R package contains the dataset used in the book to create figures.
#Loaded the mixture.example data to create figure2.1 of the ESL.

d <- mixture.example
df <- data.frame(x1 = d$x[,1], x2 = d$x[,2], y = d$y)
linear_task<- makeRegrTask(data=df,target='y')
linear_learner <-makeLearner('regr.lm')
linear_model <- train(linear_learner,linear_task)

grid = expand.grid(x1 = seq(-5, 5, .1), x2 = seq(-5, 5, .05))
prediciton<-predict(linear_model , newdata = grid)$data$response
grid["prediction"] = factor(as.numeric(prediciton > 0.5))

coe = coefficients(getLearnerModel(linear_model))

#Decision boundary which is determined by xT ˆβ = 0.5,
#which is β0 + β1x1 +β2x2 = 0.5
#Then we have x2 = (0.5-β1x1-β0)/β2
db = function(x1, coef = coe) {
  1/(coef[3]) * (0.5 - coef[1] - coef[2] * x1)
}

#Making plot and try to reproduce the similar plot as ESL figure2.1
ggplot() + 
  geom_point(aes(x = grid$x1, y = grid$x2, col = grid$prediction), shape = 20, size = .01, alpha = .5, show.legend = FALSE) +
  geom_point(aes(x = df$x1, y = df$x2, col = factor(df$y)), shape = "o", size = 5, show.legend = FALSE) +
  geom_line(aes(x = grid$x1, y = db(grid$x1))) +
  scale_colour_manual(values = c("deepskyblue", "orange")) +
  theme_void()
