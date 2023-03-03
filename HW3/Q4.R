library(mgcv)
library(microbenchmark)
two_D_generator<- function (n,p){
n<-250
x=list()
y=list()
for (i  in 1:n) {
x[i]<-runif(1)
z<-runif(1)
if (z>0.5) {
y[i]<-(1/4-(x[[i]]-1/2)^p)^(1/p)+1/2} + rnorm(1,sd=0.01) 
else{
  y[i]<- -(1/4-(x[[i]]-1/2)^p)^(1/p)+1/2+ rnorm(1,sd=0.01)
  
}} 
output <- list(a = x, b = y)
return(output)
}
x <- unlist(two_D_generator(300,2)$a)
y <- unlist(two_D_generator(300,2)$b)
microbenchmark(gam(y~te(x, y, k = c(5,5), bs = "gp"),method = "GCV.Cp"),times=250)
microbenchmark(gam(y~te(x, y, k = c(5,5), bs = "gp"),method = "REML"),times = 250)

bias_gcv<-list()
variance_gcv<-list()
mse_gcv<-list()

bias_REML<-list()
variance_REML<-list()
mse_REML<-list()
for (i in 1:250) {
  x <- unlist(two_D_generator(300,2)$a)
  y <- unlist(two_D_generator(300,2)$b)
GCV <- gam(y~te(x, y, k = c(5,5), bs = "gp"),method = "GCV.Cp")
bias_gcv[i]<- sum(GCV$residuals)
variance_gcv[i] <- var(y-GCV$residuals)
mse_gcv[i]<-variance/GCV$df.residual

REML<- gam(y~te(x, y, k = c(5,5), bs = "gp"),method = "REML")

bias_REML[i]<- sum(REML$residuals)
variance_REML[i] <- var(y-REML$residuals)
mse_REML[i]<-variance/REML$df.residual
}
REML$fitted.values
mean(unlist(bias_gcv))
mean(unlist(variance_gcv))
mean(unlist(mse_gcv))

mean(unlist(bias_REML))
mean(unlist(variance_REML))
mean(unlist(mse_REML))