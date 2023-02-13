library('ggplot2')
library('MASS')
library('glmnet')

RRAG <- function(data,lamda,target,intercept){
  if (intercept == FALSE){
  lamda_row <- rep(sqrt(lamda), ncol(data))
  data_ag <- rbind(data,lamda_row)
  data_ag[nrow(data_ag),target]<-0
  Y <- as.matrix(data_ag[,target])
  X <- as.matrix(data_ag[,-which(colnames(data_ag) %in% target)])
  library('MASS')
  beta_naive <- ginv(t(X)%*%X)%*%t(X)%*%Y
  return(beta_naive) }  else {
    Intercept <- rep(1, nrow(data))
    data_i<- cbind(Intercept,data)
    lamda_row <- rep(sqrt(lamda), ncol(data_i))
    data_ag <- rbind(data_i,lamda_row)
    data_ag[nrow(data_ag),target]<-0
    Y <- as.matrix(data_ag[,target])
    X <- as.matrix(data_ag[,-which(colnames(data_ag) %in% target)])
    library('MASS')
    beta_naive <- ginv(t(X)%*%X)%*%t(X)%*%Y
    return(beta_naive)
  }
}
url <- 'https://hastie.su.domains/ElemStatLearn/datasets/prostate.data'
download.file(url,destfile='prostate.data')
psc<-read.table('prostate.data',head=TRUE)

#Running time comparison
t1<-Sys.time()
model1<-RRAG(psc,0.01,'lpsa',TRUE)
t2<-Sys.time()
print(t2-t1)

psc_y <- as.matrix(psc['lpsa'])
psc_x <- data.frame(psc[,-which(colnames(psc) == 'lpsa')])
t1<-Sys.time()
rrfit<- glmnet(psc_x,psc_y,alpha=0,lambda=0.01)
t2<-Sys.time()
print(t2-t1)

#Two-sample T test
rr_coef<- coef(rrfit)@x
t.test(rr_coef,model1)


#Training set error
Intercept <- rep(1, nrow(psc_x))
psc_x_t<- as.matrix(cbind(Intercept,psc_x))
error_rr<- mean((psc_y - psc_x_t%*%rr_coef)^2)
error_my<- mean((psc_y - psc_x_t%*%model1)^2)

abs_error <-abs(model1 - rr_coef)
beta <- 1:10
ggplot()+
  geom_line(aes(x=beta,y=abs_error))+
  geom_point(aes(x=beta,y=abs_error),shape = 'o',size = 2)+
  xlab('The coefficients')+
  ylab('The differences of two methods')
