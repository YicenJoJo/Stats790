library('ggplot2')
library('mlr')
library('MASS')
N<-500
P<-30
Time_naive <- matrix(,N-1,P-1)
Time_QR <- matrix(,N-1,P-1)
Time_svd <- matrix(,N-1,P-1)
for (n in 2:N){
  for (p in 2:P){
        X <- matrix(rnorm(n*p,sd = 100),n,p)
        Y <- matrix(rnorm(n,sd = 10),n,1)
        t1<-Sys.time()
        beta_naive <- ginv(t(X)%*%X)%*%t(X)%*%Y
        t2<-Sys.time()
        Time_naive[n-1,p-1] <- t2-t1
        
        t1<-Sys.time()
        QR <- qr(t(X)%*%X)
        beta_naive <- ginv(qr.R(QR))%*%t(qr.Q(QR))%*%t(X)%*%Y
        t2<-Sys.time()
        Time_QR[n-1,p-1] <- t2-t1
        
        t1<-Sys.time()
        s <- svd(X)
        D <- diag(s$d)
        beta_naive <- ginv(s$u %*% D %*% t(s$v))%*%Y
        t2<-Sys.time()
        Time_svd[n-1,p-1] <- t2-t1
  }}

n <- 2:N
p <- 2:P
avt_naive_n <- Time_naive[,9]
avt_naive_p <- Time_naive[499,]
avt_QR_n <- Time_QR[,9]
avt_QR_p <- Time_QR[499,]
avt_svd_n <- Time_svd[,9]
avt_svd_p <- Time_svd[499,]

ggplot() + 
  geom_smooth(method="auto",aes(x = log(n), y = log(avt_naive_n),color = "Naive"),show.legend=TRUE)+
  geom_smooth(method="auto",aes(x = log(n), y = log(avt_QR_n),color = "QR"),show.legend=TRUE)+
  geom_smooth(method="auto",aes(x = log(n), y = log(avt_svd_n),color = "SVD"),show.legend=TRUE)+
  guides(color=guide_legend(title = "Methods"))+
  xlab("The log of the number of observations n")+
  ylab('The log of average time')


ggplot() + 
  geom_smooth(aes(x = log(p), y = log(avt_naive_p),color = "Naive"))+
  geom_smooth(aes(x = log(p), y = log(avt_QR_p),color = "QR"))+
  geom_smooth(aes(x = log(p), y = log(avt_svd_p),color = "SVD"))+
  guides(color=guide_legend(title = "Methods"))+
  xlab("The log of the number of parameters p")+
  ylab('The log of average time')

