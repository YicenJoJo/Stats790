library('ggplot2')
library(splines)

url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data"
dd <- read.csv(url, row.names = 1)
a<-quantile(dd$tobacco, seq(0, 1, length = 8))

n <- c(a[[1]],a[[3]]-a[[1]],a[[4]],a[[5]],a[[6]],a[[7]],a[[8]])
n1<- c(a[[3]]-a[[1]],a[[4]],a[[5]],a[[6]],a[[7]])
#n2<- c(a[[1]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]],a[[8]])

tps_basis <- function(x,knots.b) {
  if (!require("Matrix")) stop("need Matrix package")
  knots <- knots.b
  matrix <- matrix(nrow = length(x),ncol=length(knots))
  ## should probably use seq() instead of `:`
  ## dim: n x (df-2)
  j=1
  for (k in knots){
    for (i in 1:length(x)){
      if (x[i] > k) {matrix[i,j] <- (x[i]-k)^3 }
      else {matrix[i,j] <- 0}
    }
    j = j+1}
  ## dim: n x df
  S <- cbind(x, x^2, x^3,matrix)
  return(S)
}


mc<-function(grid,fit_tps){
  to_matrix <- matrix(nrow = length(grid),ncol=9)
  to_matrix[,1] <- 1
  to_matrix[,2:9] <- tps_basis(grid,n1)
  s<-apply(t(t(to_matrix)*fit_tps$coefficients),1,sum)
  return(s)}

mc2<-function(grid,fit_tps){
  to_matrix <- matrix(nrow = length(grid),ncol=7)
  to_matrix[,1] <- 1
  to_matrix[,2:7] <- ns(grid,knots=n1)
  s<-apply(t(t(to_matrix)*fit_tps$coefficients),1,sum)
  return(s)}

mc3<-function(grid,fit_tps){
  to_matrix <- matrix(nrow = length(grid),ncol=9)
  to_matrix[,1] <- 1
  to_matrix[,2:9] <- bs(grid,knots = n1)
  s<-apply(t(t(to_matrix)*fit_tps$coefficients),1,sum)
  return(s)}


to_grid <- seq(n[1],n[7],length.out=60)

fit_ns <- glm(chd~ns(tobacco,knots = n1),
           data=dd,
           family = binomial(link = "logit")) 
ypredict_ns<- mc2(to_grid,fit_ns)
sd_ns<- sd(ypredict_ns)


fit_bs <- glm(chd~bs(tobacco,knots = n1),
           data=dd,
           family = binomial(link = "logit")) 
ypredict_bs<- mc3(to_grid,fit_bs)
sd_bs<- sd(ypredict_bs)




t_d<- tps_basis(dd$tobacco,n1)
fit_tps1 <- glm(chd~t_d,data=dd,
                family = binomial(link = "logit"))

ypredict_1<-mc(to_grid,fit_tps1)
sd1<- sd(ypredict_1)

ggplot()+geom_ribbon(aes(x=to_grid,ymin = ypredict_1-sd1, ymax = ypredict_1+sd1),alpha = 0.3) +
  geom_line(aes(x=to_grid,y=ypredict_1))+
  xlab("Cumulative tobacco")+
  ylab("The log-odds of coronary heart disease")


ggplot()+geom_ribbon(aes(x=to_grid,ymin = ypredict_ns-sd_ns, ymax = ypredict_ns+sd_ns),alpha = 0.3) +
  geom_line(aes(x=to_grid,y=ypredict_ns))+
  xlab("Cumulative tobacco")+
  ylab("The log-odds of coronary heart disease")

ggplot()+geom_ribbon(aes(x=to_grid,ymin = ypredict_bs-sd_bs, ymax = ypredict_bs+sd_bs),alpha = 0.3) +
  geom_line(aes(x=to_grid,y=ypredict_bs))+
  xlab("Cumulative tobacco")+
  ylab("The log-odds of coronary heart disease")




# fit_tps1 <- glm(chd~poly(tobacco,3,raw=TRUE),data=dd,subset = (tobacco<=n[2]),
#                 family = binomial(link = "logit"))
# fit_tps2 <- glm(chd~poly(tobacco,3,raw=TRUE),data=dd,subset = (n[2]<tobacco & tobacco<=n[3]),
#                 family = binomial(link = "logit"))
# fit_tps3 <- glm(chd~poly(tobacco,3,raw=TRUE),data=dd,subset = (n[3]<tobacco & tobacco<=n[4]),
#                 family = binomial(link = "logit"))
# fit_tps4 <- glm(chd~poly(tobacco,3,raw=TRUE),data=dd,subset = (n[4]<tobacco & tobacco<=n[5]),
#                 family = binomial(link = "logit"))
# fit_tps5 <- glm(chd~poly(tobacco,3,raw=TRUE),data=dd,subset = (n[5]<tobacco & tobacco<=n[6]),
#                 family = binomial(link = "logit"))
# fit_tps6 <- glm(chd~poly(tobacco,3,raw=TRUE),data=dd,subset = (n[6]<tobacco & tobacco<=n[7]),
#                 family = binomial(link = "logit"))
# 
# to_grid1 <- seq(n[1],n[2],length.out=10)
# to_grid2 <- seq(n[2],n[3],length.out=10)
# to_grid3 <- seq(n[3],n[4],length.out=10)
# to_grid4 <- seq(n[4],n[5],length.out=10)
# to_grid5 <- seq(n[5],n[6],length.out=10)
# to_grid6 <- seq(n[6],n[7],length.out=10)
# 
# 
# 
# ypredict_1<-mc(to_grid1,fit_tps1)
# sd1<- sd(ypredict_1)
# 
# ypredict_2<-mc(to_grid2,fit_tps2)
# sd2<- sd(ypredict_2)
# 
# ypredict_3<-mc(to_grid3,fit_tps3)
# sd3<- sd(ypredict_3)
# 
# ypredict_4<-mc(to_grid4,fit_tps4)
# sd4<- sd(ypredict_4)
# 
# ypredict_5<-mc(to_grid5,fit_tps5)
# sd5<- sd(ypredict_5)
# 
# ypredict_6<-mc(to_grid6,fit_tps6)
# sd6<- sd(ypredict_6)
# 
# ggplot()+geom_ribbon(aes(x=to_grid1,ymin = ypredict_1-sd1, ymax = ypredict_1+sd1),alpha = 0.3) +
#   geom_line(aes(x=to_grid1,y=ypredict_1))+
#   geom_ribbon(aes(x=to_grid2,ymin = ypredict_2-sd2, ymax = ypredict_2+sd2),alpha = 0.3) +
#   geom_line(aes(x=to_grid2,y=ypredict_2))+
#   geom_ribbon(aes(x=to_grid3,ymin = ypredict_3-sd3, ymax = ypredict_3+sd3),alpha = 0.3) +
#   geom_line(aes(x=to_grid3,y=ypredict_3))+
#   geom_ribbon(aes(x=to_grid4,ymin = ypredict_4-sd4, ymax = ypredict_4+sd4),alpha = 0.3) +
#   geom_line(aes(x=to_grid4,y=ypredict_4))+
#   geom_ribbon(aes(x=to_grid5,ymin = ypredict_5-sd5, ymax = ypredict_5+sd5),alpha = 0.3) +
#   geom_line(aes(x=to_grid5,y=ypredict_5))+
#   geom_ribbon(aes(x=to_grid6,ymin = ypredict_6-sd6, ymax = ypredict_6+sd6),alpha = 0.3)+
#   geom_line(aes(x=to_grid6,y=ypredict_6))+
#   xlab("Cumulative tobacco")+
#   ylab("The log-odds of coronary heart disease")
