library(recipes)
url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data"
dd <- read.csv(url, row.names = 1)
a<-quantile(dd$tobacco, seq(0, 1, length = 8))

n <- c(a[[1]],a[[3]]-a[[1]],a[[4]],a[[5]],a[[6]],a[[7]],a[[8]])
n1<- c(a[[3]]-a[[1]],a[[4]],a[[5]],a[[6]],a[[7]])

tps_basis <- function(x,knots.b,natural) {
  if (!require("Matrix")) stop("need Matrix package")
  if (natural == FALSE) {
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
  return(S) }
  if ( natural == TRUE) {
    knots <- knots.b
    matrix <- matrix(nrow = length(x),ncol=length(knots)-2)
    ## should probably use seq() instead of `:`
    ## dim: n x (df-2)
    j=1
    for (k in knots[1:(length(knots)-2)]) {
      for (i in 1:length(x)){
        matrix[i,j] <- (max(0,(x[i]-k)^3)-max(0,(x[i]-knots[length(knots)])^3))
        - (knots[length(knots)]-k)*
          (max(0,(x[i]-knots[length(knots)-1])^3)-max(0,(x[i]-knots[length(knots)])^3))/
          (knots[length(knots)]-knots[length(knots-1)])
      }
      j = j+1}
    ## dim: n x df
    S <- cbind(x,matrix)
    return(S)
  }
}
xvec <- seq(0, 30, length = 101)

matplot(scale(tps_basis(xvec,n1,TRUE)),type = "l",xlab="",ylab="",main = "Natural bases")
matplot(scale(tps_basis(xvec,n1,FALSE)),type = "l",xlab="",ylab="",main = "Regular bases")
par(mfrow = c(1,2), las = 1, bty = "l")