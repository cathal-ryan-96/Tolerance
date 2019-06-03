#' @title Simulated version with 100 milkings
#'
#' @describe Creates simulated version with 100 milkings
#' @param NULL
#'
#' @return NULL
#'
#' @examples NULL
#'
#' @export TimeSimulated100
#Simulated data in which you have 100 milkings
N<-20
M<-100

time<-seq(1,199,by = 2)

set.seed(1)
ID<-matrix(1:N,M,N,byrow=TRUE)
alpha<-rnorm(N,0,1)
Alpha<-matrix(alpha,M,N,byrow=TRUE)
Time<-matrix(time,M,N,byrow=FALSE)
error<-rnorm(N*M,0,0.01)
y<-exp(Alpha-Time/100+error)
matplot(t(y),type="l",log="y")

ynew<-c(log(y))
timenew<-c(Time)
idnew<-c(ID)
timenew2<-c(Time)

dat<-data.frame(ynew,timenew,idnew)
#write.csv(dat, "100milkings.csv")
