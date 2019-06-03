#' @title Simulated version with random times for milkings
#'
#' @describe Creates simulated version to show effect of using milking times not equal to 1
#' 
#' @param NULL
#'
#' @return NULL
#'
#' @examples NULL
#'
#' @export RandomIntervals
#Simulated data like antibiotic dataset to see if its the same effect if the last two timepoints are all censored
set.seed(1)
#Dataset with milking times between 1 and 10
N <- 20
M <- 10
time <- c(1,5,2,6,4,7,3,4,3,9)
time_new <- c(1,6,8,14,18,25,28,32,35,44)
ID<-matrix(1:N,M,N,byrow=TRUE)
alpha<-rnorm(N,0,1)
Alpha<-matrix(alpha,M,N,byrow=TRUE)
Time<-matrix(time_new,M,N,byrow=FALSE)
error<-rnorm(N*M,0,1/10)
y<-exp(Alpha-Time/10+error)
matplot(t(y),type="l",log="y")


ynew<-c(log(y))
timenew<-c(Time)
idnew<-c(ID)

dat_random<-data.frame(ynew,idnew,timenew)
#write.csv(dat_random, "random_intervals.csv")
