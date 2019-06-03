#' @title Simulated version of the antibiotic dataset
#'
#' @describe Creates simulated version of antibiotic dataset
#' @param NULL
#'
#' @return NULL
#'
#' @examples NULL
#'
#' @export Simulated-anti
#Simulated data like antibiotic dataset to see if its the same effect if the last two timepoints are all censored
library("dplyr")
N<-20
M<-5

time<-c(24,33,48,57,72)
time_1 <- c(24)
time_2 <- c(33)
time_3 <- c(48)
time_4 <- c(57)

set.seed(1)
ID<-matrix(1:N,M,N,byrow=TRUE)
alpha<-rnorm(N,0,1)
Alpha_total<-matrix(alpha,M,N,byrow=TRUE)
Alpha_individual <- matrix(alpha,1,N, byrow = TRUE)
Time<-matrix(time,M,N,byrow=FALSE)
error_1 <- rnorm(N,0,0.1)
error_2 <- rnorm(N,0,0.1)
error_3 <- rnorm(N,0,0.1)
error_4 <- rnorm(N,0,0.1)
y_time_1 <- exp((Alpha_individual)*(1)-time_1*(.1)+error_1*(.5))

for ( i in 1:length(y_time_1)){
  if (y_time_1[i] <= exp(-3)){
    y_time_1[i] = y_time_1[i]*5
  }
}
y_time_2 <- exp((Alpha_individual)*(.7)-time_2*(.1)+error_2*(.5))

for ( i in 1:length(y_time_2)){
  if (y_time_2[i] <= exp(-3.5)){
    y_time_2[i] = y_time_1[i]*.5
  }
}

y_time_3 <- exp((Alpha_individual)*(.6)-time_3*(.1)+error_3*(.5))

quantile_y3_new <- quantile(y_time_3)
for (i in 1:length(y_time_3)){
  if (y_time_3[i] >= quantile_y3_new[2] & y_time_3[i] < quantile_y3_new[3]){
    y_time_3[i] = exp(-6)
  }
  else if (y_time_3[i] >= quantile_y3_new[3] & y_time_3[i] < quantile_y3_new[4]){
    y_time_3[i] = exp(-5.5)
  }
  else if (y_time_3[i] >= quantile_y3_new[4] & y_time_3[i] < quantile_y3_new[5]){
    y_time_3[i] = exp(-5.2)
  }
  else{
    y_time_3[i] = exp(-4.9)
  }
}


y_time_4 <- exp((Alpha_individual)*(.3)-time_4*(.1)+error_4*(.5))

quantile_y4 <- quantile(y_time_4)
for (i in 1:length(y_time_4)){
  if (y_time_4[i] < quantile_y4[2]){
    y_time_4[i] = exp(-6)
  }
  else if(y_time_4[i] > quantile_y4[2] & y_time_4[i] < quantile_y4[3]){
    y_time_4[i] = quantile_y4[3]
  }
  else if(y_time_4[i] > quantile_y4[2] & y_time_4[i] < quantile_y4[3]){
    y_time_4[i] = quantile_y4[4]
  }
  else{
    y_time_4[i] = quantile_y4[5]
  }
}

y_time_5 <- matrix(data = rep.int(exp(-6),20), nrow = 1, ncol = 20)
#y <- merge(y_time_4,y_time_5, all = TRUE)
#y <- merge(y,y_time_3, all = TRUE)
#y <- merge(y,y_time_2, all = TRUE)
#y <- merge(y,y_time_1, all = TRUE)
#y <- arrange(y, -row_number())
y <- rbind(y_time_1,y_time_2,y_time_3,y_time_4,y_time_5)

#y<-exp(Alpha-Time/10+error)
y <- as.matrix(y)
ynew<-c(log(y))
timenew<-c(Time)
idnew<-c(ID)
timenew2<-c(Time)
dat<-data.frame(ynew,timenew,idnew)

for(i in 1:nrow(dat)){
  if (dat[i,1] <=  -6){
    #print(paste(i))
    dat[i,1] = -6
  }
}
#plot(dat$ynew ~dat$timenew)
#plot(pred_anti$level_log ~ pred_anti$Time)

#write.csv(dat, "antisim.csv")
