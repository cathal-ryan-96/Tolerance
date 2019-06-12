#' @title Creates Statistical Upper Bounds to give a measure of TTSC
#'
#' @describe This package creates a upper bound to know when milk is safe for human consumption
#'
#' @param data alpha delta mrl y_variable time_variable
#'
#' @return NULL
#'
#' @examples Nat_tol(data = pred_table, alpha = 0.05, delta = 0.05, mrl = log(0.04))
#'
#' @export Nat_tol
Nat_tol <- function(cows = 20, amount = 10, data = data, alpha = 0.05, delta = 0.05, y_variable = 2, time_variable = 3, mrl = log(0.04)){
  milking_times <- as.matrix(data[1:amount, time_variable])
  milking_times <- as.numeric(milking_times)
  diff_milking <- c()
  TTSC_new <- c()
  for (i in 1:amount){
    if (i == 1){
      diff_milking[i] = milking_times[i]
    }
    else{
      diff_milking[i] = milking_times[i] - milking_times[i-1]
    }
  }
  print(diff_milking)
  TTSC <- c() #Dataframe to store the TTSC value
  MRL <- data.frame(predictions_subset = mrl) #Dataframe for mrl value
  a <- 1 - ((qnorm(1-alpha))^2)/(2*(cows-1))
  b <- (qnorm(1-delta))^2 - ((qnorm(1-alpha))^2)/(cows)
  K <- (qnorm(1-delta) + sqrt((qnorm(1-delta))^2 - a*b))/(a)
  #Tolerance limit for each time point
  for (i in 1:amount){
    subset <- data[seq(i, nrow(data), cows), ]
    pred <- subset$Pred
    mean <- mean(pred)
    sd <- sd(pred)
    TTSC_new[i] <- mean + K*sd
    exp_TTSC_new <- exp(TTSC_new)
  }
  print(TTSC_new)
  print(exp_TTSC_new)
  pdf("Plots for each cow using Natrella's tolerance.pdf")
  for(i in 1:cows){
    subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
    y <- as.matrix(subset[,y_variable])
    time <- as.matrix(subset[,time_variable])
    plot(y ~ time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
    predictions_subset <- TTSC_new
    x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
    points(x_axis, TTSC_new, type = 'l') #Creating line for each cows predicted values
    abline(h = mrl) #Adding line for mrl value
    model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
  }
  dev.off()
  model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
  TTSC <- predict(model, MRL)
  TTSC <- as.numeric(TTSC)
  print(paste('TTSC for alpha value:',alpha, 'and delta value: ', delta, 'is:', sep = ' '))
  print(TTSC)
}