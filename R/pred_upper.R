#' @title Creates Statistical Upper Bounds to give a measure of TTSC
#'
#' @describe This package creates a upper bound to know when milk is safe for human consumption
#'
#' @param data alpha delta mrl y_variable time_variable
#'
#' @return NULL
#'
#' @examples pred_upper(data = pred_table, alpha = 0.05, mrl = log(0.04))
#'
#' @export pred_upper
pred_upper <- function(data = data, alpha = 0.05, y_variable = 6,
                       time_variable = 3, mrl = log(0.04),
                       cows = 25, amount = 8){
  TTSC <- c()
  conf <- c()
  MRL <- data.frame(predictions_subset = mrl) #Dataframe for mrl value
  t <- sqrt(1+ 1/(cows))*qt((1-alpha), cows-1)
  TTSC_new <- c()
  #Tolerance limit for each time point
  for (i in 1:amount){
    subset <- data[seq(i, nrow(data), amount), ]
    pred <- subset$Pred
    mean <- mean(pred)
    sd <- sd(pred)
    TTSC_new[i] <- mean + t*sd
    exp_TTSC_new <- exp(TTSC_new)
  }
  #print(TTSC_new)
  #print(exp_TTSC_new)
  pdf("Plots for each cow using Prediction Intervals instead.pdf")
  for(i in 1:cows){
    subset <- data[(1 + (1-1)*(amount)):(amount + (1-1)*(amount)),] #Subsetting for each cow
    y <- as.matrix(subset[,y_variable])
    time <- as.matrix(subset[,time_variable])
    plot(y ~ time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
    predictions_subset <- (TTSC_new)
    x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
    points(x_axis, TTSC_new, type = 'l') #Creating line for each cows predicted values
    abline(h = mrl) #Adding line for mrl value
    model <- lm(x_axis ~ predictions_subset) #Finding out when the predicted upperbounds reach the mrl value
  }
  dev.off()
  model <- lm(x_axis ~ predictions_subset) #Finding out when the predicted upperbounds reach the mrl value
  TTSC <- predict(model, MRL)
  TTSC <- as.numeric(TTSC)
  print(paste('TTSC for alpha value:',alpha, 'is:', sep = ' '))
  print(TTSC)
}
