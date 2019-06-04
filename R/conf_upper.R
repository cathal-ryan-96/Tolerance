#' @title Creates Statistical Upper Bounds to give a measure of TTSC
#'
#' @describe This package creates a upper bound to know when milk is safe for human consumption
#'
#' @param data alpha delta mrl y_variable time_variable
#'
#' @return NULL
#'
#' @examples conf_upper(data = pred_table, alpha = 0.05, mrl = log(0.04))
#'
#' @export conf_upper
conf_upper <- function(data = data, alpha = 0.05, y_variable = 6, time_variable = 3, mrl = log(0.04), cows = 25, amount = 8){
  TTSC <- c()
  conf <- c()
  MRL <- data.frame(confidence_subset = mrl) #Dataframe for mrl value
  t <- sqrt(1/cows)*qt((1-alpha), cows-1)
  data$x_tol <- c()
  data$x_tol <- data$Pred + data$StdErrPred*t
  conf <- as.numeric(data$x_tol)
  pdf("Plots for each cow with confidence interval instead.pdf")
  for(i in 1:cows){
    subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
    subset_y <- as.numeric(as.matrix(subset[,y_variable]))
    subset_time <- as.numeric(as.matrix(subset[,time_variable]))
    plot(subset_y ~ subset_time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
    confidence_subset <- conf[(1 + (i-1)*(amount)):(amount + (i-1)*(amount))]
    x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
    points(x_axis, confidence_subset, type = 'l') #Creating line for each cows predicted values
    abline(h = mrl) #Adding line for mrl value
    model <- lm(x_axis ~ confidence_subset) #Finding out when the tolerance upperbounds reach the mrl value
    TTSC[i] <- predict(model, MRL)
  }
  dev.off()
  print(paste('TTSC for alpha value:',alpha, 'and mrl:',MRL, sep = ' '))
  print(TTSC)
}

