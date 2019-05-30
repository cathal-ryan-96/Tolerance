#' @title Creates Statistical Upper Bounds to give a measure of TTSC
#'
#' @describe This package creates a upper bound to know when milk is safe for human consumption
#'
#' @param data alpha delta mrl y_variable time_variable
#'
#' @return NULL
#'
#' @examples dif_alpha(data = pred_table, alpha = c(0.05,0.06), delta = 0.05, mrl = log(0.04))
#'
#' @export dif_alpha
dif_alpha <- function(data = data, alpha = seq(0.01,0.05, by = 0.01), cows = 20, amount = 10, delta = 0.05, y_variable = 2, time_variable = 3, mrl = log(0.04)){
  for (k in 1:length(alpha)){
    if (alpha[k] <= 0 | alpha[k] >= 1){
      break
      print("Each alpha value has to between 0 and 1")
    }
    else{
    }
  }
  length_alpha <- length(alpha)
  for (j in 1:length(alpha)){
    K <- c() #Vector to store the test statistic
    TTSC <- c() #Dataframe to store the TTSC value
    MRL <- data.frame(predictions_subset = mrl) #Dataframe for mrl value
    ncp <- qnorm(1-delta)*sqrt(cows) #Noncentral parameter for t-statistic
    K <- (qt(1-alpha[j],cows-1,ncp))/(sqrt(cows)) #test statistic
    data$x_tol <- c()
    data$x_tol <- data$Pred + K*data$StdErrPred #Tolerance bound for each point
    data_tolerance_bound <- data$x_tol #Vector for tolerance bound
    pred <- as.numeric(data_tolerance_bound)
    if (j == 1){
      pdf("Plots for each cow for first alpha value.pdf")
      for(i in 1:cows){
        subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
        plot(subset$level_log ~ subset$Time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
        predictions_subset <- pred[(1 + (i-1)*(amount)):(amount + (i-1)*(amount))]
        x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
        points(x_axis, predictions_subset, type = 'l') #Creating line for each cows predicted values
        abline(h = mrl) #Adding line for mrl value
        model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
        TTSC[i] <- predict(model, MRL)
      }
      dev.off()
      TTSC_MRL_first_alpha <- c()
      TTSC_MRL_first_alpha <- TTSC #TTSC for the first alpha value
    }
    else if (j == length_alpha){
      pdf("Plots for each cow for Last alpha value.pdf")
      for(i in 1:cows){
        subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
        subset_y <- as.numeric(as.matrix(subset[y_variable]))
        subset_time <- as.numeric(as.matrix(subset[time_variable]))
        plot(subset_y ~ subset_time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
        predictions_subset <- pred[(1 + (i-1)*(amount)):(amount + (i-1)*(amount))]
        x_axis <- c(1:amount)
        points(x_axis, predictions_subset, type = 'l') #Creating line for each cows predicted values
        abline(h = mrl) #Adding line for mrl value
        model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
        TTSC[i] <- predict(model, MRL)
      }
      dev.off()
      TTSC_MRL_last_alpha <- TTSC #TTSC for the last alpha value
    }
  }
  TTSC <- list(TTSC_MRL_first_alpha, TTSC_MRL_last_alpha)
  milking_first <- floor(TTSC_MRL_last_alpha)
  milking_last <- floor(TTSC_MRL_first_alpha)
  different_TTSC<- which(milking_first != milking_last)
  print(paste('The number of cows that have a different milking time when using the endpoints of the alpha vector: ', length(different_TTSC), sep ='' ))
  if (length(different_TTSC) == 0){
    print(paste('TTSC for alpha value:',alpha[1],'and delta value: ', delta, 'and mrl:',MRL, sep = ' '))
    print(TTSC_MRL_first_alpha)
  }
}

