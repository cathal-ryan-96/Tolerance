#' @title Creates Statistical Upper Bounds to give a measure of TTSC
#'
#' @describe This package creates a upper bound to know when milk is safe for human consumption
#'
#' @param data alpha delta mrl y_variable time_variable
#'
#' @return NULL
#'
#' @examples dif_delta(data = pred_table, alpha = 0.05, delta = c(0.05,0.06), mrl = log(0.04))
#'
#' @export dif_delta
#'
dif_delta<- function(data = data, alpha = 0.05, delta = seq(0.01,0.05,by = 0.01), cows = 20, amount = 10, y_variable = 2, time_variable = 3, mrl = log(0.04)){
  for (k in 1:length(delta)){
    if (delta[k] <= 0 | delta[k] >= 1){
      break
      print("Each delta value has to between 0 and 1")
    }
    else{
    }
  }
  length_delta <- length(delta)
  for (j in 1:length(delta)){
    K <- c() #Vector to store the test statistic
    TTSC <- c() #Dataframe to store the TTSC value
    MRL <- data.frame(predictions_subset = mrl) #Dataframe for mrl value
    ncp <- qnorm(1-delta[j])*sqrt(cows) #Noncentral parameter for t-statistic
    K <- (qt(1-alpha,cows-1,ncp))/(sqrt(cows)) #test statistic
    data$x_tol <- c()
    data$x_tol <- data$Pred + K*data$StdErrPred #Tolerance bound for each point
    data_tolerance_bound <- data$x_tol #Vector for tolerance bound
    pred <- as.numeric(data_tolerance_bound)
    if (j == 1){
      pdf("Plots for each cow for first delta value.pdf")
      for(i in 1:cows){
        subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
        plot(subset[,y_variable] ~ subset[,time_variable], xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
        predictions_subset <- pred[(1 + (i-1)*(amount)):(amount + (i-1)*(amount))]
        x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
        points(x_axis, predictions_subset, type = 'l') #Creating line for each cows predicted values
        abline(h = mrl) #Adding line for mrl value
        model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
        TTSC[i] <- predict(model, MRL)
      }
      dev.off()
      TTSC_MRL_first_delta <- TTSC #TTSC for the first alpha value
    }
    else if (j == length_delta){
      pdf("Plots for each cow for Last delta value.pdf")
      for(i in 1:cows){
        subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
        plot(subset[,y_variable] ~ subset[,time_variable], xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
        predictions_subset <- pred[(1 + (i-1)*(amount)):(amount + (i-1)*(amount))]
        x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
        points(x_axis, predictions_subset, type = 'l') #Creating line for each cows predicted values
        abline(h = mrl) #Adding line for mrl value
        model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
        TTSC[i] <- predict(model, MRL)
      }
      dev.off()
      TTSC_MRL_last_delta<- TTSC #TTSC for the last alpha value
    }
  }
  TTSC <- list(TTSC_MRL_first_delta, TTSC_MRL_last_delta)
  milking_first <- floor(TTSC_MRL_last_delta)
  milking_last <- floor(TTSC_MRL_first_delta)
  different_TTSC<- which(milking_first != milking_last)
  print(paste('The number of cows that have a different milking time when using the endpoints of the delta vector: ', length(different_TTSC), sep ='' ))
  if (length(different_TTSC) == 0){
    print(paste('TTSC for alpha value: ', alpha, 'and delta value:',delta[1], 'and mrl:',MRL, sep = ' '))
    print(TTSC_MRL_first_delta)
  }
}

