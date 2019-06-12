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
dif_delta<- function(data = data, alpha = 0.05, delta, cows = 20, amount = 10, y_variable = 2, time_variable = 3, mrl = log(0.04)){
  for (k in 1:length(delta)){
    if (delta[k] <= 0 | delta[k] >= 1){
      break
      print("Each delta value has to between 0 and 1")
    }
    else{
    }
  }
  TTSC_new <- c()
  length_delta <- length(delta)
  TTSC_MRL_first_delta <- c()
  TTSC_MRL_last_delta <- c()
  
  for (j in 1:length(delta)){
    K <- c() #Vector to store the test statistic
    TTSC <- c() #Dataframe to store the TTSC value
    MRL <- data.frame(predictions_subset = mrl) #Dataframe for mrl value
    ncp <- qnorm(1-delta[j])*sqrt(cows) #Noncentral parameter for t-statistic
    K <- (qt(1-alpha,cows-1,ncp))/(sqrt(cows)) #test statistic
    #Tolerance limit for each time point
    for (i in 1:amount){
      subset <- data[seq(i, nrow(data), cows), ]
      pred <- subset$Pred
      mean <- mean(pred)
      sd <- sd(pred)
      TTSC_new[i] <- mean + K*sd
      exp_TTSC_new <- exp(TTSC_new)
    }
    if (j == 1){
      pdf("Plots for each cow for first delta value.pdf")
      for(i in 1:cows){
        subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
        y <- as.matrix(subset[,y_variable])
        time <- as.matrix(subset[,time_variable])
        plot(y ~ time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
        predictions_subset <- TTSC_new
        x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
        points(x_axis, TTSC_new, type = 'l') #Creating line for each cows predicted values
        abline(h = mrl) #Adding line for mrl value
      }
      dev.off()
      model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
      TTSC <- predict(model, newdata = MRL)
      TTSC_MRL_first_delta <- as.numeric(TTSC)
    }
    else if (j == length_delta){
      pdf("Plots for each cow for Last delta value.pdf")
      for(i in 1:cows){
        subset <- data[(1 + (i-1)*(amount)):(amount + (i-1)*(amount)),] #Subsetting for each cow
        y <- as.matrix(subset[,y_variable])
        time <- as.matrix(subset[,time_variable])
        plot(y ~ time, xlab = 'Time', ylab = "Level") #Creating a plot for each cow actual value
        predictions_subset <- TTSC_new
        x_axis <- as.numeric(as.matrix(data[1:amount,time_variable]))
        points(x_axis, TTSC_new, type = 'l') #Creating line for each cows predicted values
        abline(h = mrl) #Adding line for mrl value
      }
      dev.off()
      model <- lm(x_axis ~ predictions_subset) #Finding out when the tolerance upperbounds reach the mrl value
      TTSC <- predict(model, newdata = MRL)
      TTSC_MRL_last_delta <- as.numeric(TTSC)
    }
  }
  TTSC <- list(TTSC_MRL_first_delta, TTSC_MRL_last_delta)
  print(paste('TTSC for alpha value: ', alpha, 'and delta value:',delta[1], 'and mrl:',MRL, sep = ' '))
  print(TTSC_MRL_first_delta)
  print(TTSC_MRL_last_delta)
}

