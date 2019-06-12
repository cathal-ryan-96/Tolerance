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
dif_alpha<- function(data = data, alpha, delta, cows = 20, amount = 10, y_variable = 2, time_variable = 3, mrl = log(0.04)){
  for (k in 1:length(alpha)){
    if (alpha[k] <= 0 | alpha[k] >= 1){
      break
      print("Each alpha value has to between 0 and 1")
    }
    else{
    }
  }
  TTSC_new <- c()
  length_alpha <- length(alpha)
  TTSC_MRL_first_alpha <- c()
  TTSC_MRL_last_alpha <- c()
  
  for (j in 1:length(alpha)){
    K <- c() #Vector to store the test statistic
    TTSC <- c() #Dataframe to store the TTSC value
    MRL <- data.frame(predictions_subset = mrl) #Dataframe for mrl value
    ncp <- qnorm(1-delta)*sqrt(cows) #Noncentral parameter for t-statistic
    K <- (qt(1-alpha[j],cows-1,ncp))/(sqrt(cows)) #test statistic
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
      TTSC_MRL_first_alpha <- as.numeric(TTSC)
    }
    else if (j == length_alpha){
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
      TTSC_MRL_last_alpha <- as.numeric(TTSC)
    }
  }
  print(paste('TTSC for alpha value: ', alpha[1], 'and delta value:',delta, 'and mrl:',MRL, sep = ' '))
  print(TTSC_MRL_first_alpha)
  print(TTSC_MRL_last_alpha)
}

