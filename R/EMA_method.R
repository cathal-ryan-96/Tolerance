#' @title Creates Statistical tolerance upper bound according to EMA paper
#'
#' @describe Caculates TTSC for the entire dataset in one grouping according to EMA guidelines
#'
#' @param data alpha delta cows amount data LOQ y_variable time_variable
#'
#' @return NULL
#'
#' @examples EMA_method(data = data, LOQ = 0.02, alpha = 0.05, delta = 0.05, cows = 25, amount = 8, y_variable = 5, time_variable = 3)
#'
#' @export EMA_method
#Residue withdrawel according to EMA method
#Cant be used on all datasets as its more specific than the general tolerance interval function
EMA_method <- function(data = data, LOQ = 0.02, alpha = 0.05, 
                       delta = 0.05, cows = 25, amount = 8, 
                       y_variable = 5, time_variable = 3, mrl){
  #data = pred_anti
  #LOQ = 0.04
  #alpha = 0.05
  #delta = 0.05 
  #cows = 20 
  #amount = 5 
  #y_variable = 3
  #time_variable = 2
  #mrl = 0.1
  par(mfrow = c(1,2))
  WP <- c()
  MRL_total <- c()
  ncp <- qnorm(1-delta)*sqrt(cows) #Noncentral parameter for t-statistic
  K <- (qt(1-alpha,cows-1,ncp))/(sqrt(cows)) #test statistic
  #Skippng monotinic regression
  data$LOQ <- (as.numeric(data[,y_variable] <= LOQ)) #Setting up LOQ variable
  data$graphLOQ <- 0.5*data$LOQ
  
  #Getting all the level points
  level <- unique(data[,y_variable])
  MRL_total <- as.numeric(as.matrix(rbind(level, mrl)))
  MRL_total <- sort(MRL_total)
  
  TTSC <- c()
  log_TTSC <- c()
  tolerance_bound <- c()
  time <- rep(c(1:amount), times = cows)
  data$time_EMA <- time
  for (i in 1:length(MRL_total)){
    for (j in 1:cows){
      subset_cow <- data[(1 + amount*(j-1)):(amount+amount*(j-1)),]
      subset_cow$less <- (as.numeric(subset_cow[,y_variable] <= MRL_total[i]))
      subset_lower <- subset(subset_cow, subset_cow$less == 1)
      TTSC[j] <- as.numeric(subset_lower[1,time_variable])
      log_TTSC[j] <- log(TTSC[j])
    }
    mean_TTSC <- mean(log_TTSC)
    sd_TTSC <- sd(log_TTSC)
    tolerance_bound[i] <- mean_TTSC + sd_TTSC*K
  }
  tolerance_bound <- tolerance_bound[!is.na(tolerance_bound)]
  #print(tolerance_bound)
  if (length(tolerance_bound) != length(MRL_total)){
    MRL_total = MRL_total[(length(MRL_total) - 
                             length(tolerance_bound) + 1):length(MRL_total)]
  }
  iso <- isoreg(-(tolerance_bound) ~ MRL_total)
  tolerance_bound <- -iso$yf
  plot(exp(tolerance_bound), MRL_total)
  exp_tol <- exp(tolerance_bound)
  for (i in 1:length(MRL_total)){
    if (MRL_total[i] == mrl){
      WP_actual = exp_tol[i]
    }
  }
  Exp_Tolerance <- as.data.frame(cbind(exp_tol, MRL_total))
  #Making the withdrawal period go to the next milking time
  milk <- as.numeric(as.matrix(data[1:amount,time_variable]))
  for (i in 1:length(exp_tol)){
    for (j in 1:length(milk)){
      if (exp_tol[i] < milk[j]){
        WP[i] = milk[j]
        break
      }
      else{
        j = j + 1
      }
    }
  }
  WP <- WP[!is.na(WP)]
  if ( length(WP) != length(MRL_total)){
    MRL_total = MRL_total[(length(MRL_total) - length(WP) + 1):length(MRL_total)]
  }
  plot(WP, MRL_total)
  print(WP)
  print(WP_actual)
}
