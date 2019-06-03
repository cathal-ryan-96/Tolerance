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
EMA_method <- function(data = data, LOQ = 0.02, alpha = 0.05, delta = 0.05, cows = 25, amount = 8, y_variable = 5, time_variable = 3){
  #Setting values under the LOQ to be at the LOQ and setting the variable z = 1 if this happened
  z <- c()
  MRL <- c()
  TTSC <- c()
  LOQ <- LOQ
  Total_UWP <- c()
  data$y <- log(data[,y_variable])
  data$level <- data[,y_variable]
  Level <- as.numeric(as.matrix(exp(data$y)))
  Level <- sort(unique(Level))
  Last_level <- c()
  First_level <- c()
  for (j in 1:cows){
    subset <- data[(1 + (j-1)*amount):(amount + (j-1)*amount),]
    First_level[j] = as.numeric(as.matrix(subset[1,5]))
    Last_level[j] = as.numeric(as.matrix(subset[8,5]))
  }
  Thresh_mrl_top <- abs(min(-(Last_level)))
  Thresh_mrl_bot <- min(First_level)
  Location_top <- as.numeric(which(Level %in% Thresh_mrl_top))
  Location_bot <- as.numeric(which(Level %in% Thresh_mrl_bot))
  subset_first <- data[seq(1, nrow(data), 8),]
  subset_last <- data[seq(8, nrow(data), 8),]
  for (i in 1:length(Level)){
    if (Level[i] < max(subset_last$Level)){
      i = i + 1
    }
    else{
      Location_bot = i
      break
    }
  }
  Location_top <- as.numeric(which(Level == min(subset_first$Level)))
  Location_bot <- as.numeric(Location_bot)
  Level <- Level[Location_bot:Location_top]
  for (i in 1:length(Level)){
    MRL = Level[i]
    for (k in 1:cows){
      subset <- data[(1 + (k-1)*amount):(amount + (k-1)*amount),]
      for (j in 1:nrow(subset)){
        if(subset[j,5] >= MRL){
          j = j + 1
        }
        else if (subset[j,5] < MRL){
          #print(paste("For cow id: ", k, ",the last milking to be above the MRL is:", sep = " "))
          #print(as.matrix(subset[j-1,3]))
          #print(as.matrix(subset[j-1,5]))
          TTSC[k] <- as.numeric(as.matrix(subset[j-1,3]))
          break
        }
      }
    }
    #print(TTSC)
    TTSC <- as.numeric(TTSC)
    x <- log(TTSC)
    mean <- mean(x)
    sd <- sd(x)
    ncp <- qnorm(1-delta)*sqrt(cows) #Noncentral parameter for t-statistic
    K <- (qt(1-alpha,cows-1,ncp))/(sqrt(cows)) #test statistic
    Tol <- mean + K*sd
    UWP <- exp(Tol)
    Total_UWP[i] <- UWP
  }
  print("The values of the TTSC for each observation that isnt either below the maximum of the first time points or above the minimum of the last time points is:")
  print(Total_UWP)
  print("The amount of observations less than the maximum of the first milkings:")
  print(Location_bot - 1)
  print("The amount of observations greatr than the minimum of the last milkings:")
  print(nrow(data) - Location_top)
}