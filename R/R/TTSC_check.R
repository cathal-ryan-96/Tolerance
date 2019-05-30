#' @title Creates Statistical Upper Bounds to give a measure of TTSC
#'
#' @describe This package creates a upper bound to know when milk is safe for human consumption
#'
#' @param data alpha delta mrl y_variable time_variable
#'
#' @return NULL
#'
#' @examples TTSC_check(data = pred_table, alpha = 0.05, delta = 0.05, mrl = log(0.04))
#'
#' @export TTSC_check
#source("tol_upper.R")
#source("dif_alpha.R")
#source("dif_delta.R")
#source("conf_upper.R")
#source("pred_upper.R")
#source("Nat_tol.R")
TTSC_check <- function(cows = 20, amount = 10, data,
                       y_variable = 2, time_variable = 3,
                       mrl = log(0.04), alpha = 0.05, delta = 0.05, ConfidenceAswell = FALSE, PredAswell = FALSE, Natrella_tolerance_instead = FALSE){
  if (!"Time" %in% colnames(data)){
    Time <- c(as.matrix(data[,time_variable]))
    data$Time <- Time
  }
  if (!"level_log" %in% colnames(data)){
    level_log <- c(as.matrix(data[,y_variable]))
    data$level_log <- level_log
  }
  if (length(delta) > 1){
    dif_delta(cows = cows,amount = amount,data = data, y_variable = y_variable,
                     time_variable = time_variable, alpha = alpha, delta = delta, mrl = mrl)
  }
  else if (length(alpha) > 1 ){
    dif_alpha(cows = cows,amount = amount,data = data, y_variable = y_variable,
                    time_variable = time_variable, alpha = alpha, delta = delta, mrl = mrl)
  }
  else if (Natrella_tolerance_instead == TRUE){
    Nat_tol(cows = cows,amount = amount,data = data, y_variable = y_variable,
                 time_variable = time_variable, alpha = alpha, delta = delta, mrl = mrl)
  }
  else{
    tol_upper(cows = cows,amount = amount,data = data, y_variable = y_variable,
                      time_variable = time_variable, alpha = alpha, delta = delta, mrl = mrl)
  }
  if (PredAswell == TRUE){
    pred_upper(data = data, y_variable = y_variable, time_variable = time_variable, alpha = alpha
              , mrl = mrl , cows = cows, amount = amount)
  }
  if (ConfidenceAswell == TRUE){
    conf_upper(data = data, y_variable = y_variable, time_variable = time_variable, alpha = alpha
                    , mrl = mrl , cows = cows, amount = amount)
  }
}

