#' Function that implement one resample of Bagging in regression
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need
#'
#' @return outputs bagging_fit1(fweak, data)
#' @export
#'
#' @examples
#' fweak <- function(x, y){
#'   lm(y ~ x)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5)
#' bagging_fit1(fweak, data)
bagging_fit1 <- function(fweak, data){
  # Get the resample index
  index_resample <- sample(1:length(data$y), length(data$y), replace = T)
  # Get the part of data resampled
  data$y <- data$y[index_resample]
  data$x <- data$x[index_resample, ]
  # Fit the weak model based on the resampled data
  coef <- fit_model(fweak, T, data)
  # Construct the trained model based on the coef
  model_train <- function(x){
    x %*% coef
  }
  # Returen the trained function
  model_train
}


#' Function that implement the algorithm of Ensemble learning in Bagging
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need
#' @param model_num - the number of weak models you want to train and combine
#'
#' @return bagging(fweak, data, model_num)
#' @export
#'
#' @examples
#' fweak <- function(x, y){
#'   lm(y ~ -1 + x)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5)
#' model_num <- 100
#' Bagging(fweak, data, model_num)
Bagging <- function(fweak, data, model_num){
  # Initialize multi_est for storing the fitting results of weak models
  model_train <- list()
  length(model_train) <- model_num
  # Fit the weak models
  for(i in 1:model_num){
    model_train[[i]] <- bagging_fit1(fweak = fweak, data = data)
  }
  # Get the multiple estimation based on the trained models
  multi_est <- prediction(data$x, model_train, parallel = T)
  # Combine the multiple estimation
  comb_out <- Comb_parallel(multi_est, rep(1, model_num))
  # Return the fitted values on training data and the list of weak models
  list(fitted_values = comb_out, model_train = model_train)
}
