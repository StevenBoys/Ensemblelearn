#' Function that implement one weak model step of AdaBoost in regression
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need including x, y and last_est which is the sample weight for this step
#'
#' @return outputs boosting_fit1(fweak, data)
#' @export
#'
#' @examples
#' fweak <- function(x, y, last_est){
#'   lm(y ~ -1 + x, weights = last_est)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5) + rnorm(200, 0, 3)
#' data$last_est <- rep(1/length(data$y), length(data$y))
#' adaboost_fit1(fweak, data)
adaboost_fit1 <- function(fweak, data){
  # Fit the weak model
  coef <- fit_model(fweak, F, data)
  # Calculate the corresponding errors
  errors <- (data$y - data$x %*% coef) * (data$y - data$x %*% coef)
  # Calculate the weights for next step based on the errors
  weights <- log(1 + errors)
  weights <- weights / sum(weights)
  # Construct the trained model based on the coef
  model_train <- function(x){
    x %*% coef
  }
  # Returen the trained function, weights and mse of this weak model for the next step
  list(model_train = model_train, weights = weights, error = mean(errors))
}


#' Function that implement the algorithm of Ensemble learning in AdaBoost
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need including x, y and last_est
#' @param model_num - the number of weak models you want to train and combine
#'
#' @return Adaboost(fweak, data, model_num)
#' @export
#'
#' @examples
#' fweak <- function(x, y, last_est){
#'   lm(y ~ -1 + x, weights = last_est)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5) + rnorm(200, 0, 3)
#' data$last_est <- rep(1/length(data$y), length(data$y))
#' model_num <- 100
#' Adaboost(fweak, data, model_num)
Adaboost <- function(fweak, data, model_num){
  # Initialize multi_est for storing the fitting results of weak models
  model_train <- list()
  length(model_train) <- model_num
  # Initialize errors for storing the mse in the weak models
  errors <- rep(0, model_num)
  # Fit the weak models
  for(i in 1:model_num){
    fit1 <- adaboost_fit1(fweak, data)
    model_train[[i]] <- fit1$model_train
    data$last_est <- fit1$weights
    errors[i] <- fit1$error
  }
  # Calculate the weights of combining the weak models based on the errors
  weights <- log(1 + 1/errors)
  weights <- weights / sum(weights)
  # Get the multiple estimation based on the trained models
  multi_est <- prediction(data$x, model_train, parallel = T)
  # Combine the multiple estimation
  comb_out <- Comb_parallel(multi_est, weights)
  # Return the fitted values on training data, the list of weak models and the weights for combining the weak models
  list(fitted_values = comb_out, model_train = model_train, weights = weights)
}





