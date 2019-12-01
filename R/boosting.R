#' Function that calculate the mse of dependent variable and its estimate
#'
#' @param y - the true dependent variable
#' @param y_hat - the estimate of the dependent variable
#'
#' @return outputs boosting_fit1(fweak, data)
#' @export
#'
#' @examples
#' y <- rnorm(100); y_hat <- rep(0, 100)
#' mse(y, y_hat)
mse <- function(y, y_hat){
  sum((y - y_hat) * (y - y_hat))
}

#' Function that calculate the negative gradient of given loss function
#'
#' @param loss - the loss function used
#' @param x - input independent variables x for the training
#' @param y - the true dependent variable
#' @param coef - the estimate of the coefficients in regression
#' @param eps - the small increase used in the calculation of gradient
#'
#' @return outputs boosting_fit1(fweak, data)
#' @export
#'
#' @examples
#' y <- rnorm(100); y_hat <- rep(0, 100)
#' mse(y, y_hat)
nega_gra <- function(loss, x, y, coef, eps = 0.001){
  sapply(coef, function(t){
    coef_new[t] <- coef[t] + eps
    (loss(y, x %*% coef_new)-loss(y, x %*% coef))/eps
  })
}

#' Function that builds weak model on Gradient Boosting for regression task
#'
#' @param x - input independent variables x for the training
#' @param y - input dependent variable y for the traisning
#' @param last_est - the output estimate from the last step
#' @param loss - the loss function used, its default value is the mean of the square error
#' @param eta - the step size we use to update the total estimate each time, its default value is 0.1
#'
#' @return graboo_reg(x ,y)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(4000), 200, 20)
#' beta <- rnorm(5)
#' y <- x[, 1:length(beta)] %*% beta + rnorm(200)
#' last_est <- rep(0, length(y))
#' graboo_reg(x, y, last_est)
graboo_reg <- function(x, y, last_est, loss = mse, eta = 0.1){
  nega_gra <- - (loss(y, last_est + ))
  return(rpart_mod)
}

#' Function that implement one resample of Boosting in regression
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need
#'
#' @return outputs boosting_fit1(fweak, data)
#' @export
#'
#' @examples
#' fweak <- function(x, y){
#'   lm(y ~ x)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5)
#' boosting_fit1(fweak, data)
boosting_fit1 <- function(fweak, data){
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

