#' Function that calculate the mse of dependent variable and its estimate
#'
#' @param y - the true dependent variable
#' @param y_hat - the estimate of the dependent variable
#'
#' @return outputs mse(y, y_hat)
#' @export
#'
#' @examples
#' y <- rnorm(100); y_hat <- rep(0, 100)
#' mse(y, y_hat)
mse <- function(y, y_hat){
  # Check the compatibility of y and yhat
  if(length(y) != length(y_hat)){
    stop("The length of y should be the same with that of yhat.")
  }
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
#' @return outputs nega_gra(loss, x, y, coef)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(4000), 200, 20)
#' beta <- rnorm(5)
#' y <- x[, 1:length(beta)] %*% beta + rnorm(200)
#' coef <- lm(y ~ -1 + x)$coefficients
#' loss <- mse
#' nega_gra(loss, x, y, coef)
nega_gra <- function(loss, x, y, coef, eps = 0.001){
  - sapply(1:length(y), function(t){
    y_hat <- x %*% coef; y_hat_new <- y_hat
    y_hat_new[t] <- y_hat_new[t] + eps
    (loss(y, y_hat_new)-loss(y, y_hat))/eps
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
#' last_est <- rep(0, ncol(x))
#' graboo_reg(x, y, last_est)
graboo_reg <- function(x, y, last_est, loss = mse, eta = 0.1){
  # Calculate the negative gradient
  nega_gra_value <- nega_gra(loss, x, y, last_est)
  # Build regression using the negative gradient as the respondse
  coef_est <- lm(nega_gra_value ~ -1 + x)$coefficients
  # Update the value of coefficients based on the estimate in the last weak model
  new_est <- last_est + coef_est * eta
  return(new_est)
}

#' Function that implement one weak model step of Gradient Boosting in regression
#'
#' @param data - list of data that fweak need
#' @param fweak - function that generates estimate from weak model based on input, its default value is graboo_reg
#' @param loss - the loss function used, its default value is the mean of the square error
#' @param eta - the step size we use to update the total estimate each time, its default value is 0.1
#'
#' @return outputs graboo_fit1(fweak, data)
#' @export
#'
#' @examples
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5) + rnorm(200, 0, 3)
#' data$last_est <- rep(0, ncol(data$x))
#' graboo_fit1(data)
graboo_fit1 <- function(data, fweak = graboo_reg, loss = mse, eta = 0.1){
  # Set the fweak function based on the input
  fweak_new <- function(x, y, last_est){
    fweak(x, y, last_est, loss = loss, eta = eta)
  }
  # Fit the weak model
  coef <- fit_model(fweak_new, F, data)
  # Construct the trained model based on the coef
  model_train <- function(x){
    x %*% coef
  }
  # Returen the trained function
  model_train
}


#' Function that implement the algorithm of Ensemble learning in Gradient Boosting
#'
#' @param data - list of data that fweak need including x, y and last_est
#' @param model_num - the number of weak models you want to train and combine
#' @param loss - the loss function used, its default value is the mean of the square error
#' @param eta - the step size we use to update the total estimate each time, its default value is 0.1
#' @param fweak - function that generates estimate from weak model based on input, its default value is graboo_reg
#'
#' @return Graboo(fweak, data, model_num)
#' @export
#'
#' @examples
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5) + rnorm(200, 0, 3)
#' data$last_est <- rep(0, 5)
#' model_num <- 100
#' Graboo(data, model_num)
Graboo <- function(data, model_num, loss = mse, eta = 0.1, fweak = graboo_reg){
  # Initialize multi_est for storing the fitting results of weak models
  model_train <- list()
  length(model_train) <- model_num
  # Fit the weak models
  for(i in 1:model_num){
    model_train[[i]] <- graboo_fit1(data, fweak)
    data$last_est <- model_train[[i]](diag(rep(1, ncol(data$x))))
  }
  # Get the fitted values based on the trained models
  comb_out <- prediction(data$x, model_train, parallel = F)
  # Return the fitted values on training data and the list of weak models
  list(fitted_values = comb_out, model_train = model_train)
}





