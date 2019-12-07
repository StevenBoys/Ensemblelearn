#' Function that combines results from weak models in parallel
#'
#' @param multi_est - list containing multiple estimates from weak models
#' @param weights - nonegative vector that combines the multiple estimates
#'
#' @return outputs Comb_parallel(multi_est, weights),
#' @export
#'
#' @examples
#' multi_est <- list(1, 2, 3)
#' weights <- c(0.2, 0.4, 0.4)
#' Comb_parallel(multi_est, weights)
Comb_parallel <- function(multi_est, weights){
  # Check if all the weights are non-negative and not all zero
  if(mean(weights < 0) != 0){
    stop("The weights should be all non-negative.")
  }else if(mean(weights == 0) == 1){
    stop("The weights should not all be 0.")
  }
  # Check the length of the weights
  if(length(weights) != length(multi_est)){
    stop("The length of the weights should be the same with the length of multi_est.")
  }
  # Normalize the weights
  weights <- weights/sum(weights)
  # Calculate the weighted sums of the multiple estimates from weak models
  comb_out <- multi_est[[1]] * weights[1]
  for(i in 2:length(multi_est)){
    comb_out <-  comb_out + multi_est[[i]] * weights[i]
  }
  # Return the parallel combined result
  return(comb_out)
}


#' Function that gets results from weak model
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param parallel - logical value, true if its results will be combined in parallel
#' @param data - list of data that fweak need
#'
#' @return outputs fit_model(fweak, parallel, data)
#' @export
#'
#' @examples
#' fweak <- function(x, y){
#'   lm(y ~ -1 + x)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' parallel <- TRUE
#' data$y <- data$x %*% rnorm(5) + rnorm(200, 0, 3)
#' fit_model(fweak, parallel, data)
fit_model <- function(fweak, parallel, data){
  # When fweak's results will be combined in parallel
  if(parallel){
    # Check the compatibility of the data based on parameter parallel
    if(mean(c("y", "x") %in% names(data)) < 1){
      stop("The list of data should include x and y.")
    }else if(length(y) != nrow(x)){
      # Check the compatibility of the dimension of x and y
      stop("The dimension of y is not compatible with that of x.")
    }
    x <- as.matrix(data$x); y <- data$y
    fweak_value <- fweak(x, y)
  }else{
    # Check the compatibility of the data based on parameter parallel
    if(mean(c("y", "x", "last_est") %in% names(data)) < 1){
      stop("The list of data should include x, y and last_est.")
    }else if(length(y) != nrow(x)){
      # Check the compatibility of the dimension of x and y
      stop("The dimension of y is not compatible with that of x.")
    }
    # When fweak's results will be combined in series
    x <- data$x; y <- data$y; last_est <- data$last_est
    fweak_value <- fweak(x, y, last_est)
  }
  # Return the new fitted value
  return(fweak_value)
}

#' Function that does prediction based on the fitted models for the new coming data
#'
#' @param x - the new coming data that we want to do prediction on
#' @param model_train - the list of models trained on the training data
#' @param parallel - logical value, true if its results will be combined in parallel
#'
#' @return outputs prediction(x, model_train, parallel)
#' @export
#'
#' @examples
#' x = matrix(rnorm(1000), 200, 5)
#' y <- x %*% rnorm(5) + rnorm(200, 0, 3)
#' parallel <- TRUE
#' coef <- lm(y ~ -1 + x)$coefficients
#' model_train <- list()
#' model_train[[1]] <- function(x){
#'   x <- matrix(x, ncol = length(coef))
#'   x %*% coef
#' }
#' x_new <- matrix(rnorm(5), 1, 5)
#' prediction(x_new, model_train, parallel)
prediction <- function(x, model_train, parallel){
  # Initialize the list that stores the estimates for new data
  multi_est <- list(); length(multi_est) <- length(model_train)
  if(parallel){
    # If the models are based on parallel setting
    for(i in 1:length(model_train)){
      multi_est[[i]] <- model_train[[i]](x)
    }
  }else{
    # If the models are based on series setting
    multi_est <- model_train[[length(model_train)]](x)
  }
  multi_est
}

