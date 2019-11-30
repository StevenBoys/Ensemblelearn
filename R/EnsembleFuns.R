#' Function that combines results from weak models in parallel
#'
#' @param multi_est - list containing multiple estimates from weak models
#' @param weights - nonegative vector that combines the multiple estimates
#' @param reg - logical value, true if the weak model is doing regression, otherwise it's doing classification
#'
#' @return outputs Comb_parallel(multi_est, weights),
#' @export
#'
#' @examples
#' multi_est <- list(1, 2, 3)
#' weights <- c(0.2, 0.4, 0.4)
#' reg <- T
#' Comb_parallel(multi_est, weights, reg)
Comb_parallel <- function(multi_est, weights, reg){
  # Normalize the weights
  weights <- weights/sum(weights)
  # Check the task is regression or classification
  if(reg){
    # Calculate the weighted sums of the multiple estimates from weak models
    comb_out <- multi_est[[1]] * weights[1]
    for(i in 2:length(multi_est)){
      comb_out <-  comb_out + multi_est[[i]] * weights[i]
    }
  }else{
    # Calculate the votes of each class from weak models
    class_num <- length(unique(multi_est))
    proba <- rep(0, class_num)
    for(i in 1:length(multi_est)){
      proba[multi_est[[i]]] <- proba[multi_est[[i]]] * weights[i]
    }
    comb_out <- which.max(proba)
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
#'   lm(y ~ x)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' parallel <- TRUE
#' data$y <- data$x %*% rnorm(5)
#' fit_model(fweak, parallel, data)
fit_model <- function(fweak, parallel, data){
  # When fweak's results will be combined in parallel
  if(parallel){
    x <- data$x; y <- data$y
    fweak_value <- fweak(x, y)
  }else{
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
#' @param reg - logical value, true if the weak model is doing regression, otherwise it's doing classification
#' @param parallel - logical value, true if its results will be combined in parallel
#'
#' @return outputs prediction(x, model_train, reg, parallel)
#' @export
#'
#' @examples
#' x = matrix(rnorm(1000), 200, 5)
#' y <- x %*% rnorm(5)
#' parallel <- TRUE
#' model_train <- list(); model_train[[1]] <- lm(y ~ -1 + x)$coefficients
#' x_new <- matrix(rnorm(5), 1, 5)
#' prediction(x_new, model_train, reg, parallel)
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


