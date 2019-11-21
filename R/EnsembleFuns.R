#' Function that combines results from weak models in parallel
#'
#' @param multi_est - list containing multiple estimates from weak models
#' @param weights - nonegative vector that combines the multiple estimates
#'
#' @return outputs Comb_parallel(multi_est, weights)
#' @export
#'
#' @examples
#' multi_est <- list(1, 2, 3)
#' weights <- c(0.2, 0.4, 0.4)
#' Comb_parallel(multi_est, weights)
Comb_parallel <- function(multi_est, weights){
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
#' @return outputs Comb_parallel(multi_est, weights)
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
