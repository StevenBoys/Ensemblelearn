#' Function that combines results from weak models in parallel
#'
#' @param data - list of data that fweak need, including x and y when parallel is true and x, y and last_est when parallel is False
#' @param parallel - logical value, true if its results will be combined in parallel
#' @param fit_fun - function that implement one fweak in the user-defined Ensemble Learning: if parallel is False, it should return both the trained model and the updated last_est
#' @param fweak - function that generates estimate from weak model based on input
#' @param model_num - the number of weak models you want to train and combine
#'
#' @return outputs Ensemble_define(data, parallel, fit_fun, fweak, model_num),
#' @export
#'
#' @examples
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5) + rnorm(200, 0, 3)
#' data$last_est <- rep(0, 5)
#' parallel <- FALSE
#' fit_fun <- function(data, fweak){
#'   fit <- graboo_fit1(data = data, fweak = fweak)
#'   last_est <- fit(diag(rep(1, ncol(data$x))))
#'   list(model = fit, last_est = last_est)
#' }
#' fweak <- graboo_reg
#' model_num <- 100
#' Ensemble_define(data, parallel, fit_fun, fweak, model_num)
Ensemble_define <- function(data, parallel, fit_fun, fweak, model_num){
  # Initialize multi_est for storing the fitting results of weak models
  model_train <- list()
  length(model_train) <- model_num
  if(parallel){
    # Fit the weak models
    for(i in 1:model_num){
      model_train[[i]] <- fit_fun(fweak = fweak, data = data)
    }
    # Get the multiple estimation based on the trained models
    multi_est <- prediction(data$x, model_train, parallel = T)
    # Combine the multiple estimation
    comb_out <- Comb_parallel(multi_est, rep(1, model_num))
  }else{
    # Fit the weak models
    for(i in 1:model_num){
      fit <- fit_fun(fweak = fweak, data = data)
      model_train[[i]] <- fit$model
      # Update the value of last_est
      data$last_est <- fit$last_est
    }
    # Get the fitted values based on the trained models
    comb_out <- prediction(data$x, model_train, parallel = F)
  }
  # Return the fitted value and the trained weak models
  list(fitted_values = comb_out, model_train = model_train)
}
