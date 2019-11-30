#' Function that builds weak model on Decision Tree for regression task

rf_reg <- function(x, y){
  rpart.mod=rpart(y ~ x, method="anova")
}

#' Function that builds weak model on Decision Tree for classification task

rf_cla <- function(x, y){

}


#' Function that implement one weak model of Random Forest based on the resample of the features
#'
#' @param fweak - function that generates estimate from weak model based on input, and the default model is Decision Tree
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
randomforest_fit1 <- function(data, fweak){
  # Get the resample index
  index_resample <- sample(1:length(data$y), length(data$y), replace = T)
  # Get the part of data resampled
  data$y <- data$y[index_resample]
  data$x <- data$x[index_resample, ]
  # Fit the weak model based on the resampled data
  fit_model(fweak, T, data)
}

#' Function that implement the algorithm of Bagging
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need
#' @param model_num - the number of weak models you want to train and combine
#' @param reg - logical value, true if the weak model is doing regression, otherwise it's doing classification
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
bagging <- function(fweak, data, model_num, reg){
  # Initialize multi_est for storing the fitting results of weak models
  multi_est <- list()
  length(multi_est) <- model_num
  # Fit the weak models
  for(i in 1:model_num){
    multi_est[[i]] <- bagging_fit1(fweak, data)
  }
  # Combine the results of weak models
  Comb_parallel(multi_est, rep(1, model_num), reg)
}






