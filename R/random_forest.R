#' Function that builds weak model on Decision Tree for regression task
#'
#' @param x - input independent variables x for the training
#' @param y - input dependent variable y for the training
#'
#' @return dt_reg(x ,y)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(4000), 200, 20)
#' beta <- rnorm(5)
#' y <- x[, 1:length(beta)] %*% beta + rnorm(200)
#' dt_reg(x, y)
dt_reg <- function(x, y){ß
  rpart_mod <- rpart(y ~ x, method="anova")
  return(rpart.mod)
}

#' Function that implement one weak model of Random Forest based on the resample of the features
#'
#' @param fweak - function that generates estimate from weak model based on input, its default value is dt_reg
#' @param data - list of data that fweak need
#' @param fea_len - the number of features we sample each time, its default value is null
#'
#' @return outputs randomforest_fit1(fweak, data)
#' @export
#'
#' @examples
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5)
#' bagging_fit1(fweak, data)
randomforest_fit1 <- function(data, fweak = dt_reg, fea_len = NULL){
  # Set the number of features we sample each time if it's initial value is null
  if(is.null(fea_len)){
    fea_len <- floor(ncol(data$x))
  }
  # Get the resample index
  index_resample <- sample(1:ncol(data$x), ncol(data$x), replace = F)
  # Get the part of data resampled
  data$x <- data$x[, index_resample]
  # Fit the weak model based on the resampled data
  rpart_mod <- fit_model(fweak, T, data)
  # Construct the trained model based on the rpart_mod
  model_train <- function(x){
    predict(rpart_mod, x)
  }
  # Returen the trained function
  model_train
}

