#' Function that builds weak model on Decision Tree for regression task
#'
#' @param x - input independent variables x for the training
#' @param y - input dependent variable y for the traisning
#'
#' @return dt_reg(x ,y)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(4000), 200, 20)
#' beta <- rnorm(5)
#' y <- x[, 1:length(beta)] %*% beta + rnorm(200)
#' dt_reg(x, y)
dt_reg <- function(x, y){
  rpart_mod <- rpart(y ~ x, method="anova")
  return(rpart_mod)
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


#' Function that implement the algorithm of Ensemble learning in Random Forest
#'
#' @param fweak - function that generates estimate from weak model based on input
#' @param data - list of data that fweak need
#' @param model_num - the number of weak models you want to train and combine
#' @param reg - logical value, true if the weak model is doing regression, otherwise it's doing classification
#'
#' @return bagging(fweak, data, model_num, reg)
#' @export
#'
#' @examples
#' fweak <- function(x, y){
#'   lm(y ~ -1 + x)$coefficients
#' }
#' data <- list(x = matrix(rnorm(1000), 200, 5))
#' data$y <- data$x %*% rnorm(5)
#' model_num <- 100; reg <- T; model <- "bagging"
#' Randomforest(fweak, data, model_num)
Randomforest <- function(fweak, data, model_num, reg){
  # Initialize multi_est for storing the fitting results of weak models
  model_train <- list()
  length(model_train) <- model_num
  # Fit the weak models
  for(i in 1:model_num){
    model_train[[i]] <- randomforest_fit1(fweak = fweak, data = data)
  }
  # Get the multiple estimation based on the trained models
  data$x <- as.data.frame(data$x)
  multi_est <- prediction(data$x, model_train, parallel = T)
  # Combine the multiple estimation
  comb_out <- Comb_parallel(multi_est, rep(1, model_num), reg)
  # Return the fitted values on training data and the list of weak models
  list(fitted_values = comb_out, model_train = model_train)
}
