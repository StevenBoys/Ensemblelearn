#' Function that builds weak model on Decision Tree for regression task
#'
#' @param x - input independent variables x for the training
#' @param y - input dependent variable y for the traisning
#'
#' @return The trained Decision Tree model based on the input data.
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
#' @param data - list of data that fweak need
#' @param fweak - function that generates estimate from weak model based on input, its default value is dt_reg
#' @param fea_len - the number of features we sample each time, its default value is null
#'
#' @return A trained model function based on the one implementation of the weak model, whose input is the independent variables.
#' @export
#'
#' @examples
#' x <- matrix(rnorm(4000), 200, 20)
#' beta <- rnorm(5)
#' y <- x[, 1:length(beta)] %*% beta + rnorm(200)
#' data <- list(x = x, y = y)
#' randomforest_fit1(data)
randomforest_fit1 <- function(data, fweak = dt_reg, fea_len = NULL){
  # Set the number of features we sample each time if it's initial value is null
  if(is.null(fea_len)){
    fea_len <- floor(ncol(data$x)/2)
  }else if(floor(fea_len) <= 0){
    # Check the compatibility of fea_len
    stop("The value of the floor of fea_len should be positive.")
  }
  # Check the compatibility of x.
  if(ncol(data$x) < 2){
    stop("The number of features on x should be bigger than 1.")
  }
  # Get the resample index
  index_resample <- sample(1:ncol(data$x), fea_len, replace = F)
  # Get the part of data resampled
  data$x <- data$x[, index_resample]
  # Fit the weak model based on the resampled data
  rpart_mod <- fit_model(fweak, T, data)
  # Construct the trained model based on the rpart_mod
  model_train <- function(x){
    predict(rpart_mod, x[, index_resample])
  }
  # Returen the trained function
  model_train
}


#' Function that implement the algorithm of Ensemble learning in Random Forest
#'
#' @param data - list of data that fweak need
#' @param model_num - the number of weak models you want to train and combine
#' @param fweak - function that generates estimate from weak model based on input
#'
#' @return A list of
#'        \item{fitted_value}{ - fitted value on the training dataset based on the trained model}
#'        \item{model_train}{ - a list of trained weak models}
#' @export
#'
#' @examples
#' x <- matrix(rnorm(4000), 200, 20)
#' beta <- rnorm(5)
#' y <- x[, 1:length(beta)] %*% beta + rnorm(200)
#' data <- list(x = x, y = y)
#' model_num <- 100
#' Randomforest(data, model_num)
Randomforest <- function(data, model_num, fweak = dt_reg){
  # Check the compatibility of model_num
  if(model_num <= 0){
    stop("The value of model_num should be positive.")
  }
  # Initialize multi_est for storing the fitting results of weak models
  model_train <- list()
  length(model_train) <- model_num
  # Fit the weak models
  for(i in 1:model_num){
    model_train[[i]] <- randomforest_fit1(data = data, fweak = fweak)
  }
  # Get the multiple estimation based on the trained models
  data$x <- as.data.frame(data$x)
  multi_est <- prediction(data$x, model_train, parallel = T)
  # Combine the multiple estimation
  comb_out <- Comb_parallel(multi_est, rep(1, model_num))
  # Return the fitted values on training data and the list of weak models
  list(fitted_values = comb_out, model_train = model_train)
}


