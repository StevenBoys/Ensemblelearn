# Ensemblelearn Package

Bowen Lei

## Ensemblelearn: Better the prediction in regression based on weak models

This package include lots of popular Ensemble Learning algorithm like Bagging, Random Forest, Gradient Boosting and Adaboost. And the goal of this package is to better the prediction in regression based on the weak models. 

And it also provides the framework for people to define their specific ensemble learning algorithm and help people to compute the results. People can define the weak models as well as the way of combining them. There are two ways of combining the weak models. One is parallel and one is series. The output of parallel is always a weighted sum of the outputs of the weak models. As for the series, the output of the last weak model will be the input of the next weak model.

There are some brief instructions about using this package. And you can **check the vignette in the package to see the detailed description about it by using the following code.**

```
vignette("Ensemblelearn")
```

## Installation

```
require(devtools)
devtools::install_github("StevenBoys/Ensemblelearn", build_vignettes = T)
```

## Usage

```
library(Ensemblelearn)
```

We can simulate the training data first.

```
# Simulate the data and put them into a list
set.seed(2); n <- 200; p <- 5
data <- list(x = matrix(rnorm(n * p), n, p))
eps <- rnorm(n, 0, 3)
beta <- rnorm(p)
data$y <- data$x %*% beta + eps
```

### Bagging

For the Bagging algorithm, we can use the following code to train the model.

```
# Build a weak model for Bagging
fweak <- function(x, y){
  lm(y ~ -1 + x)$coefficients
}
# Set the number of weak models
model_num <- 50
# Use Bagging algorithm to train the data
model_bagging <- Bagging(fweak, data, model_num)
```

And when there are new data coming, we can do prediction based on the trained Bagging model.

```
# Set the parallel indicator
parallel <- TRUE
# Set the weights for combining the weak models
weights <- rep(1, length(model_bagging$model_train))
# Simulate the new coming dataset
x_new <- matrix(rnorm(n1*p), n1, p)
eps_new <- rnorm(n1)
# Do prediction based on the bagging model
multipre_bagging <- prediction(x_new, model_bagging$model_train, parallel)
pre_bagging <- Comb_parallel(multipre_bagging, weights)
```

### Random Forest

For the Random Forest algorithm, based on the data simulated above, we can use the following code to train the model.

```
# Build the Random Forest model
model_rf <- Randomforest(data, model_num)
```

And when there are new data coming, we can do prediction based on the trained Random Forest model.

```
# Do prediction based on the random forest model
multipre_rf <- prediction(x_new, model_rf$model_train, parallel)
pre_rf <- Comb_parallel(multipre_rf, weights)
```
### Adaboost

For the Adaboost algorithm, based on the data simulated above, we can use the following code to train the model.

```
# Build a weak model for Adaboost
fweak <- function(x, y, last_est){
  lm(y ~ -1 + x, weights = last_est)$coefficients
}
# Initialize the weights
data$last_est <- rep(1/length(data$y), length(data$y))
# Build the Adaboost model
model_adaboost <- Adaboost(fweak, data, model_num)
```

And when there are new data coming, we can do prediction based on the trained Adaboost model.

```
# Do prediction based on the adaboost model
multi_est <- prediction(x_new, model_adaboost$model_train, parallel)
pre_adaboost <- Comb_parallel(multi_est, model_adaboost$weights)
```

### Gradient Boosting

For the Gradient Boosting algorithm, based on the data simulated above, we can use the following code to train the model.

```
# Initialize the last_est in the list of data
data$last_est <- rep(0, ncol(data$x))
# Build the Gradient Boosting model
model_graboo <- Graboo(data, model_num)
```

And when there are new data coming, we can do prediction based on the trained Gradient Boosting model.

```
# Set the parallel indicator
parallel <- FALSE
# Do prediction based on the Gradient Boosting
pre_graboo <- prediction(x_new, model_graboo$model_train, parallel)
```

### User-defined Ensemble Learning Algorithm

We can change the functions of Gradient Boosting to a user-defined Ensemble Learning algorithm. And we keep using the dataset simulated above.

```{r}
# Set the value of parallel for Gradient Boosting
parallel <- FALSE
# Define fit_fun based on graboo_fit1
fit_fun <- function(data, fweak){
  # Fit a Gradient Boosting fweak
  fit <- graboo_fit1(data = data, fweak = fweak)
  # Update the value of last_est
  last_est <- fit(diag(rep(1, ncol(data$x))))
  list(model = fit, last_est = last_est)
}
# Set the fweak
fweak <- graboo_reg
# Set the number of weak models we want to train
model_num <- 100
# Build the user-defined model
model_user_define <- Ensemble_define(data, parallel, fit_fun, fweak, model_num)
```

## References

[1] Bartosz Krawczyk, Leandro L.Minku, João Gama, Jerzy Stefanowski, and Michał Woźniak. Ensemble learning for data stream analysis: A survey. Information Fusion, 37:132–156, 2017.

[2] Omer Sagi and Lior Rokach. Ensemble learning: A survey. WIREs, 8, 2018.

[3] Wikipedia. Ensemble learning. https://en.wikipedia.org/wiki/Ensemble_learning.



