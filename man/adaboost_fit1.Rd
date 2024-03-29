% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adaboost.R
\name{adaboost_fit1}
\alias{adaboost_fit1}
\title{Function that implement one weak model step of AdaBoost in regression}
\usage{
adaboost_fit1(fweak, data)
}
\arguments{
\item{fweak}{- function that generates estimate from weak model based on input}

\item{data}{- list of data that fweak need including x, y and last_est which is the sample weight for this step}
}
\value{
A trained model function based on the one implementation of the weak model, whose input is the independent variables.
}
\description{
Function that implement one weak model step of AdaBoost in regression
}
\examples{
fweak <- function(x, y, last_est){
  lm(y ~ -1 + x, weights = last_est)$coefficients
}
data <- list(x = matrix(rnorm(1000), 200, 5))
data$y <- data$x \%*\% rnorm(5) + rnorm(200, 0, 3)
data$last_est <- rep(1/length(data$y), length(data$y))
adaboost_fit1(fweak, data)
}
