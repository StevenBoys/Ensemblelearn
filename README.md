# Ensemblelearn Package

Bowen Lei

## Ensemblelearn: Better the prediction based on weak models

This package include lots of popular Ensemble Learning algorithm like Bayes optimal classifier, Bootstrap aggregating (bagging), Boosting, Bayesian parameter averaging, Bayesian model combination, Bucket of models, Stacking and so on. I will implement these algorithms. And the goal of this package is to better the prediction based on the weak models. 

And it also provides the framework for people to define their specific ensemble learning algorithm and help people to compute the results. People can define the weak models as well as the way of combining them. There are two ways of combining the weak models. One is parallel and one is series. The output of parallel is always a weighted sum of the outputs of the weak models. As for the series, the output of the last weak model will be the input of the next weak model.

## Installation

```
devtools::install_github("StevenBoys/Ensemblelearn")
```

## Usage

```
library(Ensemblelearn)
```






