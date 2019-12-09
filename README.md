# lightgbm.py (!!!under development!!!)

<!-- badges: start -->
[![pipeline status](https://gitlab.com/kapsner/lightgbm-py/badges/master/pipeline.svg)](https://gitlab.com/kapsner/lightgbm-py/commits/master)
[![coverage report](https://gitlab.com/kapsner/lightgbm-py/badges/master/coverage.svg)](https://gitlab.com/kapsner/lightgbm-py/commits/master)
<!-- badges: end -->

The goal of lightgbm.py is to bring the famous [LightGBM](https://lightgbm.readthedocs.io) gradient booster into an R package which is easy to install. 
The [original R package](https://github.com/microsoft/LightGBM/tree/master/R-package) comes with a quite difficult installation workflow. However, to be used by other R packages, a simple installation workflow, which can also be automatized, is required. Hence the idea was born, to implement an R package based on [LightGBM's python module](https://github.com/microsoft/LightGBM/tree/master/python-package) by using the [reticulate](https://github.com/rstudio/reticulate) R package as an R interface to Python. 

The code of `lightgbm.py` is implemented using the [R6](https://github.com/r-lib/R6) class for object oriented programming in R. 

## Installation

You can install the development version of `lightgbm.py` with:

``` r
install.packages("devtools")
devtools::install_github("kapsner/lightgbm.py")
```

## Example

This is a basic example which shows you how to create a binary classifier. For further details please view the package's vignettes. 

``` r
library(lightgbm.py)
library(mlbench)
```

## Prerequisites

Before you can start using the `lightgbm.py` package, make sure, the reticulate package is configured properly on your system. If not, you can e.g. install `miniconda`:

```r
reticulate::install_miniconda(
  path = reticulate::miniconda_path(),
  update = TRUE,
  force = FALSE
)
reticulate::py_config()
```

Then use the function `install_py_lightgbm` in order to install the lightgbm python module. This function will first look, if the reticulate package is configured well and if the python module `lightgbm` is aready present. If not, it is automatically installed. 

```r
lightgbm.py::install_py_lightgbm()
```

## Load and prepare data

The data must be provided as a `data.table` object. To simplify the subsequent steps, the target column name and the ID column name are stored in the objects `target_col` and `id_col`, respectively. 

```r
data("PimaIndiansDiabetes2")
dataset <- data.table::as.data.table(PimaIndiansDiabetes2)
target_col <- "diabetes"
id_col <- NULL
```

To evaluate the model performance, the dataset is split into a training set and a test set with `caret::createDataPartition`. This function ensures are stratified sampling. 

```r
set.seed(17)
train_index <- caret::createDataPartition(
  y = dataset[, get(target_col)],
  times = 1,
  p = 0.7
)[[1]]

# test index
test_index <- setdiff(1:nrow(dataset), train_index)
```

## Instantiate lgb_learner 

Initially, the LightgbmTrain class needs to be instantiated: 

```r
lgb_learner <- LightgbmTrain$new(
  dataset = dataset[train_index, ],
  target_col = target_col,
  id_col = id_col
)
```

## Prepare learner 

Next, the learner parameters need to be set. At least, the `objective` parameter needs to be provided! Almost all possible parameters have been implemented here. You can inspect them using the following command: 

```r
lgb_learner$param_set
```

Please refer to the [LightGBM manual](https://lightgbm.readthedocs.io) for further details on these parameters.  

```r
lgb_learner$param_set$values <- list(
  "objective" = "binary",
  "learning_rate" = 0.01,
  "seed" = 17L
)
```

When we have set the learner's objective, we can perform the data preprocessing step by using the learner's function `data_preprocessing`. This function takes two arguments, `validation_split` (default = 0.7) and `split_seed` (defaul: NULL). 
`validation_split` can be set in order to further split the training data and evaluate the model performance during training against the validation set. The allowed value range is 0 < validation_split <= 1. This parameter can also be set to "1", taking the whole training data for validation during the model training. For reproducibility, please use the `split_seed` argument. 

```r
lgb_learner$data_preprocessing(validation_split = 0.7, split_seed = 2)
```

## Train learner

We can now train the learner by using its `train` function. The parameters `num_boost_round` and `early_stopping_rounds` can be set here. Please refer to the [LightGBM manual](https://lightgbm.readthedocs.io) for further details these parameters. 

```r
lgb_learner$train(
  num_boost_round = 5000,
  early_stopping_rounds = 1000
)
```

## Evaluate Model Performance

Basic values can be assesed directly from the model: 

```r
lgb_learner$model$best_iteration
lgb_learner$model$best_score$valid_0
```

The learner's `predict` function returns a list object, which consists of the predicted probabilities for each class and the predicted class labels: 

```r
predictions <- lgb_learner$predict(newdata = dataset[test_index,])
head(predictions$probabilities)
```

A confusion matrix can be calculated using the learner's function `confusion_matrix`: 

```r
lgb_learner$confusion_matrix(
  y_true = dataset[test_index, get(lgb_learner$target_names)]
)
```

The variable importance plot can be calculated by using the learner's `importance` function: 

```r
imp <- lgb_learner$importance()
imp$raw_values
```

```r
filename <- "./imp_plot_binary.png"
grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
print(imp$plot)
grDevices::dev.off()
knitr::include_graphics(filename)
```

