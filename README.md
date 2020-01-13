# lightgbm.py (!!!under development!!!)

<!-- badges: start -->
[![pipeline status](https://gitlab.com/kapsner/lightgbm-py/badges/master/pipeline.svg)](https://gitlab.com/kapsner/lightgbm-py/commits/master)
[![coverage report](https://gitlab.com/kapsner/lightgbm-py/badges/master/coverage.svg)](https://gitlab.com/kapsner/lightgbm-py/commits/master)
<!-- badges: end -->

The goal of [lightgbm.py](https://github.com/kapsner/lightgbm.py) is to provide the [LightGBM gradient booster](https://lightgbm.readthedocs.io) with an R package, using its [python module](https://github.com/microsoft/LightGBM/tree/master/python-package). It is therefore easy to install and can also be integrated into other R packages as a dependency (such as the [mlr3learners.lgbpy](https://github.com/kapsner/mlr3learners.lgbpy) R package).

The [original LightGBM R package](https://github.com/microsoft/LightGBM/tree/master/R-package) comes with some difficulties regarding its installation workflow. However, to be used by other R packages, a simple installation workflow, which can also be automatized, is required. Hence the idea was born, to implement an R package based on [LightGBM's python module](https://github.com/microsoft/LightGBM/tree/master/python-package) by using the [reticulate](https://github.com/rstudio/reticulate) R package as an R interface to Python. 

The code of `lightgbm.py` is implemented using the [R6](https://github.com/r-lib/R6) class for object oriented programming in R. 

# Features 

* integrated native cross-validation (CV) step before the actual model training to find the optimal `num_boost_round` for the given training data and parameter set  
* GPU support  

# Installation

You can install the development version of `lightgbm.py` with:

``` r
install.packages("devtools")
devtools::install_github("kapsner/lightgbm.py")
```

# Example

This is a basic example which shows you how to create a binary classifier. For further details please view the [package vignettes](vignettes/). 

``` r
library(lightgbm.py)
library(mlbench)
```

## Prerequisites

Before you can start using the `lightgbm.py` package, make sure, the `reticulate` R package is configured properly on your system (`reticulate` version >= 1.14) and is pointing to a python environment. If not, you can e.g. install `miniconda`:

```r
reticulate::install_miniconda(
  path = reticulate::miniconda_path(),
  update = TRUE,
  force = FALSE
)
reticulate::py_config()
```

Then use the function `lightgbm.py::install_py_lightgbm` in order to install the lightgbm python module. This function will first look, if the reticulate package is configured well and if the python module `lightgbm` is aready present. If not, it is automatically installed. 

```r
lightgbm.py::install_py_lightgbm()
```

## Load the dataset

The data must be provided as a `data.table` object. To simplify the subsequent steps, the target column name and the ID column name are assigned to the variables `target_col` and `id_col`, respectively. 

```r
data("PimaIndiansDiabetes2")
dataset <- data.table::as.data.table(PimaIndiansDiabetes2)
target_col <- "diabetes"
id_col <- NULL
```

To evaluate the model's performance, the dataset is split into a training set and a test set with `lightgbm.py::sklearn_train_test_split`. This function is a wrapper around python sklearn's [sklearn.model_selection.train_test_split](https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.train_test_split.html) method a ensures a stratified sampling. 

```r
split <- lightgbm.py::sklearn_train_test_split(
  dataset,
  target_col,
  split = 0.7,
  seed = 17,
  return_only_index = TRUE
)
```

## Instantiate the lightgbm learner 

Initially, the LightGBM class needs to be instantiated: 

```r
lgb_learner <- LightGBM$new(
  dataset = dataset[split$train_index, ],
  target_col = target_col,
  id_col = id_col
)
```

## Configure the learner 

Next, the learner parameters need to be set. At least, the `objective` parameter needs to be provided! Almost all possible parameters have been implemented here. You can inspect them using the following command: 

```r
lgb_learner$param_set
```

Please refer to the [LightGBM manual](https://lightgbm.readthedocs.io) for further details on these parameters.  

```r
lgb_learner$param_set$values <- list(
  "objective" = "binary",
  "learning_rate" = 0.01,
  "seed" = 17L,
  "metric" = "auc"
)
```

## Train the learner 

The learner is now ready to be trained by using its `train` function. The parameters `num_boost_round` and `early_stopping_rounds` can be set here. Please refer to the [LightGBM manual](https://lightgbm.readthedocs.io) for further details these parameters. 

```r
lgb_learner$num_boost_round <- 5000
lgb_learner$early_stopping_rounds <- 1000
lgb_learner$train()
```

## Evaluate the model performance 

The learner's `predict` function returns a list object, which consists of the predicted probabilities for each class and the predicted class labels: 

```r
predictions <- lgb_learner$predict(newdata = dataset[split$test_index, ])
head(predictions)
```

In order to calculate the model metrics, the test's set target variable has to be transformed accordingly to the learner's target variable's transformation. The value mappings are stored in the learner's object `value_mapping`:

```r
# before transformation
head(dataset[split$test_index, get(target_col)])

# use the learners transform_target-method
target_test <- lgb_learner$trans_tar$transform_target(
  vector = dataset[split$test_index, get(target_col)],
  mapping = "dvalid"
)
# after transformation
head(target_test)

lgb_learner$trans_tar$value_mapping_dvalid
```

Now, several model metrics can be calculated:

```r
MLmetrics::ConfusionMatrix(
  y_true = target_test,
  y_pred = ifelse(predictions > 0.5, 1, 0)
)
MLmetrics::Accuracy(
  y_true = target_test,
  y_pred = ifelse(predictions > 0.5, 1, 0)
)
MLmetrics::AUC(
  y_true = target_test,
  y_pred = predictions
)
```

The variable importance plot can be calculated by using the learner's `importance` function: 

```r
imp <- lgb_learner$importance()
imp$raw_values
plot(imp$plot)
```

For further information and examples, please view the `lightgbm.py` [package vignettes](vignettes/).  

# GPU acceleration

The `lightgbm.py` can also be used with lightgbm's GPU compiled version.

To install the lightgbm python package with GPU support, execute the following commands ([lightgbm manual](https://github.com/microsoft/LightGBM/blob/master/python-package/README.md)):

```bash
pip install lightgbm --install-option=--gpu
```

In order to use the GPU acceleration, the parameter `device_type = "gpu"` (default: "cpu") needs to be set. According to the [LightGBM parameter manual](https://lightgbm.readthedocs.io/en/latest/Parameters.html), 'it is recommended to use the smaller `max_bin` (e.g. 63) to get the better speed up'. 

```r
lgb_learner$param_set$values <- list(
  "objective" = "multiclass",
  "learning_rate" = 0.01,
  "seed" = 17L,
  "metric" = "multi_logloss",
  "device_type" = "gpu",
  "max_bin" = 63L
)
```

All other steps are similar to the workflow without GPU support. 

The GPU support has been tested in a [Docker container](https://github.com/kapsner/docker_images/blob/master/Rdatascience/rdsc_gpu/Dockerfile) running on a Linux 19.10 host, Intel i7, 16 GB RAM, an NVIDIA(R) RTX 2060, CUDA(R) 10.2 and [nvidia-docker](https://github.com/NVIDIA/nvidia-docker). 

# More Infos:

- RStudio's reticulate R package: https://rstudio.github.io/reticulate/
- Microsoft's LightGBM: https://lightgbm.readthedocs.io/en/latest/
- Python's scikit-learn: https://scikit-learn.org/stable/
