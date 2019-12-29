data("iris")

dataset <- data.table::as.data.table(iris)
target_col <- "Species"
id_col <- NULL
split <- 0.7

lightgbm <- reticulate::import("lightgbm")

self <- list()

self$dataset <- dataset
self$target_names <- target_col
self$feature_names <- setdiff(
  c(colnames(self$dataset), id_col),
  self$target_names
)

ps <- lgbparams()
ps$values <- list("objective" = "multiclass")

self$split <- split



# get parameters
pars = ps$get_values(tags = "train")


# transform target to numeric for classification tasks
if (pars[["objective"]] %in% c("binary", "multiclass",
                               "multiclassova", "lambdarank")) {

  # if target is not numeric
  if (!is.numeric(self$dataset[, get(self$target_names)])) {
    self$dataset[
      , (self$target_names) := (as.numeric(get(self$target_names)) - 1L)
      ]

    # if target is numeric and integer
  } else if (is.integer(self$dataset[, get(self$target_names)])) {

    # check, if minimum != 0 --> if == 0, we have nothing to do
    if (self$dataset[, min(get(self$target_names))] != 0) {

      # if min == 1, substract 1 --> lightgbm need the first class
      # to be 0
      if (self$dataset[, min(get(self$target_names))] == 1) {
        self$dataset[
          , (self$target_names) := (get(self$target_names) - 1L)
          ]

        # else stop with warning
      } else {
        stop(
          paste0("Please provide a valid target variable ",
                 "for classification tasks")
        )
      }
    }

    # stop if target is numeric and not integer
  } else {
    stop(
      paste0("Please provide a valid target variable ",
             "for classification tasks")
    )
  }

  # extract classification classes

  if (self$dataset[, length(unique(get(self$target_names)))] > 2) {
    stopifnot(
      pars[["objective"]] %in%
        c("multiclass", "multiclassova", "lambdarank")
    )
    pars[["num_class"]] <- self$dataset[, length(
      unique(get(self$target_names))
      )]
  }

  # transform numeric variables
} else {

  # we have only work here, if target is not numeric
  if (!is.numeric(self$dataset[, get(self$target_names)])) {

    # try to transform to numeric
    transform_error <- tryCatch(
      expr = {
        self$dataset[
          , (self$target_names) := as.numeric(
            as.character(get(self$target_names))
          )]
        ret <- FALSE
        ret
      }, error = function(e) {
        print(e)
        ret <- TRUE
        ret
      }, finally = function(f) {
        return(ret)
      }
    )
    stopifnot(isFALSE(transform_error))
  }
}

self$split <- split

if (self$split < 1) {
  train_index <- caret::createDataPartition(
    y = self$dataset[, get(self$target_names)],
    times = 1,
    p = self$split
  )[[1]]
  valid_index <- setdiff(1:nrow(self$dataset), train_index)

  self$train_data <- self$dataset[
    train_index, (self$feature_names), with = FALSE
    ]
  self$train_label <- self$dataset[train_index, get(self$target_names)]

  self$valid_data <- self$dataset[
    valid_index, (self$feature_names), with = FALSE
    ]
  self$valid_label <- self$dataset[valid_index, get(self$target_names)]
} else {
  self$train_data <- self$dataset[
    , (self$feature_names), with = FALSE
    ]
  self$train_label <- self$dataset[, get(self$target_names)]
}



num_boost_round = 10000
early_stopping_rounds = 1000
feval = NULL
verbose_eval = 50


num_boost_round <- as.integer(num_boost_round)
early_stopping_rounds <- as.integer(early_stopping_rounds)
verbose_eval <- as.integer(verbose_eval)

x_train <- as.matrix(self$train_data)
# convert Missings to NaN, otherwise they wil be transformed
# wrong to python/ an error occurs
for (i in colnames(x_train)) {
  x_train[which(is.na(x_train[, i])), i] <- NaN
}
x_label <- self$train_label
self$label_names <- unique(x_label)

# python lightgbm$Dataset
train_set <- lightgbm$Dataset(
  data = x_train,
  label = x_label
)

if (!is.null(self$valid_data)) {
  x_valid <- as.matrix(self$valid_data)
  for (i in colnames(x_valid)) {
    x_valid[which(is.na(x_valid[, i])), i] <- NaN
  }
  x_label_valid <- self$valid_label

  # python lightgbm$Dataset
  valid_set <- lightgbm$Dataset(
    data = x_valid,
    label = x_label_valid
  )
} else {
  valid_set <- train_set
}


self$model <- lightgbm$train(
  params = pars,
  train_set = train_set,
  num_boost_round = num_boost_round,
  valid_sets = valid_set,
  feval = feval,
  early_stopping_rounds = early_stopping_rounds,
  verbose_eval = verbose_eval
)

self$model$best_iteration
self$model$best_score
