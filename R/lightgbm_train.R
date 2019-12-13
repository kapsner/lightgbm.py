#' @title R6 LightGBM train function
#'
#' @description A wrapper around the lightgbm.train python class.
#'
#' @references \url{https://lightgbm.readthedocs.io}
#'
#'
#' @import data.table
#' @import paradox
#'
#' @export
#% @seealso \code{\link{rstudio/reticulate}}
LightgbmTrain <- R6::R6Class(
  classname = "LightgbmTrain",

  private = list(
    # define private objects

    lightgbm = NULL,
    validation_split  = NULL,
    classes = NULL,
    label_names = NULL,

    transform_features = function(data_matrix) {
      for (i in seq_len(ncol(data_matrix))) {
        transf <- tryCatch(
          expr = {
            ret <- as.numeric(as.character(data_matrix[, i]))
            ret
          }, warning = function(e) {
            ret <- as.numeric(factor(data_matrix[, i]))
            ret
          }, finally = function(f) {
            return(ret)
          }
        )
        data_matrix[, i] <- transf
        data_matrix[which(is.na(data_matrix[, i])), i] <- NaN
      }
      return(data_matrix)
    }
  ),

  public = list(

    # define public objects
    #' @field dataset A data.table object holding the dataset, provided at
    #'   instantiation.
    dataset = NULL,

    #' @field feature_names A character vector holding the feature names.
    feature_names = NULL,
    #' @field target_names A character vector holding the target names.
    target_names = NULL,

    #' @field param_set The parameter set.
    param_set = NULL,
    #' @field parameters The modified parameter set, which is provided to the
    #'   train function.
    parameters = NULL,

    #' @field train_data A data.table object holding the training data.
    train_data = NULL,
    #' @field train_label A vector holding the training labels.
    train_label = NULL,

    #' @field valid_data A data.table object holding the validation data.
    valid_data = NULL,
    #' @field valid_label A vector holding the validation labels.
    valid_label = NULL,

    #' @field model The trained lightgbm model (python class 'Booster').
    model = NULL,

    # define methods
    #' @description The initialize function.
    #' @param dataset A data.table object. The dataset used for training.
    #' @param target_col A character string. The name of the target column.
    #' @param id_col (optional) A character string. The name if the ID column
    #'   (default: NULL).
    #'
    initialize = function(dataset, target_col, id_col = NULL) {

      stopifnot(
        data.table::is.data.table(dataset),
        is.character(target_col),
        is.character(id_col) || is.null(id_col),
        target_col %in% colnames(dataset),
        reticulate::py_module_available("lightgbm")
      )

      private$lightgbm <- reticulate::import("lightgbm", delay_load = TRUE)

      self$dataset <- dataset
      self$target_names <- target_col
      self$feature_names <- setdiff(
        c(colnames(self$dataset), id_col),
        self$target_names
      )

      self$param_set <- lgbparams()
    },

    #' @description The data preprocessing function.
    #' @param validation_split A numeric. Ratio to further split the training
    #'   data for validation (default: 0.7). The allowed value range is
    #'   0 < validation_split <= 1. This parameter can also be set to
    #'   '1', taking the whole training data for validation during the model
    #'   training.
    #' @param split_seed A integer (default: NULL). Please use this argument in
    #'   order to generate reproducible results.
    #'
    data_preprocessing = function(validation_split = 0.7, split_seed = NULL) {

      stopifnot(
        is.numeric(validation_split),
        validation_split > 0 && validation_split <= 1,
        is.numeric(split_seed) || is.null(split_seed)
      )

      # get parameters
      self$parameters <- self$param_set$get_values(tags = "train")

      self$dataset[, (self$target_names) := self$transform_target(
        get(self$target_names)
      )]

      private$validation_split <- validation_split

      if (private$validation_split < 1) {

        train_test_split <- tryCatch(
          expr = {
            train_test_split <- sklearn_train_test_split(
              self$dataset,
              self$target_names,
              split = private$validation_split,
              seed = split_seed,
              return_only_index = TRUE,
              stratify = TRUE
            )
            train_test_split
          }, error = function(e) {
            print(e)
            message("\nFalling back to splitting without stratification.")
            train_test_split <- sklearn_train_test_split(
              self$dataset,
              self$target_names,
              split = private$validation_split,
              seed = split_seed,
              return_only_index = TRUE,
              stratify = FALSE
            )
            train_test_split
          }, finally = function(f) {
            return(train_test_split)
          }
        )

        # train index
        train_index <- train_test_split$train_index

        # validation index
        valid_index <- train_test_split$test_index

        # define train_data
        self$train_data <- self$dataset[
          train_index, (self$feature_names), with = FALSE
          ]
        # define train_label
        self$train_label <- self$dataset[
          train_index, get(self$target_names)
          ]

        # define valid_data
        self$valid_data <- self$dataset[
          valid_index, (self$feature_names), with = FALSE
          ]
        # define valid_label
        self$valid_label <- self$dataset[
          valid_index, get(self$target_names)
          ]

        # else, data should not be splitted
      } else {
        self$train_data <- self$dataset[
          , (self$feature_names), with = FALSE
          ]
        self$train_label <- self$dataset[, get(self$target_names)]
      }
    },

    #' @description The training function.
    #'
    #' @details All arguments are passed to the train function of python's
    #'   lightgbm implementation by using R's reticulate package.
    #'
    #' @param num_boost_round A integer. The number of boosting iterations
    #'   (default: 100).
    #' @param early_stopping_rounds A integer. It will stop training if one
    #'   metric of one validation data doesn’t improve in last
    #'   `early_stopping_round` rounds. '0' means disable (default: 0).
    #' @param feval Customized evaluation function (default: NULL).
    #' @param verbose_eval A integer. The eval metric on the valid set is
    #'   printed at every verbose_eval boosting stage (default: 50).
    #'
    train = function(num_boost_round = 100,
                     early_stopping_rounds = 0,
                     feval = NULL,
                     verbose_eval = 50) {

      stopifnot(
        is.numeric(num_boost_round),
        is.numeric(early_stopping_rounds) ||
          is.null(early_stopping_rounds),
        is.numeric(verbose_eval),
        !is.null(self$train_data),
        !is.null(self$train_label)
      )

      num_boost_round <- as.integer(num_boost_round)
      early_stopping_rounds <- as.integer(early_stopping_rounds)
      verbose_eval <- as.integer(verbose_eval)

      x_train <- as.matrix(self$train_data)
      # convert Missings to NaN, otherwise they wil be transformed
      # wrong to python/ an error occurs
      x_train <- private$transform_features(x_train)

      x_label <- self$train_label
      private$label_names <- sort(unique(x_label))

      # python lightgbm$Dataset
      train_set <- private$lightgbm$Dataset(
        data = x_train,
        label = x_label
      )

      if (!is.null(self$valid_data)) {
        x_valid <- as.matrix(self$valid_data)
        x_valid <- private$transform_features(x_valid)

        x_label_valid <- self$valid_label

        # python lightgbm$Dataset
        valid_set <- private$lightgbm$Dataset(
          data = x_valid,
          label = x_label_valid
        )
      } else {
        valid_set <- train_set
      }


      # train lightgbm
      self$model <- private$lightgbm$train(
        params = self$parameters,
        train_set = train_set,
        num_boost_round = num_boost_round,
        valid_sets = valid_set,
        feval = feval,
        early_stopping_rounds = early_stopping_rounds,
        verbose_eval = verbose_eval
      )
    },

    #' @description The predict function.
    #'
    #' @details All arguments are passed to the predict function of the created
    #'   lightgbm python model by using R's reticulate package.
    #'
    #' @param newdata A data.table object holding the data which should be
    #'   predicted.
    #'
    predict = function(newdata) {
      stopifnot(
        data.table::is.data.table(newdata),
        !is.null(self$model)
      )

      pred_data <- newdata[, self$feature_names, with = F] # get newdata
      # transform it
      x_test <- as.matrix(pred_data)
      x_test <- private$transform_features(x_test)

      for (i in colnames(x_test)) {
        x_test[which(is.na(x_test[, i])), i] <- NaN
      }

      probs <- self$model$predict(
        data = x_test,
        is_reshape = TRUE
      )

      outlist <- list("probabilities" = probs)

      if (self$parameters[["objective"]] %in%
          c("multiclass", "multiclassova", "lambdarank")) {
        colnames(probs) <- as.character(unique(private$label_names))

        private$classes <- sapply(seq_len(nrow(probs)), function(x) {
          ret <- colnames(probs)[which(probs[x, ] ==
                                         max(probs[x, ]))]
          return(as.integer(ret))
        })
        outlist <- c(outlist, list("classes" = private$classes))

      } else if (self$parameters[["objective"]] == "binary") {
        private$classes <- as.integer(ifelse(probs > 0.5, 1, 0))
        outlist <- c(outlist, list("classes" = private$classes))
      }

      if (length(outlist) == 1) {
        outlist <- list("response" = outlist[[1]])
      }

      return(outlist)
    },

    #' @description Transform the target variable
    #'
    #' @details The function is used internally, to transform the target
    #'   variable to meet LightGBMs requirements. It can also be used,
    #'   to transform the yet untransformed target variable of a holdout
    #'   dataset.
    #'
    #' @param vector A vector containing targets.
    #'
    transform_target = function(vector) {

      # transform target to numeric for classification tasks
      if (self$parameters[["objective"]] %in%
          c("binary", "multiclass", "multiclassova", "lambdarank")) {

        # if target is not numeric
        if (!is.numeric(vector)) {

          vector <- (as.numeric(vector) - 1L)

          # if target is numeric and integer
        } else if (is.integer(vector) || is.numeric(vector)) {

          vector <- as.integer(vector)

          # check, if minimum != 0 --> if == 0, we have nothing to do
          if (min(vector) != 0) {

            # if min == 1, substract 1 --> lightgbm need the first class
            # to be 0
            if (min(vector) == 1) {
              vector <- (self$target_names - 1L)

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

        if (length(unique(vector)) > 2) {
          stopifnot(
            self$parameters[["objective"]] %in%
              c("multiclass", "multiclassova", "lambdarank")
          )
          self$parameters[["num_class"]] <- length(unique(vector))
        }

        # transform numeric variables
      } else {

        # we have only work here, if target is not numeric
        if (!is.numeric(vector)) {

          # try to transform to numeric
          transform_error <- tryCatch(
            expr = {
              vector <- as.numeric(
                as.character(vector)
              )
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

      return(vector)
    },

    # Add method for importance, if learner supports that.
    # It must return a sorted (decreasing) numerical, named vector.
    #' @description The importance function.
    #'
    #' @details Exports the model's variable importance.
    #'
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      # importance dataframe
      imp <- data.table::data.table(
        "Feature" = self$feature_names,
        "Value" = as.numeric(
          as.character(self$model$feature_importance())
        )
      )[order(get("Value"), decreasing = T)]

      if (nrow(imp) > 20) {
        imp <- imp[1:20, ]
      }

      # importance plot
      imp_plot <- ggplot2::ggplot(
        data = NULL,
        ggplot2::aes(x = reorder(imp$Feature, imp$Value),
                     y = imp$Value,
                     fill = imp$Value)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_continuous(type = "viridis") +
        ggplot2::labs(title = "LightGBM Feature Importance") +
        ggplot2::ylab("Feature") +
        ggplot2::xlab("Importance") +
        ggplot2::theme(legend.position = "none")


      return(
        list("raw_values" = imp,
             "plot" = imp_plot)
      )
    }
  )
)