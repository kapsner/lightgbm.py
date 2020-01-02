#' @title R6 LightGBM function
#'
#' @description A wrapper around the lightgbm python api.
#'
#' @references \url{https://lightgbm.readthedocs.io}
#'
#'
#' @import data.table
#' @import paradox
#'
#' @export
#% @seealso \code{\link{rstudio/reticulate}}
LightGBM <- R6::R6Class(
  classname = "LightGBM",

  private = list(
    # define private objects

    lightgbm = NULL,

    # data: train, valid, test
    train_input = NULL,
    valid_input = NULL,
    test_input = NULL,
    input_rules = NULL,

    # the list, passed to the train function
    valid_list = NULL,

    # save importance values
    imp = NULL,

    dataset = NULL,
    feature_names = NULL,
    target_names = NULL,

    # convert object types
    # this is necessary, since mlr3 tuning does pass wrong types
    convert_types = function() {
      self$num_boost_round <- as.integer(self$num_boost_round)
      self$early_stopping_rounds <- as.integer(self$early_stopping_rounds)
      self$cv_folds <- as.integer(self$cv_folds)

      if (is.null(self$categorical_feature)) {
        self$categorical_feature <- "auto"
      } else {
        stopifnot(
          is.list(self$categorical_feature)
        )
      }

      # set correct types for parameters
      for (param in names(self$param_set$values)) {
        value <- self$param_set$values[[param]]
        if (self$param_set$class[[param]] == "ParamInt") {
          self$param_set$values[[param]] <- as.integer(round(value))
        } else if (self$param_set$class[[param]] == "ParamDbl") {
          self$param_set$values[[param]] <- as.numeric(value)
        }
      }
    },

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
    },

    #' @description The data_preprocessing function.
    #'
    data_preprocessing = function(data) {

      stopifnot(
        !is.null(self$param_set$values[["objective"]])
      )

      # create training label
      self$train_label <- self$trans_tar$transform_target(
        vector = data[, get(private$target_names)],
        mapping = "dtrain"
      )

      # some further special treatments, when we have a classification task
      if (self$param_set$values[["objective"]] %in%
          c("binary", "multiclass", "multiclassova", "lambdarank")) {
        # store the class label names
        self$label_names <- sort(unique(self$train_label))

        # if a validation set is provided, check if value mappings are
        # identical
        if (!is.null(self$valid_data)) {
          stopifnot(
            identical(
              self$trans_tar$value_mapping_dtrain,
              self$trans_tar$value_mapping_dvalid
            )
          )
        }

        # extract classification classes and set num_class
        if (length(self$label_names) > 2) {
          stopifnot(
            self$param_set$values[["objective"]] %in%
              c("multiclass", "multiclassova", "lambdarank")
          )
          self$param_set$values[["num_class"]] <- length(self$label_names)
        }
      }

      # create lgb.Datasets
      x_train <- as.matrix(data[, private$feature_names, with = F])
      # convert Missings to NaN, otherwise they wil be transformed
      # wrong to python/ an error occurs
      x_train <- private$transform_features(x_train)
      self$train_data <- private$lightgbm$Dataset(
        data = x_train,
        label = self$train_label,
        feature_name = private$feature_names,
        free_raw_data = FALSE,
        reference = private$input_rules
      )
      if (is.null(private$input_rules)) {
        private$input_rules <- self$train_data
      }

      # add to training data to validation set:
      if (is.null(private$valid_list)) {
        private$valid_list <- list(self$train_data)
      } else {
        private$valid_list <- c(
          private$valid_list,
          list(self$train_data)
        )
      }
    }
  ),

  public = list(

    # define public objects
    #' @field num_boost_round Number of training rounds.
    num_boost_round = NULL,

    #' @field early_stopping_rounds A integer. Activates early stopping.
    #'   Requires at least one validation data and one metric. If there's
    #'   more than one, will check all of them except the training data.
    #'   Returns the model with (best_iter + early_stopping_rounds).
    #'   If early stopping occurs, the model will have 'best_iter' field.
    early_stopping_rounds = NULL,

    #' @field categorical_feature A list of str or int. Type int represents
    #'   index, type str represents feature names.
    categorical_feature = NULL,

    #' @field train_data A data.table object holding the training data.
    train_data = NULL,
    #' @field train_label A vector holding the training labels.
    train_label = NULL,

    #' @field valid_data A data.table object holding the validation data.
    valid_data = NULL,
    #' @field valid_label A vector holding the validation labels.
    valid_label = NULL,

    #' @field label_names The unique label names in classification tasks.
    label_names = NULL,

    #' @field trans_tar The transfrom_target instance.
    trans_tar = NULL,

    #' @field param_set The lightgbm parameters.
    param_set = NULL,

    #' @field nrounds_by_cv A logical. Calculate the best nrounds by using
    #'   the `lgb.cv` before the training step
    nrounds_by_cv = NULL,

    #' @field cv_folds The number of cross validation folds, when setting
    #'   `nrounds_by_cv` = TRUE (default: 5).
    cv_folds = NULL,

    #' @field cv_model The cross validation model.
    cv_model = NULL,

    #' @field model The trained lightgbm model.
    model = NULL,

    # define methods
    #' @description The initialize function.
    #'
    initialize = function() {

      stopifnot(
        reticulate::py_module_available("lightgbm")
      )

      self$num_boost_round <- 10L

      self$cv_folds <- 5L

      self$nrounds_by_cv <- TRUE

      # initialize parameter set
      self$param_set <- lgbparams()

      # load python module
      private$lightgbm <- reticulate::import("lightgbm", delay_load = TRUE)

      self$trans_tar <- TransformTarget$new(
        param_set = self$param_set
      )

    },

    #' @description Initialize dataset function.
    #' @param dataset A data.table object. The dataset used for training.
    #' @param target_col A character string. The name of the target column.
    #' @param id_col (optional) A character string. The name of the ID column
    #'   (default: NULL).
    #'
    init_data = function(dataset, target_col, id_col = NULL) {

      stopifnot(
        data.table::is.data.table(dataset),
        is.character(target_col),
        is.character(id_col) || is.null(id_col),
        target_col %in% colnames(dataset)
      )

      private$dataset <- dataset
      private$target_names <- target_col
      private$feature_names <- setdiff(
        c(colnames(private$dataset), id_col),
        private$target_names
      )
    },

    #' @description The train_cv function
    #'
    train_cv = function() {
      message(
        sprintf(
          paste0("Optimizing num_boost_round with %s fold CV."),
          self$cv_folds
        )
      )

      private$data_preprocessing(private$dataset)

      private$convert_types()

      # set stratified
      if (self$param_set$values[["objective"]] %in%
          c("binary", "multiclass", "multiclassova", "lambdarank")) {
        stratified <- TRUE
      } else {
        stratified <- FALSE
      }

      self$cv_model <- private$lightgbm$cv(
        params = self$param_set$values,
        train_set = self$train_data,
        num_boost_round = self$num_boost_round,
        nfold = self$cv_folds,
        categorical_feature = self$categorical_feature,
        verbose_eval = 10L,
        early_stopping_rounds = self$early_stopping_rounds,
        stratified = stratified
      )

      helper_cv_names <- names(self$cv_model)
      cv_mean_name <- helper_cv_names[grepl("mean", helper_cv_names)]
      best_iter <- length(self$cv_model[[cv_mean_name]])
      message(
        sprintf(
          paste0("CV results: best iter %s; best score: %s"),
          best_iter,
          self$cv_model[[cv_mean_name]][[best_iter]][[1]]
        )
      )
      # set nrounds to best iteration from cv-model
      self$num_boost_round <- as.integer(best_iter)
      # if we already have figured out the best nrounds, which are provided
      # to the train function, we don't need early stopping anymore
      self$early_stopping_rounds <- NULL
    },

    #' @description The train function
    #'
    train = function() {
      if (self$nrounds_by_cv) {
        self$train_cv()
      } else if (isFALSE(self$nrounds_by_cv)) {
        private$data_preprocessing(private$dataset)
        private$convert_types()
      }

      self$model <- private$lightgbm$train(
        params = self$param_set$values,
        train_set = self$train_data,
        num_boost_round = self$num_boost_round,
        valid_sets = private$valid_list,
        categorical_feature = self$categorical_feature,
        verbose_eval = 10L,
        early_stopping_rounds = self$early_stopping_rounds
      )
      message(
        sprintf("Final model: current iter: %s",
                self$model$current_iteration())
      )
      return(self$model)
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

      newdata <- newdata[, private$feature_names, with = F] # get newdata

      x_test <- as.matrix(newdata)
      # convert Missings to NaN, otherwise they wil be transformed
      # wrong to python/ an error occurs
      x_test <- private$transform_features(x_test)

      # create lgb.Datasets
      private$test_input <- private$lightgbm$Dataset(
        data = x_test,
        reference = private$input_rules
      )

      test_data <- as.matrix(private$test_input$data)

      p <- self$model$predict(
        data = test_data,
        is_reshape = TRUE
      )

      return(p)
    },

    # Add method for importance, if learner supports that.
    # It must return a sorted (decreasing) numerical, named vector.
    #' @description The importance function.
    #'
    #' @param view_max An integer. The maximum number of features to be
    #'   shown in the importance plot (default: 20).
    #'
    #' @details Exports the model's variable importance.
    #'
    importance = function(view_max = 20) {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      # importance dataframe
      imp <- data.table::data.table(
        "Feature" = private$feature_names,
        "Value" = as.numeric(
          as.character(self$model$feature_importance())
        )
      )[order(get("Value"), decreasing = T)]

      if (nrow(imp) > view_max) {
        imp <- imp[1:view_max, ]
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
    },

    #' @description The valids function
    #'
    #' @details The function can be used to provide a subsample to the data
    #'   to the lightgbm's train function's `valids` argument. This is e.g.
    #'   needed, when the argument `early_stopping_rounds` is used.
    #'
    #' @param validset A data.table object, containing the validation data.
    #'
    valids = function(validset) {

      if (!is.null(private$target_names) &&
          !is.null(private$feature_names)) {

        # create label
        self$valid_label <- self$trans_tar$transform_target(
          vector = validset[, get(private$target_names)],
          mapping = "dvalid"
        )

        # create lgb.Datasets
        x_valid <- as.matrix(validset[, private$feature_names, with = F])
        # convert Missings to NaN, otherwise they wil be transformed
        # wrong to python/ an error occurs
        x_valid <- private$transform_features(x_valid)
        self$valid_data <- private$lightgbm$Dataset(
          data = x_valid,
          label = self$valid_label,
          feature_name = private$feature_names,
          free_raw_data = FALSE
        )

        private$input_rules <- self$valid_data

        private$valid_list <- list(self$valid_data)
      } else {
        stop("Please initialize the training dataset first.")
      }
    }
  )
)
