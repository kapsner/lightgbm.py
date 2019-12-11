#' @title Sklearn train_test_split wrapper
#'
#' @description A function to split a dataset into training and validation set,
#'   stratified by the target column.
#'
#' @param dataset A data.table object. The dataset used for training.
#' @param target_col A character string. The name of the target column.
#' @param split A numeric. Ratio to split `dataset` into training set and
#'   validation set (default: 0.7). The allowed value range is
#'   0 < validation_split < 1.
#' @param seed A integer (default: NULL). Please use this argument in
#'   order to generate reproducible results.
#' @param return_only_index A logical (default: FALSE). If FALSE,, the return
#'   value is a list containing the split data.tables. If TRUE, only the
#'   sampled row numbers are returned.
#'
#' @references \url{https://scikit-learn.org/stable/modules/generated/sklearn.
#'   model_selection.train_test_split.html}
#'
#' @export
#'
sklearn_train_test_split <- function(dataset,
                                     target_col,
                                     split,
                                     seed = NULL,
                                     return_only_index = FALSE) {
  stopifnot(
    reticulate::py_module_available("sklearn"),
    data.table::is.data.table(dataset),
    is.character(target_col),
    is.numeric(split),
    split > 0 && split < 1,
    is.numeric(seed) || is.null(seed),
    is.logical(return_only_index)
  )

  sklearn <- reticulate::import("sklearn.model_selection")

  # perform split
  splitlist <- sklearn$train_test_split(
    dataset[,-(target_col), with = F],
    dataset[, get(target_col)],
    stratify = dataset[, get(target_col)],
    train_size = 0.7,
    random_state = as.integer(seed)
  )

  if (return_only_index) {
    return(list(
      train_index = as.integer(rownames(splitlist[[1]])) + 1L,
      test_index = as.integer(rownames(splitlist[[2]])) + 1L
    ))
  } else {
    d_train <- data.table::data.table(
      cbind(splitlist[[3]], splitlist[[1]]), keep.rownames = F
    )
    colnames(d_train)[1] <- target_col

    d_test <- data.table::data.table(
      cbind(splitlist[[4]], splitlist[[2]]), keep.rownames = F
    )
    colnames(d_test)[1] <- target_col

    return(list(d_train = d_train, d_test = d_test))
  }
}



