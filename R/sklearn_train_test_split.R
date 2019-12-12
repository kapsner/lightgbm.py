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

  # import sklearn.model_selection
  sklearn <- reticulate::import("sklearn.model_selection")

  # transform to dataframe and add row-ids
  dframe <- as.data.frame(dataset[, -c(target_col), with = FALSE])
  dframe$lgb_split_row_id <- seq_len(nrow(dframe))

  # perform split
  splitlist <- sklearn$train_test_split(
    # use matrix here to avoid error with python conversion
    as.matrix(dframe), # x
    dataset[, get(target_col)], # y
    train_size = split,
    random_state = as.integer(seed),
    stratify = dataset[, get(target_col)]
  )

  # convert splitted data.frames to data.frames again
  # and reset colnames
  splitlist[[1]] <- as.data.frame(splitlist[[1]])
  colnames(splitlist[[1]]) <- colnames(dframe)
  splitlist[[2]] <- as.data.frame(splitlist[[2]])
  colnames(splitlist[[2]]) <- colnames(dframe)

  if (return_only_index) {
    return(list(
      train_index = splitlist[[1]]$lgb_split_row_id,
      test_index = splitlist[[2]]$lgb_split_row_id
    ))
  } else {
    # return everything as data.table
    # remove lgb_split_row_id from returend datasets
    d_train <- data.table::data.table(
      cbind(splitlist[[3]], splitlist[[1]][, which(
        colnames(splitlist[[1]]) %in%
          setdiff(colnames(dframe), "lgb_split_row_id")
      )]), keep.rownames = F
    )
    colnames(d_train)[1] <- target_col

    d_test <- data.table::data.table(
      cbind(splitlist[[4]], splitlist[[2]][, which(
        colnames(splitlist[[2]]) %in%
          setdiff(colnames(dframe), "lgb_split_row_id")
      )]), keep.rownames = F
    )
    colnames(d_test)[1] <- target_col

    return(list(d_train = d_train, d_test = d_test))
  }
}
