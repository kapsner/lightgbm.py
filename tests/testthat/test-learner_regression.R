context("Test Regression")

test_that(
  desc = "Learner Regression",
  code = {

    library(mlbench)
    data("BostonHousing2")
    dataset <- data.table::as.data.table(BostonHousing2)

    target_col <- "medv"
    id_col <- NULL

    split <- sklearn_train_test_split(
      dataset,
      target_col,
      split = 0.7,
      seed = 17,
      return_only_index = TRUE,
      stratify = FALSE
    )

    lgb_learner <- LightGBM$new()

    lgb_learner$init_data(
      dataset = dataset[split$train_index, ],
      target_col = target_col,
      id_col = id_col
    )

    lgb_learner$param_set$values <- list(
      "objective" = "regression",
      "learning_rate" = 0.1,
      "seed" = 17,
      "metric" = "rmse"
    )

    lgb_learner$num_boost_round <- 10
    lgb_learner$early_stopping_rounds <- 10

    lgb_learner$train()

    expect_equal(lgb_learner$model$current_iteration(), 10L)
    expect_known_hash(lgb_learner$train_label, "8bdc91bfc8")

    predictions <- lgb_learner$predict(newdata = dataset[split$test_index, ])

    expect_known_hash(predictions, "b33701e3d0")

    imp <- lgb_learner$importance()
    expect_equal(imp$raw_values[1, get("Value")], 99.993991103510424523)
  }
)
