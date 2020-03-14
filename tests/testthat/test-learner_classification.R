context("Test Classification")

test_that(
  desc = "Learner Classification",
  code = {

    library(mlbench)

    data("PimaIndiansDiabetes2")
    dataset <- data.table::as.data.table(PimaIndiansDiabetes2)

    target_col <- "diabetes"
    id_col <- NULL

    split <- sklearn_train_test_split(
      dataset,
      target_col,
      split = 0.7,
      seed = 17,
      return_only_index = TRUE,
      stratify = TRUE
    )

    lgb_learner <- LightGBM$new()

    lgb_learner$init_data(
      dataset = dataset[split$train_index, ],
      target_col = target_col,
      id_col = id_col
    )

    lgb_learner$param_set$values <- list(
      "objective" = "binary",
      "learning_rate" = 0.1,
      "seed" = 17,
      "metric" = "auc"
    )

    lgb_learner$positive <- "pos"
    lgb_learner$num_boost_round <- 10
    lgb_learner$early_stopping_rounds <- 10

    lgb_learner$train()

    expect_equal(lgb_learner$model$current_iteration(), 4L)
    expect_known_hash(lgb_learner$train_label, "f2d601cfa1")

    predictions <- lgb_learner$predict(newdata = dataset[split$test_index, ])

    expect_known_hash(predictions, "46c5e3fee9")

    imp <- lgb_learner$importance()
    expect_equal(imp$raw_values[1, get("Value")], 52.664776887985503606)
  }
)
