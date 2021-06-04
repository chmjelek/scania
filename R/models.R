#' Support vector machine algorithm
#' @param data list of data.frames; containing the training and testing sets for the model
#' @param kernel chr; the kernel used in training and predicting
#' @param gamma float; parameter needed for all kernels except linear (default: 1/(data dimension))
#' @param cost float; cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation
#' @return list of parameters created by the SVM model
build_svm <- function(data, kernel, gamma, cost) {
  cat(paste0("Building SVM_", kernel, " model... "))

  svm <- e1071::svm(class ~ .,
    data = data[["train"]],
    kernel = kernel,
    gamma = gamma,
    cost = cost
  )

  pred <- predict(svm, data[["test"]])

  cm <- caret::confusionMatrix(pred, data[["test"]]$class)
  acc <- round(cm$overall[["Accuracy"]] * 100, 2)
  kappa <- round(cm$overall[["Kappa"]] * 100, 2)
  recall <- round(cm$byClass[["Recall"]] * 100, 2)
  precision <- round(cm$byClass[["Precision"]] * 100, 2)
  tn <- cm$table[1, 1]
  tp <- cm$table[2, 2]
  fn <- cm$table[2, 1]
  fp <- cm$table[1, 2]

  total_cost <- fp * 500 + fn * 10

  svm_model <- data.frame(
    name = paste0("SVM_", kernel),
    Accuracy = acc,
    Kappa = kappa,
    Recall = recall,
    Precision = precision,
    tn = tn,
    tp = tp,
    fn = fn,
    fp = fp,
    TotalCost = total_cost
  )

  cat("Done!\n\n")

  print(cm$table)

  cat("\nTotal cost: ", total_cost, "\n\n")

  return(svm_model)
}

#' Breiman's random forest algorithm
#' @param data list of data.frames; containing the training and testing sets for the model
#' @param ntrees int; number of trees to grow
#' @param mtry int; number of variables randomly sampled as candidates at each split
#' @param importance bool; should importance of predictors be assessed
#' @return list of parameters created by the RandomForest model
build_rf <- function(data, ntrees, mtry, importance) {
  cat("Building RandomForest model... ")

  rf <- randomForest::randomForest(class ~ .,
    data = data[["train"]],
    ntree = ntrees,
    mtry = mtry,
    importance = importance
  )
  pred_rf <- predict(rf, data[["test"]], type = "class")
  cm <- caret::confusionMatrix(as.factor(pred_rf), as.factor(data[["test"]]$class))

  acc <- round(cm$overall[["Accuracy"]] * 100, 2)
  kappa <- round(cm$overall[["Kappa"]] * 100, 2)
  recall <- round(cm$byClass[["Recall"]] * 100, 2)
  precision <- round(cm$byClass[["Precision"]] * 100, 2)

  tn <- cm$table[1, 1]
  tp <- cm$table[2, 2]
  fn <- cm$table[2, 1]
  fp <- cm$table[1, 2]

  total_cost <- fp * 500 + fn * 10

  rf_model <- data.frame(
    name = "RandomForest",
    Accuracy = acc,
    Kappa = kappa,
    Recall = recall,
    Precision = precision,
    tn = tn,
    tp = tp,
    fn = fn,
    fp = fp,
    TotalCost = total_cost
  )

  cat("Done!\n\n")

  print(cm$table)

  cat("\nTotal cost: ", total_cost, "\n\n")

  return(rf_model)
}

#' XGBoost algorithm
#' @param data list of data.frames; containing the training and testing sets for the model
#' @param eta float; control the learning rate; scale the contribution of each tree by a factor of 0 < eta < 1
#' @param nrounds int; number of iterations;lower value for eta implies larger value
#' @param max_depth int; maximum depth of a tree
#' @param subsample float; subsample ratio of the training instance; 0.5 prevents overfitting
#' @param colsample_bytree float; subsample ratio of columns when constructing each tree
#' @param eval_metric chr; evaluation metrics for validation data
#' @param objective chr; specify the learning task and the corresponding learning objective
#' @importFrom dplyr %>%
#' @return list of parameters created by the XGBoost model
build_xgboost <- function(data, eta = 0.3, nrounds = 75, max_depth = 6, subsample = 0.5, colsample_bytree = 0.5, eval_metric = "error", objective = "binary:logistic") {
  cat("Building XGBoost model... ")

  train_label <- as.matrix(data[["train"]] %>% dplyr::select(c(class)))
  train <- as.matrix(data[["train"]] %>% dplyr::select(-c(class)))
  xgb_train <- xgboost::xgb.DMatrix(data = train, label = train_label)

  test_label <- as.matrix(data[["test"]] %>% dplyr::select(c(class)))
  test <- as.matrix(data[["test"]] %>% dplyr::select(-c(class)))

  xgb <- xgboost::xgboost(
    data = xgb_train,
    eta = eta,
    nrounds = nrounds,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    eval_metric = eval_metric,
    objective = objective
  )

  pred <- round(predict(xgb, test))

  cm <- caret::confusionMatrix(factor(pred), factor(test_label))

  acc <- round(cm$overall[["Accuracy"]] * 100, 2)
  kappa <- round(cm$overall[["Kappa"]] * 100, 2)
  recall <- round(cm$byClass[["Recall"]] * 100, 2)
  precision <- round(cm$byClass[["Precision"]] * 100, 2)

  tn <- cm$table[1, 1]
  tp <- cm$table[2, 2]
  fn <- cm$table[2, 1]
  fp <- cm$table[1, 2]

  total_cost <- fp * 500 + fn * 10

  xgb_model <- data.frame(
    name = "XGBoost",
    Accuracy = acc,
    Kappa = kappa,
    Recall = recall,
    Precision = precision,
    tn = tn,
    tp = tp,
    fn = fn,
    fp = fp,
    TotalCost = total_cost
  )

  cat("Done!\n\n")

  print(cm$table)

  cat("\nTotal cost: ", total_cost, "\n\n")

  return(xgb_model)
}

#' Builds models
#' @param data list of 3; noises, train, test
#' @param gamma float; svm; parameter needed for all kernels except linear (default: 1/(data dimension))
#' @param cost float; svm; cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation
#' @param ntrees int; RandomForest; number of trees to grow
#' @param mtry int; RandomForest; number of variables randomly sampled as candidates at each split
#' @param importance bool; RandomForest; should importance of predictors be assessed
#' @param eta float; xgboost; control the learning rate; scale the contribution of each tree by a factor of 0 < eta < 1; by default 0.3
#' @param nrounds int; xgboost; number of iterations;lower value for eta implies larger value; by default 75
#' @param max_depth int; xgboost; maximum depth of a tree; by default 6
#' @param subsample float; xgboost; subsample ratio of the training instance; 0.5 prevents overfitting; by default 0.5
#' @param colsample_bytree float; xgboost; subsample ratio of columns when constructing each tree; by default 0.5
#' @param eval_metric chr; xgboost; evaluation metrics for validation data; by default "error"
#' @param objective chr; xgboost; specify the learning task and the corresponding learning objective; by default "binary:logistic"
#' @return data.frame; containing models
#' @export
building_models <- function(data,
                            gamma = 0.1,
                            cost = 0.1,
                            ntrees = 50,
                            mtry = 5,
                            importance = TRUE,
                            ...) {
  svm_lin <- build_svm(
    data = data,
    kernel = "linear",
    gamma = gamma,
    cost = cost
  )

  svm_rad <- build_svm(
    data = data,
    kernel = "radial",
    gamma = gamma,
    cost = cost
  )

  svm_pol <- build_svm(
    data = data,
    kernel = "polynomial",
    gamma = gamma,
    cost = cost
  )

  rf <- build_rf(
    data = data,
    ntrees = ntrees,
    mtry = mtry,
    importance = importance
  )

  xgb <- build_xgboost(
    data = data,
    ...
  )

  models <- dplyr::bind_rows(svm_lin, svm_rad, svm_pol, rf, xgb)
}
