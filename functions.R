
# Functions

# 1. Data ----

get_sample_submission<- function(){
  readr::read_csv("data/sample_submission.csv") 
}

get_train_data <- function(type = "feather"){
  if(type == "feather") data <- feather::read_feather("data/train.feather")
  if(type == "csv") data <- readr::read_csv("data/test.csv")
  return(data)
}

get_test_data <- function(type = "feather"){
  if(type == "feather") data <- feather::read_feather("data/test.feather")
  if(type == "csv") data <- readr::read_csv("data/train.csv")
  return(data)
}

# Measures ----
ROC_AUC_metric <- function(rocr_prediction_pred_object){
  # 'Docstring': This function returns the 'Area Under Curve' value (ROC AUC)
  auc_rocr <- ROCR::performance(rocr_prediction_pred_object, measure = "auc")
  auc_rocr <- auc_rocr@y.values[[1]]
  return(auc_rocr)
}

ROC_AUC_plot <- function(rocr_prediction_pred_object){
  # 'Docstring': This function returns ROC Curve (on which the AUC is calculated)
  perf <- ROCR::performance(rocr_prediction_pred_object, "tpr", "fpr")
  roc <- tibble(
    x = unlist(perf@x.values),
    y = unlist(perf@y.values)
  )
  roc_plot <- ggplot(roc, aes(x, y)) + 
    geom_line() +
    # geom_abline(intercept = 0, slope = 1)
    labs(
      title = "ROC Curve",
      x = perf@x.name,
      y = perf@y.name
    )
  return(roc_plot)
}






