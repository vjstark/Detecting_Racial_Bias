# This file contains code for using some of the logistic regression models for
# predictions.
# Code based on https://www.shirin-glander.de/2018/01/plumber/

# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list_of_packages <- c("tidyverse", "rjson")
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  print("Installing packages\n")
  install.packages(new.packages())
}

library(tidyverse)
library(rjson)


# Global variable initialization
base_dir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
model_path <- paste(base_dir, "/train/2014/models", sep = "")
data_dir <- paste(base_dir, "/train/2014/data", sep = "")

# Available models.
model_income_loan_amount <- paste(model_path, "/model_income_loan_amount.rdata", sep = "")
model_log_income_loan_amount <- paste(model_path, "/model_log_income_loan_amount.rdata", sep = "")
model_ratio_of_loan_to_income_with_log_of_income_and_amount <- paste(model_path, "/model_loan_income_ratio_with_log_of_income_and_amount.rdata", sep = "")
tuned_param_model_loan_to_income_ratio <- paste(model_path, "/tuned_param_model_loan_to_income_ratio.rdata", sep = "")

# End of global variables

# Load test and training data.

load(paste(data_dir, "/train.rdata", sep = ""))
load(paste(data_dir, "/test.rdata", sep = ""))

# Load all the models.
load(model_income_loan_amount)
load(model_log_income_loan_amount)
load(model_ratio_of_loan_to_income_with_log_of_income_and_amount)
load(tuned_param_model_loan_to_income_ratio)

var_names <- all.vars(formula(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount)[-2])
var_names

# show parameter definition for features.
for (i in 1:length(var_names)) {
  var <- var_names[i]
  train_data_subs <- train[, which(colnames(train) == var)]
  type <- class(train_data_subs)
  
  if (type == "numeric" | type == "integer") {
    min <- min(train_data_subs)
    max <- max(train_data_subs)
    
    cat("Variable:", var, "is of type:", type, "\n",
        "Min value in training data =", min, "\n",
        "Max value in training data =", max, "\n----------\n")
  } else if (type == "factor") {
    cat("Variable:", var, "is of type:", type, "\n",
        "Levels in training data = ", levels(train_data_subs), "\n",
        "\n----------\n")
  } else if (type == "character") {
    cat("Variable:", var, "is of type:", type, "\n")
  }
}

# predict test case using model_income_loan_amount
pred <- predict(model_2014_income_loan_amount, test[1, ], type = "response")
cat("----------------\nTest case 1 predicted to be", as.character(pred), "\n----------------")

# predict test case using model_income_loan_amount
pred <- predict(model_2014_log_income_loan_amount, test[1, ], type = "response")
cat("----------------\nTest case predicted to be", as.character(pred), "\n----------------")

pred <- predict(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount, test[1, ], type = "response")
cat("----------------\nTest case predicted to be", as.character(pred), "\n----------------")

african_american_index <- which(test$applicant_race_and_ethnicity == "Black or African American")
white_index <- which(test$applicant_race_and_ethnicity == "White")
hispanic_index <- which(test$applicant_race_and_ethnicity == "Hispanic or Latino")
asian_index <- which(test$applicant_race_and_ethnicity == "Asian")

pred <- predict(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount, test[african_american_index[1], ], type = "response")
cat("----------------\nAfrican American test case predicted to be", as.character(pred), "\n----------------")

pred <- predict(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount, test[white_index[1], ], type = "response")
cat("----------------\nWhite test case predicted to be", as.character(pred), "\n----------------")

pred <- predict(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount, test[hispanic_index[1], ], type = "response")
cat("----------------\nHispanic test case predicted to be", as.character(pred), "\n----------------")

pred <- predict(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount, test[asian_index[1], ], type = "response")
cat("----------------\nAsian test case predicted to be", as.character(pred), "\n----------------")

test_case_json <- toJSON(test[white_index[1], ])
cat(test_case_json)

test_case_json <- toJSON(test[african_american_index[1], ])
cat(test_case_json)

pred <- predict(tuned_param_model_loan_to_income_ratio, test[asian_index[1], ], type = "prob")
cat("----------------\nAsian test case predicted to be", as.character(pred[,2]), "\n----------------")

pred <- predict(tuned_param_model_loan_to_income_ratio, test[white_index[1], ], type = "prob")
cat("----------------\nWhite test case predicted to be", as.character(pred[,2]), "\n----------------")

pred <- predict(tuned_param_model_loan_to_income_ratio, test[african_american_index[1], ], type = "prob")
cat("----------------\nAfrican American test case predicted to be", as.character(pred[,2]), "\n----------------")

