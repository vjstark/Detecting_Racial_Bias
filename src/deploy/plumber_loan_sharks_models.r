# script name:
# plumber_loan_sharks_models.r

# Code based on https://www.shirin-glander.de/2018/01/plumber/

## Install required packages.

# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list_of_packages <- c("tidyverse", "plumber")
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  print("Installing packages\n")
  install.packages(new.packages())
}

library(tidyverse)
library(plumber)

# set API title and description to show up in http://localhost:8000/__swagger__/
#' @apiTitle predict the probability of an applicant getting approved for a home loan with the loan sharks models.
#' @apiDescription This API takes home loan applicant data and returns a prediction whether the loan will be approved.
#' Indicates that the loan will be approved or not.

# Global variable initialization
base_dir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
model_2014_path <- paste(base_dir, "/train/2014/models", sep = "")
data_2014_dir <- paste(base_dir, "/train/2014/data", sep = "")
model_2015_path <- paste(base_dir, "/train/2015/models", sep = "")
data_2015_dir <- paste(base_dir, "/train/2015/data", sep = "")
model_2016_path <- paste(base_dir, "/train/2016/models", sep = "")
data_2016_dir <- paste(base_dir, "/train/2016/data", sep = "")
model_2017_path <- paste(base_dir, "/train/2017/models", sep = "")
data_2017_dir <- paste(base_dir, "/train/2017/data", sep = "")

# Available models.
model_2014_income_loan_amount <- paste(model_2014_path, "/model_income_loan_amount.rdata", sep = "")
model_2014_log_income_loan_amount <- paste(model_2014_path, "/model_log_income_loan_amount.rdata", sep = "")
model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount <- paste(model_2014_path, "/model_loan_income_ratio_with_log_of_income_and_amount.rdata", sep = "")
model_2014_il_ratio_of_loan_to_income_with_log_of_income_and_amount <- paste(model_2014_path, "/model_il_loan_income_ratio_with_log_of_income_and_amount.rdata", sep = "")

model_2015_income_loan_amount <- paste(model_2015_path, "/model_income_loan_amount.rdata", sep = "")
model_2015_log_income_loan_amount <- paste(model_2015_path, "/model_log_income_loan_amount.rdata", sep = "")
model_2015_ratio_of_loan_to_income_with_log_of_income_and_amount <- paste(model_2015_path, "/model_loan_income_ratio_with_log_of_income_and_amount.rdata", sep = "")

model_2016_income_loan_amount <- paste(model_2016_path, "/model_income_loan_amount.rdata", sep = "")
model_2016_log_income_loan_amount <- paste(model_2016_path, "/model_log_income_loan_amount.rdata", sep = "")
model_2016_ratio_of_loan_to_income_with_log_of_income_and_amount <- paste(model_2016_path, "/model_loan_income_ratio_with_log_of_income_and_amount.rdata", sep = "")

model_2017_income_loan_amount <- paste(model_2017_path, "/model_income_loan_amount.rdata", sep = "")
model_2017_log_income_loan_amount <- paste(model_2017_path, "/model_log_income_loan_amount.rdata", sep = "")
model_2017_ratio_of_loan_to_income_with_log_of_income_and_amount <- paste(model_2017_path, "/model_loan_income_ratio_with_log_of_income_and_amount.rdata", sep = "")

tuned_param_model_loan_to_income_ratio <- paste(model_2014_path, "/tuned_param_model_loan_to_income_ratio.rdata", sep = "")

# End of global variables


# Load test and training data.

load(paste(data_2014_dir, "/train.rdata", sep = ""))
load(paste(data_2014_dir, "/test.rdata", sep = ""))

load(paste(data_2015_dir, "/train.rdata", sep = ""))
load(paste(data_2015_dir, "/test.rdata", sep = ""))

# Load all the models.
load(model_2014_income_loan_amount)
load(model_2014_log_income_loan_amount)
load(model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount)
load(tuned_param_model_loan_to_income_ratio)
load(model_2014_il_ratio_of_loan_to_income_with_log_of_income_and_amount)

load(model_2015_income_loan_amount)
load(model_2015_log_income_loan_amount)
load(model_2015_ratio_of_loan_to_income_with_log_of_income_and_amount)

load(model_2016_income_loan_amount)
load(model_2016_log_income_loan_amount)
load(model_2016_ratio_of_loan_to_income_with_log_of_income_and_amount)

load(model_2017_income_loan_amount)
load(model_2017_log_income_loan_amount)
load(model_2017_ratio_of_loan_to_income_with_log_of_income_and_amount)


#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req) {
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# Helper function to return predictions for our models.
# based on the various predictors specified as input. The |model| parameter
# identifies the glm model being used for evaluation. For rest of the parameter
# docs please refer to one of the APIs below.
predict_helper <- function(model,
                           applicant_race_and_ethnicity,
                           applicant_income_000s,
                           owner_occupancy,
                           preapproval,
                           property_type,
                           loan_amount_000s,
                           applicant_sex,
                           county_code,
                           tract_to_msamd_income,
                           co_applicant_present,
                           agency_code,
                           minority_population,
                           rate_spread,
                           loan_purpose,
                           respondent_id,
                           type = "response") {
  applicant_race_and_ethnicity <- as.factor(applicant_race_and_ethnicity)
  applicant_income_000s <- as.integer(applicant_income_000s)
  owner_occupancy <- as.factor(owner_occupancy)
  preapproval <- as.factor(preapproval)
  property_type <- as.factor(property_type)
  loan_amount_000s <- as.integer(loan_amount_000s)
  applicant_sex <- as.factor(applicant_sex)
  county_code <- as.factor(county_code)
  tract_to_msamd_income <- as.numeric(tract_to_msamd_income)
  co_applicant_present <- as.factor(co_applicant_present)
  agency_code <- as.factor(agency_code)
  minority_population <- as.numeric(minority_population)
  loan_purpose <- as.factor(loan_purpose)
  respondent_id <- as.factor(respondent_id)
  rate_spread <- as.numeric(rate_spread)
  
  log_loan_amount_000s <- log(loan_amount_000s)
  log_applicant_income_000s <- log(applicant_income_000s)
  loan_to_income_ratio <- loan_amount_000s / applicant_income_000s
  
  input_data <- data.frame(applicant_race_and_ethnicity,
                           applicant_income_000s,
                           owner_occupancy,
                           preapproval,
                           property_type,
                           loan_amount_000s,
                           applicant_sex,
                           co_applicant_present,
                           county_code,
                           tract_to_msamd_income,
                           agency_code,
                           minority_population,
                           log_loan_amount_000s,
                           log_applicant_income_000s,
                           loan_to_income_ratio,
                           respondent_id,
                           loan_purpose,
                           rate_spread)
  # validation for parameter
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric/integers/factors"
  }
  
  pred <<- predict(model, input_data, type)
  return (pred)
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return


#' Model adding loan to income ratio and log of income and loan amount. PA 2014 dataset
#' predict the probability of an applicant getting approved for a home loan with the logistic regression model
#' @param applicant_race_and_ethnicity:character The race of the applicant. Allowed values include Black or African American, White, Asian, Hispanic or Latino, etc.
#' @param applicant_income_000s:int The gross annual income of the applicant rounded to the nearest thousand.
#' @param owner_occupancy:int A code representing owner-occupancy status of the property. Second homes, vacation homes, etc. Allowed values are 1, 2, 3
#' @param preapproval:int A code representing preapproval status of the applicant. Allowed values 1, 2, 3.
#' @param property_type:int A code representing the type of the property.
#' @param applicant_sex:int A code representing the sex of the applicant. Allowed values are 1, 2, 3, 4
#' @param county_code:int A three digit code representing the county of the property. 
#' @param tract_to_msamd_income:numeric The percentage of the median family income for the tract compared to the median family income for the MSA/MD, rounded to two decimal places.
#' @param co_applicant_present:int A boolean value indicating whether a coapplicant was present in the loan application. Allowed values are 1 and 0.
#' @param agency_code:int The federal regulatory agency.
#' @param minority_population:numeric The percentage of minority population to total population for the census tract, carried to two decimal places
#' @param rate_spread:numeric The rate spread for the loan, which is the difference between the loan's annual percentage rate (APR) and the average prime offer rate (APOR).
#' @param respondent_id A code representing the bank or other financial institution that is reporting the loan or application.
#' @param loan_amount_000s:int The amount of the loan applied for, in thousands of dollars.
#' @param loan_purpose:int The loan purpose. Allowed values are 1, 2 and 3.
#' @get /predict_with_ratio_of_incomes_log_incomes_2014_model
#' @html
#' @response 200 Returns the probability of the applicant getting a loan or not.
predict_loan_decision_with_log_incomes_2014 <- function(applicant_race_and_ethnicity,
                                                        applicant_income_000s,
                                                        owner_occupancy,
                                                        preapproval,
                                                        property_type,
                                                        loan_amount_000s,
                                                        applicant_sex,
                                                        county_code,
                                                        tract_to_msamd_income,
                                                        co_applicant_present,
                                                        agency_code,
                                                        minority_population,
                                                        rate_spread,
                                                        loan_purpose,
                                                        respondent_id) {
  pred <- predict_helper(
    model_2014_ratio_of_loan_to_income_with_log_of_income_and_amount,
    applicant_race_and_ethnicity, applicant_income_000s, owner_occupancy, preapproval,
    property_type, loan_amount_000s, applicant_sex, county_code, tract_to_msamd_income,
    co_applicant_present, agency_code, minority_population, rate_spread, loan_purpose,
    respondent_id)

  paste("----------------\nTest case predicted to be", as.character(pred), "\n----------------\n")
}

#' Model adding loan to income ratio and log of income and loan amount. PA 2014 dataset
#' predict the probability of an applicant getting approved for a home loan with the decision tree model
#' @param applicant_race_and_ethnicity:character The race of the applicant. Allowed values include Black or African American, White, Asian, Hispanic or Latino, etc.
#' @param applicant_income_000s:int The gross annual income of the applicant rounded to the nearest thousand.
#' @param owner_occupancy:int A code representing owner-occupancy status of the property. Second homes, vacation homes, etc. Allowed values are 1, 2, 3
#' @param preapproval:int A code representing preapproval status of the applicant. Allowed values 1, 2, 3.
#' @param property_type:int A code representing the type of the property.
#' @param applicant_sex:int A code representing the sex of the applicant. Allowed values are 1, 2, 3, 4
#' @param county_code:int A three digit code representing the county of the property. 
#' @param tract_to_msamd_income:numeric The percentage of the median family income for the tract compared to the median family income for the MSA/MD, rounded to two decimal places.
#' @param co_applicant_present:int A boolean value indicating whether a coapplicant was present in the loan application. Allowed values are 1 and 0.
#' @param agency_code:int The federal regulatory agency.
#' @param minority_population:numeric The percentage of minority population to total population for the census tract, carried to two decimal places
#' @param rate_spread:numeric The rate spread for the loan, which is the difference between the loan's annual percentage rate (APR) and the average prime offer rate (APOR).
#' @param respondent_id A code representing the bank or other financial institution that is reporting the loan or application.
#' @param loan_amount_000s:int The amount of the loan applied for, in thousands of dollars.
#' @param loan_purpose:int The loan purpose. Allowed values are 1, 2 and 3.
#' @get /predict_with_decision_tree_2014_model
#' @html
#' @response 200 Returns the probability of the applicant getting a loan or not.
predict_loan_decision_tree_pa_2014 <- function(applicant_race_and_ethnicity,
                                               applicant_income_000s,
                                               owner_occupancy,
                                               preapproval,
                                               property_type,
                                               loan_amount_000s,
                                               applicant_sex,
                                               county_code,
                                               tract_to_msamd_income,
                                               co_applicant_present,
                                               agency_code,
                                               minority_population,
                                               rate_spread,
                                               loan_purpose,
                                               respondent_id) {
  pred <- predict_helper(
    tuned_param_model_loan_to_income_ratio,
    applicant_race_and_ethnicity, applicant_income_000s, owner_occupancy, preapproval,
    property_type, loan_amount_000s, applicant_sex, county_code, tract_to_msamd_income,
    co_applicant_present, agency_code, minority_population, rate_spread, loan_purpose,
    respondent_id, type = "prob")
  
  paste("----------------\nTest case predicted to be", as.character(pred[, 2]), "\n----------------\n")
}

#' Model adding loan to income ratio and log of income and loan amount. PA 2015 dataset
#' predict the probability of an applicant getting approved for a home loan with the logistic regression model
#' @param applicant_race_and_ethnicity:character The race of the applicant. Allowed values include Black or African American, White, Asian, Hispanic or Latino, etc.
#' @param applicant_income_000s:int The gross annual income of the applicant rounded to the nearest thousand.
#' @param owner_occupancy:int A code representing owner-occupancy status of the property. Second homes, vacation homes, etc. Allowed values are 1, 2, 3
#' @param preapproval:int A code representing preapproval status of the applicant. Allowed values 1, 2, 3.
#' @param property_type:int A code representing the type of the property.
#' @param applicant_sex:int A code representing the sex of the applicant. Allowed values are 1, 2, 3, 4
#' @param county_code:int A three digit code representing the county of the property. 
#' @param tract_to_msamd_income:numeric The percentage of the median family income for the tract compared to the median family income for the MSA/MD, rounded to two decimal places.
#' @param co_applicant_present:int A boolean value indicating whether a coapplicant was present in the loan application. Allowed values are 1 and 0.
#' @param agency_code:int The federal regulatory agency.
#' @param minority_population:numeric The percentage of minority population to total population for the census tract, carried to two decimal places
#' @param rate_spread:numeric The rate spread for the loan, which is the difference between the loan's annual percentage rate (APR) and the average prime offer rate (APOR).
#' @param respondent_id A code representing the bank or other financial institution that is reporting the loan or application.
#' @param loan_amount_000s:int The amount of the loan applied for, in thousands of dollars.
#' @param loan_purpose:int The loan purpose. Allowed values are 1, 2 and 3.
#' @get /predict_with_ratio_of_incomes_log_incomes_2015_model
#' @html
#' @response 200 Returns the probability of the applicant getting a loan or not.
predict_loan_decision_with_log_incomes_2015 <- function(applicant_race_and_ethnicity,
                                                        applicant_income_000s,
                                                        owner_occupancy,
                                                        preapproval,
                                                        property_type,
                                                        loan_amount_000s,
                                                        applicant_sex,
                                                        county_code,
                                                        tract_to_msamd_income,
                                                        co_applicant_present,
                                                        agency_code,
                                                        minority_population,
                                                        rate_spread,
                                                        loan_purpose,
                                                        respondent_id) {
  pred <- predict_helper(
    model_2015_ratio_of_loan_to_income_with_log_of_income_and_amount,
    applicant_race_and_ethnicity, applicant_income_000s, owner_occupancy, preapproval,
    property_type, loan_amount_000s, applicant_sex, county_code, tract_to_msamd_income,
    co_applicant_present, agency_code, minority_population, rate_spread, loan_purpose,
    respondent_id)
  
  paste("----------------\nTest case predicted to be", as.character(pred), "\n----------------\n")
}

#' Model adding loan to income ratio and log of income and loan amount. PA 2016 dataset
#' predict the probability of an applicant getting approved for a home loan with the logistic regression model
#' @param applicant_race_and_ethnicity:character The race of the applicant. Allowed values include Black or African American, White, Asian, Hispanic or Latino, etc.
#' @param applicant_income_000s:int The gross annual income of the applicant rounded to the nearest thousand.
#' @param owner_occupancy:int A code representing owner-occupancy status of the property. Second homes, vacation homes, etc. Allowed values are 1, 2, 3
#' @param preapproval:int A code representing preapproval status of the applicant. Allowed values 1, 2, 3.
#' @param property_type:int A code representing the type of the property.
#' @param applicant_sex:int A code representing the sex of the applicant. Allowed values are 1, 2, 3, 4
#' @param county_code:int A three digit code representing the county of the property. 
#' @param tract_to_msamd_income:numeric The percentage of the median family income for the tract compared to the median family income for the MSA/MD, rounded to two decimal places.
#' @param co_applicant_present:int A boolean value indicating whether a coapplicant was present in the loan application. Allowed values are 1 and 0.
#' @param agency_code:int The federal regulatory agency.
#' @param minority_population:numeric The percentage of minority population to total population for the census tract, carried to two decimal places
#' @param rate_spread:numeric The rate spread for the loan, which is the difference between the loan's annual percentage rate (APR) and the average prime offer rate (APOR).
#' @param respondent_id A code representing the bank or other financial institution that is reporting the loan or application.
#' @param loan_amount_000s:int The amount of the loan applied for, in thousands of dollars.
#' @param loan_purpose:int The loan purpose. Allowed values are 1, 2 and 3.
#' @get /predict_with_ratio_of_incomes_log_incomes_2016_model
#' @html
#' @response 200 Returns the probability of the applicant getting a loan or not.
predict_loan_decision_with_log_incomes_2016 <- function(applicant_race_and_ethnicity,
                                                        applicant_income_000s,
                                                        owner_occupancy,
                                                        preapproval,
                                                        property_type,
                                                        loan_amount_000s,
                                                        applicant_sex,
                                                        county_code,
                                                        tract_to_msamd_income,
                                                        co_applicant_present,
                                                        agency_code,
                                                        minority_population,
                                                        rate_spread,
                                                        loan_purpose,
                                                        respondent_id) {
  pred <- predict_helper(
    model_2016_ratio_of_loan_to_income_with_log_of_income_and_amount,
    applicant_race_and_ethnicity, applicant_income_000s, owner_occupancy, preapproval,
    property_type, loan_amount_000s, applicant_sex, county_code, tract_to_msamd_income,
    co_applicant_present, agency_code, minority_population, rate_spread, loan_purpose,
    respondent_id)

  paste("----------------\nTest case predicted to be", as.character(pred), "\n----------------\n")
}

#' Model adding loan to income ratio and log of income and loan amount. PA 2017 dataset
#' predict the probability of an applicant getting approved for a home loan with the logistic regression model
#' @param applicant_race_and_ethnicity:character The race of the applicant. Allowed values include Black or African American, White, Asian, Hispanic or Latino, etc.
#' @param applicant_income_000s:int The gross annual income of the applicant rounded to the nearest thousand.
#' @param owner_occupancy:int A code representing owner-occupancy status of the property. Second homes, vacation homes, etc. Allowed values are 1, 2, 3
#' @param preapproval:int A code representing preapproval status of the applicant. Allowed values 1, 2, 3.
#' @param property_type:int A code representing the type of the property.
#' @param applicant_sex:int A code representing the sex of the applicant. Allowed values are 1, 2, 3, 4
#' @param county_code:int A three digit code representing the county of the property. 
#' @param tract_to_msamd_income:numeric The percentage of the median family income for the tract compared to the median family income for the MSA/MD, rounded to two decimal places.
#' @param co_applicant_present:int A boolean value indicating whether a coapplicant was present in the loan application. Allowed values are 1 and 0.
#' @param agency_code:int The federal regulatory agency.
#' @param minority_population:numeric The percentage of minority population to total population for the census tract, carried to two decimal places
#' @param rate_spread:numeric The rate spread for the loan, which is the difference between the loan's annual percentage rate (APR) and the average prime offer rate (APOR).
#' @param respondent_id A code representing the bank or other financial institution that is reporting the loan or application.
#' @param loan_amount_000s:int The amount of the loan applied for, in thousands of dollars.
#' @param loan_purpose:int The loan purpose. Allowed values are 1, 2 and 3.
#' @get /predict_with_ratio_of_incomes_log_incomes_2017_model
#' @html
#' @response 200 Returns the probability of the applicant getting a loan or not.
predict_loan_decision_with_log_incomes_2017 <- function(applicant_race_and_ethnicity,
                                                        applicant_income_000s,
                                                        owner_occupancy,
                                                        preapproval,
                                                        property_type,
                                                        loan_amount_000s,
                                                        applicant_sex,
                                                        county_code,
                                                        tract_to_msamd_income,
                                                        co_applicant_present,
                                                        agency_code,
                                                        minority_population,
                                                        rate_spread,
                                                        loan_purpose,
                                                        respondent_id) {
  pred <- predict_helper(
    model_2017_ratio_of_loan_to_income_with_log_of_income_and_amount,
    applicant_race_and_ethnicity, applicant_income_000s, owner_occupancy, preapproval,
    property_type, loan_amount_000s, applicant_sex, county_code, tract_to_msamd_income,
    co_applicant_present, agency_code, minority_population, rate_spread, loan_purpose,
    respondent_id)

  paste("----------------\nTest case predicted to be", as.character(pred), "\n----------------\n")
}

#' Model adding loan to income ratio and log of income and loan amount. PA 2014 dataset
#' predict the probability of an applicant getting approved for a home loan with the logistic regression model
#' @param applicant_race_and_ethnicity:character The race of the applicant. Allowed values include Black or African American, White, Asian, Hispanic or Latino, etc.
#' @param applicant_income_000s:int The gross annual income of the applicant rounded to the nearest thousand.
#' @param owner_occupancy:int A code representing owner-occupancy status of the property. Second homes, vacation homes, etc. Allowed values are 1, 2, 3
#' @param preapproval:int A code representing preapproval status of the applicant. Allowed values 1, 2, 3.
#' @param property_type:int A code representing the type of the property.
#' @param applicant_sex:int A code representing the sex of the applicant. Allowed values are 1, 2, 3, 4
#' @param county_code:int A three digit code representing the county of the property. 
#' @param tract_to_msamd_income:numeric The percentage of the median family income for the tract compared to the median family income for the MSA/MD, rounded to two decimal places.
#' @param co_applicant_present:int A boolean value indicating whether a coapplicant was present in the loan application. Allowed values are 1 and 0.
#' @param agency_code:int The federal regulatory agency.
#' @param minority_population:numeric The percentage of minority population to total population for the census tract, carried to two decimal places
#' @param rate_spread:numeric The rate spread for the loan, which is the difference between the loan's annual percentage rate (APR) and the average prime offer rate (APOR).
#' @param respondent_id A code representing the bank or other financial institution that is reporting the loan or application.
#' @param loan_amount_000s:int The amount of the loan applied for, in thousands of dollars.
#' @param loan_purpose:int The loan purpose. Allowed values are 1, 2 and 3.
#' @get /predict_with_ratio_of_incomes_log_incomes_2014_il_model
#' @html
#' @response 200 Returns the probability of the applicant getting a loan or not.
predict_loan_decision_with_log_incomes_il_2014 <- function(applicant_race_and_ethnicity,
                                                           applicant_income_000s,
                                                           owner_occupancy,
                                                           preapproval,
                                                           property_type,
                                                           loan_amount_000s,
                                                           applicant_sex,
                                                           county_code,
                                                           tract_to_msamd_income,
                                                           co_applicant_present,
                                                           agency_code,
                                                           minority_population,
                                                           rate_spread,
                                                           loan_purpose,
                                                           respondent_id) {
  pred <- predict_helper(
    model_2014_il_ratio_of_loan_to_income_with_log_of_income_and_amount,
    applicant_race_and_ethnicity, applicant_income_000s, owner_occupancy, preapproval,
    property_type, loan_amount_000s, applicant_sex, county_code, tract_to_msamd_income,
    co_applicant_present, agency_code, minority_population, rate_spread, loan_purpose,
    respondent_id)
  
  paste("----------------\nTest case predicted to be", as.character(pred), "\n----------------\n")
}
