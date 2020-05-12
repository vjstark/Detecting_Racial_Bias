## Helper functions for models like loading csv files, model building functions, etc.

library(caret)
library(data.table)
library(lmtest)
library(pROC)
library(ROCR)

# Provides functionality to load a csv file. We use the fread api for this
# Reason for this is that the hmda datasets are very large. While read.table
# works it takes a long time for loading the files. fread works by memory mapping
# the file into memory. This ensures that the file is paged in as needed.
# |data_dir|. The directory from which the file needs to be loaded.
# |file_name|. The name of the csv file.
# |sep|. Separator for the csv fle.
load_csv_file <- function(data_dir, file_name, sep = "") {
 data <- fread(paste(data_dir, file_name, sep = ""))
 data_df <- as.data.frame(data)
 return (data_df)
}

# Takes the HMDA input data frame and returns a dataframe which has a subset
# of the columns in the original data frame. Please note that the ist of columns
# were picked based  on intuition.
hmda_data_frame_for_model <- function(input_df) {
  # Added based on intuition. Please update as needed.
  # msamd -> Metropolitan Statistical Area Division code.
  # https://cfpb.github.io/api/hmda/fields.html
  initial_columns_for_model <- c("applicant_race_and_ethnicity", 
                                 "applicant_income_000s", 
                                 "co_applicant_race_and_ethnicity", 
                                 "co_applicant_sex", 
                                 "county_code", 
                                 "county_name", 
                                 "hoepa_status", 
                                 "lien_status", 
                                 "msamd", 
                                 "owner_occupancy", 
                                 "preapproval", 
                                 "purchaser_type",
                                 "property_type", 
                                 "loan_amount_000s", 
                                 "applicant_sex", 
                                 "hud_median_family_income", 
                                 "tract_to_msamd_income", 
                                 "loan_to_income_ratio", 
                                 "agency_code", 
                                 "minority_population",
                                 "respondent_id",
                                 "loan_purpose",
                                 "rate_spread",
                                 "population",
                                 "number_of_owner_occupied_units")
  
  # Ignore action values of 4 : Application withdrawn by applicant
  # 6: Loan purchased by the institution and 2: Application approved but
  # not accepted. 
  # The remaining action types are 1: Loan originated, 3. Application denied
  # by financial institution, 5. File closed for incompleteness and 7. Preapproval
  # request denied by financial institution.
  input_df <- input_df[input_df$action_taken != 2 & input_df$action_taken != 4 & input_df$action_taken != 6, ]

  input_df$loan_to_income_ratio <- input_df$loan_amount_000s / input_df$applicant_income_000s
  
  # https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  df_for_model <- input_df[initial_columns_for_model]
  df_for_model$loan_granted <- ifelse(input_df$action_taken == 1 | input_df$action_taken == 6, "yes", "no")
  df_for_model$log_loan_amount_000s <- log(df_for_model$loan_amount_000s)
  df_for_model$log_applicant_income_000s <- log(df_for_model$applicant_income_000s)

  # Credit scores play a huge part in loan decisions. We don't have access to
  # that data as reporting credit scores is not part of HMDA reporting requirements.
  # One option we have is to assign average credit scores by race. This is fraught
  # with danger for obvious reasons and could lead to model bias.
  # We could refine this based on income classes assuming that lower income groups
  # have lower credit scores and so on.
  # https://www.valuepenguin.com/average-credit-score
  # https://www.thebalance.com/definition-of-middle-class-income-4126870
  # https://www.valuepenguin.com/average-credit-score#average-credit-score-by-income
  df_for_model$credit_score <- 732
  
  df_for_model$credit_score[df_for_model$applicant_race_and_ethnicity == "Black or African American"] <- 677
  df_for_model$credit_score[df_for_model$applicant_race_and_ethnicity == "White"] <- 734
  df_for_model$credit_score[df_for_model$applicant_race_and_ethnicity == "Asian"] <- 745
  
  return (df_for_model)
}

# Provides functionality for train test split using stratified sampling. We use
# the rpart function createDataPartition for this.
# |formula_for_split|. Contains the field on which we want to split the data on.
# |input_df|: The input data frame.
# |train_proportion|: Train test split percentage. Defaults to 0.8
train_test_split <- function(formula_for_split, input_df, train_proportion = 0.8) {

  # https://stackoverflow.com/questions/20776887/stratified-splitting-the-data
  train.index <- createDataPartition(y = formula_for_split,
                                     p = train_proportion, list = FALSE)
  
  train <- input_df[train.index, ]
  test <-  input_df[-train.index, ]
  
  val_list <- list(train, test)
  return (val_list)
}

# The function keeps the top n values and sets the rest to the |agg_field_value|
restrict_levels_for_field <- function(df, field_name, agg_field_value, top_n) {
  df[, field_name] <- as.character(df[, field_name]) 
  
  name_table <- table(df[, field_name])
  agg <- sort(name_table, decreasing = TRUE)[1:top_n]
  
  df[,paste0(field_name, "_agg")] <- df[, field_name]
  df[!df[, field_name] %in% names(agg), paste0(field_name, "_agg")] <- agg_field_value
  
  df[, field_name] <- as.factor(df[, field_name])
  df[,paste0(field_name, "_agg")] <- as.factor(df[,paste0(field_name, "_agg")])
  return (df)
}

# Ensures that the hmda dataframe we use for modeling has the columns
# set to the correct type.
process_model_df_columns <- function(model_df) {
  model_df$co_applicant_present <- NA
  model_df$co_applicant_present <- ifelse( model_df$co_applicant_race_and_ethnicity == "White" | model_df$co_applicant_race_and_ethnicity == "Black or African American" | model_df$co_applicant_race_and_ethnicity == "Hispanic or Latino" | model_df$co_applicant_race_and_ethnicity == "Asian" | model_df$co_applicant_race_and_ethnicity == "Native Hawaiian or Other Pacific Islander" | model_df$co_applicant_race_and_ethnicity == "American Indian or Alaska Native", 1, 0)
  model_df$applicant_race_and_ethnicity <- as.factor(model_df$applicant_race_and_ethnicity)
  model_df$co_applicant_race_and_ethnicity <- as.factor(model_df$co_applicant_race_and_ethnicity)
  model_df$co_applicant_sex <- as.factor(model_df$co_applicant_sex)
  model_df$co_applicant_present <- as.factor(model_df$co_applicant_present)
  model_df$county_name <- as.factor(model_df$county_name)
  model_df$hoepa_status <- as.factor(model_df$hoepa_status)
  model_df$lien_status <- as.factor(model_df$lien_status)
  model_df$preapproval <- as.factor(model_df$preapproval)
  model_df$property_type <- as.factor(model_df$property_type)
  model_df$applicant_sex <- as.factor(model_df$applicant_sex)
  model_df$county_code <- as.integer(model_df$county_code)
  model_df$agency_code <- as.factor(model_df$agency_code)
  model_df$owner_occupancy <- as.factor(model_df$owner_occupancy)
  model_df$loan_granted <- as.factor(model_df$loan_granted)
  # We have 1024 respondent ids in PA for e.g Remove dashes from the
  # id and convert it to an integer column
  #model_df$respondent_id <- gsub("-", "", model_df$respondent_id)
  #model_df$respondent_id <- as.numeric(model_df$respondent_id)
  model_df$respondent_id <- as.factor(model_df$respondent_id)
  model_df$loan_purpose <- as.factor(model_df$loan_purpose)
  model_df$credit_score <- as.factor(model_df$credit_score)
  
  model_df <- restrict_levels_for_field(model_df, "respondent_id", "other", 50)
  model_df <- restrict_levels_for_field(model_df, "county_code", "other", 25)
  
  model_df <- model_df[, !names(model_df) %in% c("respondent_id", "county_code")]
  names(model_df)[names(model_df) == "respondent_id_agg"] <- "respondent_id";
  names(model_df)[names(model_df) == "county_code_agg"] <- "county_code";
  return (model_df)
}

# Helper function to build a logistic regression model using the |model_formula|
# and the training dataframe passed in.
# Returns the built model.
build_glm_model <- function(model_formula, train_df, iter = 10) {
  model <- glm(as.formula(model_formula), family = binomial(link='logit'), data=train_df, 
               control = glm.control(maxit = iter))
  return (model)
}

# Helper function to build a cross validation glm (logistic regression model)
# |model_formula|. The list of input columns and the output result column.
# |train_df|. The training data frame.
# Returns the cross validation model.
cross_validation <- function(model_formula, train_df) {
  ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE)
  model_cross_val <- train(as.formula(model_formula), data=train_df, method="glm", trControl = ctrl, family = "binomial",
                           tuneLength = 15)
  return (model_cross_val)
}

# Prints the model summary and its plot.
model_summary_and_plot <- function(model, train) {
  writeLines("Model summary")
  writeLines("")
  print(summary(model))
  writeLines("Anova for model")
  print(anova(model, test="Chisq"))
  writeLines("")
  plot(model)

  pred <- prediction(fitted(model), train$loan_granted)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  
  writeLines("")
  writeLines("optimal cut threshold is")
  print(opt.cut(perf, pred))
  flush.console()

  # Add threshold labels.
  plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
       text.adj=c(-0.2,1.7))
  
  plot.roc(train$loan_granted, fitted(model), auc.polygon = TRUE, 
           auc.polygon.col=rgb(.35,0.31,0.61, alpha = 0.4), 
           auc.polygon.border=rgb(.35,0.31,0.61, 0.4), print.auc = TRUE)
}

# https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
# McFadden's pseudo-R squared value.
mcfadden_r_square <- function(model, nullModel) {
  l <- logLik(model) / logLik(nullModel)
  R_square <- 1 - l
  writeLines("")
  writeLines("Mcfadden Rsquare value")
  return(R_square)
}

# Generates the output label for the predictions based on the threshold value
# for the probability of a class being labeled as positive/negative. We then print
# the confusion matrix for the same.
# The confusion matrix is returned.
prediction_stats <- function(fitted.probs, test, positive = TRUE, negative = FALSE,
                             threshold = 0.5) {
  fitted.results <- ifelse(fitted.probs > threshold, positive, negative)
  fitted.results <- as.factor(fitted.results)
  
  print(levels(fitted.results))
  flush.console()
  
  classification_error <- mean(fitted.results != test$loan_granted)
  classification_error
  
  print(paste('Accuracy', 1 - classification_error))
  print(confusionMatrix(as.factor(fitted.results), as.factor(test$loan_granted), positive = positive))
}

## Function to calculate precision and recall using the confusion matrix
precision_recall <- function(confusion_matrix){
  precision <- confusion_matrix[4] / (confusion_matrix[4]+confusion_matrix[2])
  print("Precision and Recall")
  print(precision)
  
  recall <- confusion_matrix[4] / (confusion_matrix[4]+confusion_matrix[3])
  print(recall)
  
  answer <- list(precision, recall)
  return (answer)
}

# Function for ROC and PRcurve
# Needs predicted probabilites of the test data and actual value labels of test data.
model_performance_curves <- function(pred_proba,labels){
  library(precrec)
  precrec_obj <- evalmod(scores = pred_proba, labels = labels)
  autoplot(precrec_obj)
}

## Get optimum threshold for which we try precision > 0.85 and max value for recall
optimum_threshold<-function(fitted.probabilites , label){
  pr<-c() # Empty vector to store precision values
  re<-c() # Empty vector to store recall values
  thresholds<-seq(0, 0.999999999, length.out = 44) # 44 possible thresholds between 0 to 1
  for (i in thresholds){ 
    fitted.results <- ifelse(fitted.probabilites > i, 1, 0)
    labels <- ifelse(label == "yes" , 1, 0)
    model1_perf<-confusionMatrix(as.factor(fitted.results), as.factor(labels))
    pr_table<-precision_recall(model1_perf$table)
    pr<-c(pr,pr_table[[1]])# Add value of precision to vector
    re<-c(re,pr_table[[2]])# Add value of recall to vector
  }
  # Create dataframe of the vectors.
  opt_thres<-data.frame(Precision = pr , Recall = re , Threshold = thresholds) 
  # Highest recall value for precisions more than 0.85
  high<-max(opt_thres$Recall[opt_thres$Precision>0.85])
  opt<-opt_thres$Threshold[opt_thres$Recall == high]
  print(paste("The optimum threshold value is ", opt , "for Precision = " , opt_thres$Precision[opt_thres$Recall == high] , "and Recall = ", high))
  return (opt)
}

# load the census dataset containing poplation distribution per county.
load_and_process_census_data <- function(census_file_path) {
  census_data <- fread(census_file_path)
  
  census_data_df <- as.data.frame(census_data)
  census_data_df <- filter(census_data_df, census_data_df$AGEGRP == 0 & census_data_df$YEAR == 1)
  
  census_data_df$WHITE_PERCENT <- ((census_data_df$WA_MALE + census_data_df$WA_FEMALE) / census_data_df$TOT_POP) * 100
  census_data_df$BLACK_PERCENT <- ((census_data_df$BA_MALE + census_data_df$BA_FEMALE) / census_data_df$TOT_POP) * 100
  census_data_df$ASIAN_PERCENT <- ((census_data_df$AA_MALE + census_data_df$AA_FEMALE) / census_data_df$TOT_POP) * 100
  census_data_df$NATIVE_HAWAII_PACIFIC_ISLANDER_PERCENT <- ((census_data_df$NA_FEMALE + census_data_df$NA_MALE) / census_data_df$TOT_POP) * 100
  census_data_df$AMERICAN_INDIAN_PERCENT <- ((census_data_df$IA_FEMALE + census_data_df$IA_FEMALE) / census_data_df$TOT_POP) * 100
  census_data_df$HISPANIC_PERCENT <- ((census_data_df$H_MALE + census_data_df$H_FEMALE) / census_data_df$TOT_POP) * 100
  census_data_df$CTYNAME <- as.factor(census_data_df$CTYNAME)
  return (census_data_df)
}

# Helper function to merge hmda dataset with the census dataset.We merge using
# the county name column.
merge_hmda_df_with_census_df <- function(census_df, hmda_df) {
  table1 <- as.data.table(hmda_df)
  table2 <- as.data.table(census_df[, c("WHITE_PERCENT", "BLACK_PERCENT", "ASIAN_PERCENT", "NATIVE_HAWAII_PACIFIC_ISLANDER_PERCENT", "AMERICAN_INDIAN_PERCENT", "HISPANIC_PERCENT", "CTYNAME")])
  
  join <- merge(table1, table2, by.x = c("county_name"), by.y = c("CTYNAME"))
  
  merged_hmda_data_df_for_model <- as.data.frame(join)
  merged_hmda_data_df_for_model$log_loan_amount_000s <- log(merged_hmda_data_df_for_model$loan_amount_000s)
  merged_hmda_data_df_for_model$log_applicant_income_000s <- log(merged_hmda_data_df_for_model$applicant_income_000s)
  
  return (merged_hmda_data_df_for_model)
}

# https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

# Function based from this stackoverflow post
# https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
draw_confusion_matrix <- function(cm, class1, class2) {
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, class2, cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, class1, cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, class2, cex=1.2, srt=90)
  text(140, 335, class1, cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  precision_present = FALSE
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  
  if (names(cm$byClass[5]) == "Precision") {
    print("Precision found")
    precision_present = TRUE
  } else {
    print("Precision not found")
    print(names(cm$byClass[5]))
  }
  
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

  precision <- cm$table[4] / (cm$table[4]+cm$table[2])
  recall <- cm$table[4] / (cm$table[4]+cm$table[3])
  f1_score <- 2 * (recall * precision) / (recall + precision)
  
  # add in the accuracy information 
  text(10, 35, names(cm$overall[1]), cex=1.2, font=2)
  text(10, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(30, 35, names(cm$overall[2]), cex=1.2, font=2)
  text(30, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
  
  if (precision_present == FALSE) {
    text(50, 35, "Precision", cex=1.2, font=2)
    text(50, 20, round(precision, 3), cex=1.4)
    text(70, 35, "Recall", cex=1.2, font=2)
    text(70, 20, round(recall, 3), cex=1.4)
    text(90, 35, "F1-score", cex=1.2, font=2)
    text(90, 20, round(f1_score, 3), cex=1.4)
  }
}  

# Helper function to print model performance stats like precision/recall values, confusion
# matrix, roc curve, etc.
print_model_performance <- function(model_probs, test, positive, negative, threshold = 0.5) {
  model_perf <- prediction_stats(model_probs, test, positive, negative, threshold)
  print(model_perf)
  old_par <- par()
  draw_confusion_matrix(model_perf, "Loan Granted", "Loan not granted")
  par(old_par)
  print(precision_recall(model_perf$table))
  pred <- prediction(model_probs, test$loan_granted)
  perf <- ROCR::performance(pred, measure = "prec", x.measure = "rec")
  plot(perf)
  #roc(test$loan_granted~model_probs, plot = TRUE, print.auc = TRUE)
  plot.roc(test$loan_granted, model_probs, auc.polygon = TRUE,
           auc.polygon.col=rgb(.35,0.31,0.61, alpha = 0.4), 
           auc.polygon.border=rgb(.35,0.31,0.61, 0.4),
           print.auc = TRUE)
  
}

# Helper function to load the census dataframe from the path passed in
# and merge it with the hmda dataframe passed in.
merge_hmda_df_with_census <- function(census_data_path, hmda_df) {
  census_df <- load_and_process_census_data(census_data_path)
  
  merged_hmda_data_df <- merge_hmda_df_with_census_df(census_df,
                                                      hmda_df)
  return (merged_hmda_data_df)
}

build_and_evaluate_hmda_null_model <- function(model_formula, hmda_train_df,
                                               hmda_test_df) {
  null_model <- build_glm_model(model_formula, hmda_train_df)
  print(summary(null_model))

  writeLines("")
  writeLines("Running predictions on null model")
  null_model.probs <- predict(null_model, hmda_test_df, type = "response")
  null_model.results <- ifelse(null_model.probs > 0.5, "yes", "no")
  null_model.results <- factor(null_model.results, levels = c("no", "yes"))
  
  classification_error <- mean(null_model.results != hmda_test_df$loan_granted)
  classification_error
  
  print(paste('Accuracy', 1 - classification_error))
  
  cm <- confusionMatrix(as.factor(null_model.results), as.factor(hmda_test_df$loan_granted), positive = "yes")
  
  old_par <- par()
  draw_confusion_matrix(cm, "Loan Granted", "Loan not granted")
  par(old_par)

  roc(hmda_test_df$loan_granted~null_model.probs, plot = TRUE, print.auc = TRUE)    
  return(null_model)
}

# Goodness of fit tests for the |model|. |null_model| is the
# model without any predictors.
model_goodness_of_fit_tests <- function(model, null_model) {
  writeLines("")
  writeLines("Gsquare value is")
  G2 <- model$null.deviance - model$deviance
  print(G2)
  
  writeLines("")
  writeLines("Chisquare test for Gsquare")
  
  print(1 - pchisq(G2, df = model$df.null - model$df.residual))
  
  writeLines("")
  writeLines("Chisquare test two tailed")
  pchisq(G2, df = model$df.null - model$df.residual)
  
  print(lrtest(null_model, model))
  print(anova(null_model, model, test="Chisq"))
}

# Loan to value ratio is an important metric which banks use to decide
# whether a loan should be granted or not. For e.g. the higher the loan
# to value ratio the riskier it is for the bank.
# https://en.wikipedia.org/wiki/Loan-to-value_ratio
# Banks are not required  to report this metric in the HMDA reporting
# law. We use the median of the home values in that county based on
# data available from zillow https://www.zillow.com/research/data/
augment_hmda_data_with_estimated_loan_to_value_ratio_from_zillow <- 
  function(zillow_file_path, hmda_data_df_for_model, state_name, 
           zillow_period_list) {
  zillow_data <- fread(zillow_file_path)
  zillow_data_df <- as.data.frame(zillow_data)
  zillow_data_df <- filter(zillow_data_df, State == "PA")
  
  zillow_column_list <- zillow_period_list
  zillow_column_list <- c(zillow_period_list, "RegionName")
  print(zillow_column_list)

  table1 <- as.data.table(hmda_data_df_for_model)
  table2 <- as.data.table(zillow_data_df[, zillow_column_list])
  join <- merge(table1, table2, by.x = c("county_name"), by.y = c("RegionName"))
  
  merged_hmda_with_house_values <- as.data.frame(join)
  
  median_home_values <- apply(merged_hmda_with_house_values[, zillow_period_list], 1, median)
  
  merged_hmda_with_house_values$median_home_values <- median_home_values
  
  merged_hmda_with_house_values$loan_to_value_ratio <-
    merged_hmda_with_house_values$loan_amount_000s /  (merged_hmda_with_house_values$median_home_values / 1000)
  
  merged_hmda_with_house_values$loan_to_value_ratio[which(is.na(merged_hmda_with_house_values$loan_to_value_ratio))] <- 0
  return (merged_hmda_with_house_values)
}
