
library(naniar)
library(ggplot2)

## Helper function to graph the distribution of loan types. 
## Before we filter out conventional loans, look at the distribution of those loans.
## Pass original dataframe as a parameter
graph_loan_types<-function(dataf){
  ggplot(data = summarise_at(group_by(dataf,loan_type_name),
                             vars(loan_type),funs(n())),
         aes(x = loan_type_name,y =  loan_type)) + 
    geom_bar(stat = "identity",fill = "#009E73") + 
    geom_text(aes(label = loan_type), vjust = -0.5) +
    labs(title = "Type of Loans Distribution" , x = "Loan Type" , y = "Count")
}

## Helper function to graph mortgage distribution by applicant ethinicity.
## Please pass the hmda data frame instance as a parameter.
graph_by_enthicity <- function(mortgage_by_ethnicity) {
  ggplot(mortgage_by_ethnicity, aes(x = reorder(applicant_race_and_ethnicity, EthnicityCount), y = EthnicityCount)) +
    geom_bar(stat='identity', colour="white", fill = 'steelblue') +
    geom_text(aes(x = applicant_race_and_ethnicity, y = 1, label = paste0("(",round(EthnicityCount),")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Applicant Ethnicity', y = 'Count of Mortgages by Ethnicity', title = 'Ethnicity Distribution') +  theme_bw()  + coord_flip() 
}

## Helper function to graph ethnicities which have the largest share of
## loans in origination status.
graph_ethnicity_proportion_of_loans <- function(mortgage_status_aggregated_by_ethnicity) {
  # https://plot.ly/ggplot2/facet_wrap/
  ggplot(mortgage_status_aggregated_by_ethnicity,
         aes(x = reorder(action_taken_name, percentage), y = percentage)) +
    geom_bar(stat='identity',colour="white", fill = "blue") +
    facet_wrap(~ applicant_race_and_ethnicity) +
    geom_text(aes(x = action_taken_name, y = 1, label =   
                    paste0("(",round(percentage),"%)",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Action taken', y = 'Percentage of Loans in origination status', title
         = 'Percentage of Loans in origination status by ethnicity') + 
    coord_flip() + theme_bw()
}

## Function to graph the mortgage distribution by race1.
## Please pass the hmda data frame to the function
graph_mortgage_distribution_by_race1 <- function(mortgage_by_race) {
  ggplot(mortgage_by_race, aes(x = reorder(applicant_race_name_1, RaceCount), y =
                               RaceCount)) +
    geom_bar(stat='identity', colour="white", fill = 'blue') +
    geom_text(aes(x = applicant_race_name_1, y = 1, label =
                    paste0("(",round(RaceCount),")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Applicant Race', y = 'Count of Mortgages by Race', title = 'Race
         Distribution') +  theme_bw()  + coord_flip() 
}

# Function to graph which applicant races have the largest proportion of loans in
# various stages
graph_application_race_proportion_of_loans <- function(mortgage_status_aggregated_by_race1) {
  # https://plot.ly/ggplot2/facet_wrap/
  ggplot(mortgage_status_aggregated_by_race1, aes(x = reorder(action_taken_name, percentage), y = percentage)) +
    geom_bar(stat='identity', colour="white", fill = "#00AFBB") +
    facet_wrap(~ applicant_race_and_ethnicity) +
    geom_text(aes(x = action_taken_name, y = 1, label = paste0("(",round(percentage),"%)",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Action taken', y = 'Percentage of Loans in origination status', title = 'Percentage of Loans in origination status by Race 1') +  coord_flip() + theme_bw()  
}

## Function to graph applicant income histogram.
graph_applicant_income_histogram <- function(hmda_df, title) {
  hmda_df %>% ggplot(aes(as.numeric(applicant_income_000s))) +
    scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + geom_histogram(binwidth = 50,, fill=c("steelblue")) + labs(x = "Income in Thousands", y = "Applicant Count", title = "Applicant Income Distribution for Originated Loans") + theme_bw() +
    ggtitle(title) 
}

## Function to graph loan distribution by county.
graph_distribution_by_county <- function(mortgage_distribution_by_counties) {
  mortgage_distribution_by_counties %>% ggplot(aes(x = county_name,y = percentage)) +
    geom_bar(stat='identity',colour="white", fill=c("steelblue")) +
    geom_text(aes(x = county_name, y = 1, label = paste0("( ",round(percentage, 2),"% )",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'County', y = 'Loan count', title = 'County loan distribution') +
    coord_flip() + 
    theme_bw()
}

## Helper function to graph mortgage distribution by applicant ethinicity.
## Please pass the hmda data frame instance as a parameter.
graph_by_race_and_enthicity <- function(mortgage_by_race_and_ethnicity) {
  ggplot(mortgage_by_race_and_ethnicity, aes(x = reorder(applicant_race_and_ethnicity, EthnicityCount), y = EthnicityCount)) +
    geom_bar(stat='identity', colour="white", fill = 'steelblue') +
    geom_text(aes(x = applicant_race_and_ethnicity, y = 1, label = paste0("(",round(EthnicityCount),")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Applicant Race and Ethnicity', y = 'Count of Mortgages by Race and Ethnicity', title = 'Ethnicity Distribution') +  theme_bw()  + coord_flip() 
}

## Helper function to graph ethnicities which have the largest share of
## loans in origination status.
graph_race_and_ethnicity_proportion_of_loans <- function(mortgage_status_aggregated_by_race_and_ethnicity) {
  # https://plot.ly/ggplot2/facet_wrap/
  ggplot(mortgage_status_aggregated_by_race_and_ethnicity,
         aes(x = reorder(action_taken_name, percentage), y = percentage)) +
    geom_bar(stat='identity',colour="white", fill = "steelblue") +
    facet_wrap(~ applicant_race_and_ethnicity) +
    geom_text(aes(x = action_taken_name, y = 1, label =   
                    paste0("(",round(percentage),"%)",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Action taken', y = 'Percentage of Loans in origination status', title
         = 'Percentage of Loans in origination status by race and ethnicity') + 
    coord_flip() + theme_bw()
}

## Function to graph the mortgage distribution by race and ethnicity
## Please pass the hmda data frame to the function
graph_mortgage_distribution_by_race_and_ethnicity <- function(mortgage_by_race_and_ethnicity) {
  ggplot(mortgage_by_race_and_ethnicity, aes(x = reorder(applicant_race_and_ethnicity, RaceCount), y =
                                 RaceCount)) +
    geom_bar(stat='identity', colour="white", fill = 'steelblue') +
    geom_text(aes(x = applicant_race_and_ethnicity, y = 1, label =
                    paste0("(",round(RaceCount),")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Applicant Race and Ethnicity', y = 'Count of Mortgages by Race and Ethnicity', title = 'Race
         and Ethnicity Distribution') +  theme_bw()  + coord_flip() 
}

# Function to graph which applicant race and ethnicity have the largest proportion of loans in
# various stages
graph_application_race_and_ethnicity_proportion_of_loans <- function(mortgage_status_aggregated_by_race1) {
  # https://plot.ly/ggplot2/facet_wrap/
  ggplot(mortgage_status_aggregated_by_race1, aes(x = reorder(action_taken_name, percentage), y = percentage)) +
    geom_bar(stat='identity', colour="white", fill = "steelblue") +
    facet_wrap(~ applicant_race_and_ethnicity) +
    geom_text(aes(x = action_taken_name, y = 1, label = paste0("(",round(percentage),"%)",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Action taken', y = 'Percentage of Loans in origination status', title = 'Percentage of Loans in origination status by Race and Ethnicity') +  coord_flip() + theme_bw()  
}

get_corr_matrix_for_sig_level <- function(data=df, sig=0.5) {
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  return (mtx_corr)
}

# https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
corr_simple <- function(data, sig=0.5) {
  mtx_corr <- get_corr_matrix_for_sig_level(data, sig)
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", tl.cex = 0.5)
}

# Helper to visualize missing values in the hmda data frame passed in.
# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# http://naniar.njtierney.com/articles/naniar-visualisation.html
# https://stackoverflow.com/questions/17964513/subset-variables-in-data-frame-based-on-column-type
visualize_missing_values <- function(hmda_data_df) {
  print(gg_miss_upset(hmda_data_df))
  
  hmda_filtered_for_whites <- hmda_data_df[hmda_data_df$applicant_race_and_ethnicity == "White",]
  hmda_filtered_for_african_americans <- hmda_data_df[hmda_data_df$applicant_race_and_ethnicity == "Black or African American",]
  
  print(gg_miss_upset(hmda_filtered_for_whites))
  print(gg_miss_upset(hmda_filtered_for_african_americans))

  print(gg_miss_var(hmda_data_df[, sapply(hmda_data_df, is.character) 
                           | sapply(hmda_data_df, is.factor)]) + 
    labs(y = "Visualize factors/characters missing values"))
  
  print(gg_miss_var(hmda_data_df[, sapply(hmda_data_df, is.numeric) 
                           | sapply(hmda_data_df, is.integer)]) + 
    labs(y = "Visualize numeric/integer missing values"))

  print(gg_miss_var(hmda_filtered_for_whites[, 
                                       sapply(hmda_filtered_for_whites, is.character) 
                                       | sapply(hmda_filtered_for_whites, is.factor)]) + 
    labs(y = "Visualize factors/characters missing values for Whites"))

  print(gg_miss_var(hmda_filtered_for_whites[, 
                                                        sapply(hmda_filtered_for_whites, is.numeric) 
                                                        | sapply(hmda_filtered_for_whites, is.integer)]) + 
          labs(y = "Visualize numeric/integer missing values for Whites"))

  print(gg_miss_var(hmda_filtered_for_african_americans[, 
                                             sapply(hmda_filtered_for_african_americans, is.character) 
                                             | sapply(hmda_filtered_for_african_americans, is.factor)]) + 
          labs(y = "Visualize factors/characters missing values for African Americans"))
  
  print(gg_miss_var(hmda_filtered_for_african_americans[, 
                                                  sapply(hmda_filtered_for_african_americans, is.numeric) 
                                                  | sapply(hmda_filtered_for_african_americans, is.integer)]) + 
    labs(y = "Visualize numeric/integer missing values for African Americans"))
}

## Function to find the feature importances from a given dataframe.
## The function uses xgboost to rank the features on there importance.
## This may take some time to run.
## Function Parameters: 
## dataf : Dataframe with original values and action taken column present with original values.
## number_of_top_features : How many top features you need the function to display.
feature_importances <- function(dataf,number_of_top_features){
  library(xgboost)
  dataf$loan_granted <- ifelse(dataf$action_taken == 1 | dataf$action_taken == 6, TRUE, FALSE)
  d <-c("action_taken")
  dataf<-dataf[ , !(names(dataf) %in% d)]
  
  dmy <- dummyVars(" ~ .", data = dataf, fullRank=T)
  df <- data.frame(predict(dmy, newdata = dataf))
  
  df = df %>% select(-loan_grantedTRUE)
  df$loan_granted = dataf$loan_granted
  
  features<-colnames(df)
  
  for (f in features) {
    if ((class(df[[f]])=="factor") || (class(df[[f]])=="character")) {
      levels <- unique(df[[f]])
      df[[f]] <- as.numeric(factor(df[[f]], levels=levels))
    }
  }
  
  formula = loan_granted ~ .
  
  fitControl <- trainControl(method="none",classProbs = TRUE)
  
  xgbGrid <- expand.grid(nrounds = 100,
                         max_depth = 3,
                         eta = .05,
                         gamma = 0,
                         colsample_bytree = .8,
                         min_child_weight = 1,
                         subsample = 1)
  
  
  set.seed(13)
  
  df$loan_granted = as.factor(df$loan_granted)
  levels(df$loan_granted) = c("No","Yes")
  
  XGB = train(formula, data = df,
              method = "xgbTree",trControl = fitControl,
              tuneGrid = xgbGrid,na.action = na.pass)
  
  
  importance = varImp(XGB)
  
  
  
  varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                              Importance = round(importance[[1]]$Overall,2))
  
  rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance)))) %>%
    head(number_of_top_features)
  
  rankImportancefull = rankImportance
  print(rankImportance)
  ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                             y = Importance)) +
    geom_bar(stat='identity',colour="white", fill = "steelblue") +
    geom_text(aes(x = Variables, y = 1, label = Rank),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Variables', title = 'Relative Variable Importance') +
    coord_flip() + 
    theme_bw()
}

## Add a new column applicant_race_and_ethnicity and group all applicants
## with ethnicity as Hispanic or Latino as Hispanic or Latino in this column.
## For everyone else, this column gets values from the applicant_race_name_1 
## column
add_applicant_race_and_ethicity_to_hmda <- function (df) {
  df$applicant_race_and_ethnicity <- NA
  df$co_applicant_race_and_ethnicity <- NA
  
  df$applicant_race_and_ethnicity <- ifelse(df$applicant_ethnicity_name == "Hispanic or Latino",
                                                                 "Hispanic or Latino", df$applicant_race_name_1)
  df$co_applicant_race_and_ethnicity <- ifelse(df$co_applicant_ethnicity_name == "Hispanic or Latino",
                                                                    "Hispanic or Latino", df$co_applicant_race_name_1)
  return (df)
}
