#Clear
cat("\014")  
rm(list=ls())

#Set Directory as appropriate
setwd("/Users/waiyan_1020/Desktop/EC349 Project")

#Pre-Processing Yelp Academic Data for the Assignment
library(jsonlite)

#Load Different Data
business_data <- stream_in(file("/Users/waiyan_1020/Desktop/EC349  Supplementary Material/yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data  <- stream_in(file("/Users/waiyan_1020/Desktop/EC349  Supplementary Material/yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data  <- stream_in(file("/Users/waiyan_1020/Desktop/EC349  Supplementary Material/yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)

#Load smaller user and review data
load("/Users/waiyan_1020/Desktop/EC349  Supplementary Material/yelp_review_small.Rda")
load("/Users/waiyan_1020/Desktop/EC349  Supplementary Material/yelp_user_small.Rda")

# Sentiment Analysis
# Load word list
positive_words <- read.table("positive-words.txt", header = FALSE, stringsAsFactors = FALSE)
negative_words <- read.table("negative-words.txt", header = FALSE, stringsAsFactors = FALSE)

# Text pre-processing
library (tm)
review_data_small$text <- tolower(review_data_small$text)
review_data_small$text <- gsub("[[:punct:]]", "", review_data_small$text)
review_data_small$text <- gsub("[[:digit:]]", "", review_data_small$text)
review_data_small$text <- removeWords(review_data_small$text, stopwords("english"))

# Function to count positive or negative words in a text
count_positive_words <- function(text, positive_words) {
  words <- unlist(strsplit(text, " "))
  count <- sum(words %in% positive_words)
  return(count)
}
count_negative_words <- function(text, negative_words) {
  words <- unlist(strsplit(text, " "))
  count <- sum(words %in% negative_words)
  return(count)
}

# Add columns for positive and negative word count in review data **takes forever
review_data_small$positive_word_count <- sapply(review_data_small$text, count_positive_words, positive_words = positive_words$V1)
review_data_small$negative_word_count <- sapply(review_data_small$text, count_negative_words, negative_words = negative_words$V1)

# Add a column indicating sentiment based on the count
review_data_small$sentiment<- ifelse(review_data_small$positive_word_count > review_data_small$negative_word_count, "positive", ifelse(review_data_small$positive_word_count < review_data_small$negative_word_count, "negative", "neutral"))

#Select average stars in user data as variable
library(tidyverse)
user_data_small2 <- user_data_small %>%
  select(user_id, average_stars)

#Merge review and user data by user id
user_review_data <- left_join(review_data_small, user_data_small2, by = "user_id")

#Select stars in business data as variables
business_data2 <- business_data %>%
  select(business_id, stars)

#Merge business and user data
user_business_data <- left_join(user_review_data, business_data2, by = "business_id")

#Remove unused variables 
user_business_data <- user_business_data %>%
  select(stars.x, useful, funny, cool, positive_word_count, negative_word_count, sentiment, average_stars, stars.y)

#Change variables from int to numeric; stars to factor
str(user_business_data)
user_business_data$useful <- as.numeric(user_business_data$useful)
user_business_data$funny <- as.numeric(user_business_data$funny)
user_business_data$cool<- as.numeric(user_business_data$cool)
user_business_data$stars.x <- as.factor(user_business_data$stars.x)
user_business_data$positive_word_count <- as.numeric(user_business_data$positive_word_count)
user_business_data$negative_word_count <- as.numeric(user_business_data$negative_word_count)
user_business_data$sentiment <- as.factor(user_business_data$sentiment)

#Remove missing values
sum(is.na(user_business_data))
user_business_data <- na.omit(user_business_data)

#Set seed for reproducibility
set.seed(1) 

#Split the data into test and training
test_obs <- sample(1:nrow(user_business_data), 10000)
review_test <- user_business_data[test_obs, ]
review_train <- user_business_data[-test_obs, ]

#Generate frequency table for actual user review
test_actual <- table (review_test$stars.x)
actual_df <- as.data.frame(test_actual)
colnames(actual_df) <- c("Actual_Outcome", "Actual_Frequency")
write.csv(actual_df, "actual_frequency_table.csv", row.names = FALSE)

#Random forests- 100 trees**takes even longer
library(randomForest)
rf_model <- randomForest(stars.x ~., data = review_train, ntree=100, mtry = sqrt(ncol(review_train)-1), nodesize = 10) 

# Print the model summary
print(rf_model)

#Fit the model
rf_pred<-predict(rf_model, newdata = review_test)
summary (rf_pred)

#Evaluate the accuracy/error rate
library(caret)
rf_conf_matrix <- confusionMatrix(rf_pred, review_test$stars.x)
rf_accuracy <- rf_conf_matrix$overall["Accuracy"]
print(rf_conf_matrix)
print(paste("Accuracy:", round(rf_accuracy, 4)))
write.table(rf_conf_matrix$table, "confusion_matrix.csv", sep = ",", col.names = NA, qmethod = "double")

#Compare with logistic regression again (all predictors)
library(nnet)
multinom_model <- multinom(stars.x ~., data = review_train)
summary (multinom_model)
multinom_coef_df <- as.data.frame(coef(summary(multinom_model)))
write.csv(multinom_coef_df, file = "regression_coefficients.csv", row.names = FALSE)

# Make predictions on the test set
multinom_prediction <- predict(multinom_model, newdata = review_test)

#Evaluate the accuracy/error rate
library(caret)
log_conf_matrix <- confusionMatrix(multinom_prediction, review_test$stars.x)
log_accuracy <- log_conf_matrix$overall["Accuracy"]
print(log_conf_matrix)
print(paste("Accuracy:", round(log_accuracy, 4)))
write.table(rf_conf_matrix$table, "log_confusion_matrix.csv", sep = ",", col.names = NA, qmethod = "double")
