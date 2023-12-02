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

# Plot association between stars and useful, cool, funny
install.packages("ggplot2")
library(ggplot2)
ggplot(review_data_small, aes(x=stars, y=useful)) + geom_bar(position='dodge', stat='summary', fun='mean')
ggplot(review_data_small, aes(x=stars, y=cool)) + geom_bar(position='dodge', stat='summary', fun='mean')
ggplot(review_data_small, aes(x=stars, y=funny)) + geom_bar(position='dodge', stat='summary', fun='mean')

# Sentiment Analysis
# Load word list
positive_words <- read.table("positive-words.txt", header = FALSE, stringsAsFactors = FALSE)
negative_words <- read.table("negative-words.txt", header = FALSE, stringsAsFactors = FALSE)

# Text preprocessing
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
data$Sentiment_Label <- ifelse(data$Positive_Word_Count > data$Negative_Word_Count, "positive",
                               ifelse(data$Positive_Word_Count < data$Negative_Word_Count, "negative", "neutral"))

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

#Change variables from int to numeric; stars to factor
str(user_business_data)
user_business_data$useful <- as.numeric(user_business_data$useful)
user_business_data$funny <- as.numeric(user_business_data$funny)
user_business_data$cool<- as.numeric(user_business_data$cool)
user_business_data$stars.x <- as.factor(user_business_data$stars.x)
user_business_data$positive_word_count <- as.numeric(user_business_data$positive_word_count)
user_business_data$negative_word_count <- as.numeric(user_business_data$negative_word_count)

#Remove unused variables 
user_business_data <- user_business_data %>%
  select(stars.x, useful, funny, cool, positive_word_count, negative_word_count, average_stars, stars.y)

#Check and handle missing values
sum(is.na(user_business_data))
user_business_data$average_stars<- ifelse(is.na(user_business_data$average_stars), mean(user_business_data$average_stars, na.rm = TRUE), user_business_data$average_stars)

#Set seed for reproducibility
set.seed(1) 

#Split the data into test and training
test_obs <- sample(1:nrow(user_business_data), 10000)
review_test <- user_business_data[test_obs, ]
review_train <- user_business_data[-test_obs, ]
reviewx_train <- review_train[,-1]
reviewy_train <- review_train[,1]
reviewx_test <- review_test[,-1]
reviewy_test <- review_test [,1]
reviewy_train <- as.numeric(reviewy_train)

#Multinomial logistic regression
library(nnet)

# Train the multinomial logistic model
multinom_model <- multinom(stars.x ~., data = review_train)
summary (multinom_model)

# Make predictions on the test set
multinom_prediction <- predict(multinom_model, newdata = review_test)
summary (multinom_prediction)
multinom_prediction <- predict(multinom_model, newdata = review_test, type = "class")
# Calculate accuracy
accuracy <- sum(multinom_prediction == review_test$stars.x) / length(review_test$stars.x)
print (accuracy) #0.5717

#Subsetting data
userx_business_data <- user_business_data [,-1]
usery_business_data <- user_business_data [,1]

#Ridge with validation
install.packages("glmnet")
library(glmnet) 
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(as.matrix(userx_business_data), as.matrix(usery_business_data), alpha = 0, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(as.matrix(reviewx_train), as.matrix(reviewy_train), alpha = 0, nfolds = 5)
plot(cv.out)
lambda_ridge_cv <- cv.out$lambda.min
#Re-Estimate Ridge with lambda chosen by Cross validation
ridge.mod<-glmnet(reviewx_train, reviewy_train, alpha = 0, lambda = lambda_ridge_cv, thresh = 1e-12)
#Fit on Test Data
ridge.pred <- predict(ridge.mod, s = lambda_ridge_cv, newx = as.matrix(reviewx_test))
str(reviewy_test)
reviewy_test <- as.numeric(reviewy_test)
ridge_MSE<- mean((ridge.pred - reviewy_test) ^ 2) 
print(ridge_MSE) 
cat(ridge_MSE, file = "ridge_MSE.txt")

#LASSO with validation
cv.out <- cv.glmnet(as.matrix(reviewx_train), as.matrix(reviewy_train), alpha = 1, nfolds = 5)
plot(cv.out)
lambda_LASSO_cv <- cv.out$lambda.min #cross-validation is the lambda minimising empirical MSE in training data
#Re-Estimate Ridge with lambda chosen by Cross validation
LASSO.mod<-glmnet(reviewx_train, reviewy_train, alpha = 1, lambda = lambda_LASSO_cv, thresh = 1e-12)
coef(LASSO.mod) 
#no coefficients are set to 0
#Fit on Test Data
LASSO.pred <- predict(LASSO.mod, s = lambda_LASSO_cv, newx = as.matrix(reviewx_test))
str(reviewy_test)
LASSO_MSE<- mean((LASSO.pred - reviewy_test) ^ 2) 
print(LASSO_MSE)
cat(LASSO_MSE, file = "LASSO_MSE.txt")



