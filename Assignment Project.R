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

#View data
View(business_data)
View(checkin_data)
View(review_data_small)
View(tip_data)
View(user_data_small)

#Set seed for reproducibility
set.seed(1) 

#Split data into training and test
test_obs <- sample(1:nrow(review_data_small), 10000)
review_test <- review_data_small[test_obs, ]
review_train <- review_data_small[-test_obs, ]

#Subsetting data
reviewx_train <-review_train[,-1]
reviewy_train <-review_train[,1]
reviewx_test <-review_test[,-1]
reviewy_test <-review_test[,1]

#Preliminary Multinomial Logistic Regression and Prediction
install.packages("nnet")
library(nnet)
review_multinom_model <- multinom(stars ~ useful + funny + cool, data = review_train)
summary (review_multinom_model)
multinom_prediction <- predict(review_multinom_model, newdata = review_test)
summary (multinom_prediction)
review_test$stars <- factor(review_test$stars)
summary (review_test$stars)

#Merge datasets- train & test
merge_review_user <- merge (review_data_small, user_data_small, by ="user_id")
test_obs2 <- sample(1:nrow(merge_review_user), 10000)
review_test2 <- merge_review_user[test_obs2, ]
review_train2 <- merge_review_user[-test_obs2, ]

#Preliminary Multinomial Logistic Regression and Prediction adding one more variable
review_multinom_model2 <- multinom(stars ~ useful.x + funny.x + cool.x + average_stars, data = review_train2)
summary(review_multinom_model2)
multinom_prediction2 <- predict(review_multinom_model2, newdata = review_test2)
summary (multinom_prediction2)
review_test2$stars <- factor(review_test2$stars)
summary (review_test2$stars)

#Clear previous data
rm (merge_review_user, review_multinom_model, review_multinom_model2, review_test, review_test2, review_train, review_train2, reviewx_test, reviewx_train, reviewy_test, reviewy_train, test_obs, test_obs2, multinom_prediction, multinom_prediction2)

# Plot association between stars and useful, cool, funny, word count in text- review
install.packages("ggplot2")
library(ggplot2)
ggplot(review_data_small, aes(x=stars, y=useful)) + geom_bar(position='dodge', stat='summary', fun='mean')
ggplot(review_data_small, aes(x=stars, y=cool)) + geom_bar(position='dodge', stat='summary', fun='mean')
ggplot(review_data_small, aes(x=stars, y=funny)) + geom_bar(position='dodge', stat='summary', fun='mean')
library(dplyr)
library(stringr)
review_data_small <- review_data_small %>% + mutate(word_count = str_count(text, "\\S+"))
ggplot(review_data_small, aes(x=stars, y=word_count)) + geom_bar(position='dodge', stat='summary', fun='mean')

#Transforming user data
user_data_small$sum_compliments <- rowSums(user_data_small[, 12:22])

#Plot association between review count, sum of compliments and average stars
ggplot(user_data_small, aes(x=review_count, y=average_stars)) + geom_point() + geom_smooth(method="lm")

#merge review and user data by user id
library(tidyverse)
user_data_small2 <- user_data_small %>%
  select(user_id, review_count, average_stars, sum_compliments)
user_review_merged <- left_join(review_data_small, user_data_small2, by = "user_id")

#Merge business checkin and tip data by business_id
library(stringr)
library(dplyr)
checkin_data <- checkin_data %>%
  mutate(checkin_count = sapply(checkin_data$date[-1], function(cell) str_count(cell, ",") + 1))

