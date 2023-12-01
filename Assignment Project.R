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

# Plot association between stars and useful, cool, funny, word count in text- review
install.packages("ggplot2")
library(ggplot2)
ggplot(review_data_small, aes(x=stars, y=useful)) + geom_bar(position='dodge', stat='summary', fun='mean')
ggplot(review_data_small, aes(x=stars, y=cool)) + geom_bar(position='dodge', stat='summary', fun='mean')
ggplot(review_data_small, aes(x=stars, y=funny)) + geom_bar(position='dodge', stat='summary', fun='mean')
library(dplyr)
library(stringr)
review_data_small <- review_data_small %>% 
  mutate(word_count = str_count(text, "\\S+"))
ggplot(review_data_small, aes(x=stars, y=word_count)) + geom_bar(position='dodge', stat='summary', fun='mean')

#Transforming user data
user_data_small$sum_compliments <- rowSums(user_data_small[, 12:22])

#Plot association between review count, sum of compliments and average stars
ggplot(user_data_small, aes(x=review_count, y=average_stars)) + geom_point() + geom_smooth(method="lm")

#Merge review and user data by user id
library(tidyverse)
user_data_small2 <- user_data_small %>%
  select(user_id, review_count, average_stars, sum_compliments)
user_review_merged <- left_join(review_data_small, user_data_small2, by = "user_id")


#Transform checkin_data
library(stringr)
library(dplyr)
checkin_data <- checkin_data %>%
  mutate(checkin_count = sapply(checkin_data$date, function(cell) str_count(cell, ",") + 1))

#Merge business and checkin data by business_id
checkin_data2 <- checkin_data %>%
  select(business_id, checkin_count)
business_checkin_merged <- left_join(business_data, checkin_data2, by = "business_id")
business_checkin_merged2 <- business_checkin_merged %>%
  select(business_id, stars, review_count, checkin_count)

#Merge business and user data
user_business_merged <- left_join(user_review_merged, business_checkin_merged2, by = "business_id")
user_business_merged2 = subset(user_business_merged, select = -c(1,2,3,8,9))

#Change variables from int to numeric; stars to factor
str(user_business_merged2)
user_business_merged2$useful <- as.numeric(user_business_merged2$useful)
user_business_merged2$funny <- as.numeric(user_business_merged2$funny)
user_business_merged2$cool<- as.numeric(user_business_merged2$cool)
user_business_merged2$word_count <- as.numeric(user_business_merged2$word_count)
user_business_merged2$review_count.x <- as.numeric(user_business_merged2$review_count.x)
user_business_merged2$review_count.y <- as.numeric(user_business_merged2$review_count.y)
user_business_merged2$stars.x <- as.factor(user_business_merged2$stars.x)

#Split the data into test and training
set.seed(1)
test_obs <- sample(1:nrow(user_business_merged2), 10000)
review_test <- user_business_merged2[test_obs, ]
review_train <- user_business_merged2[-test_obs, ]


#Handle missing values- replace NA with means/median
review_train$review_count.x <- ifelse(is.na(review_train$review_count.x), median(review_train$review_count.x, na.rm = TRUE), review_train$review_count.x)
review_train$average_stars<- ifelse(is.na(review_train$average_stars), mean(review_train$average_stars, na.rm = TRUE), review_train$average_stars)
review_train$sum_compliments <- ifelse(is.na(review_train$sum_compliments), median(review_train$sum_compliments, na.rm = TRUE), review_train$sum_compliments)
review_train$checkin_count <- ifelse(is.na(review_train$checkin_count), median(review_train$checkin_count, na.rm = TRUE), review_train$checkin_count)
sum(is.na(review_train))

#Random Forest
install.packages("randomForest")
library(randomForest)
model_RF <- randomForest(stars.x ~ ., data = review_train)

#File is too large TT- run in parallel then combine
# Set the number of cores to use
cores_to_use <- 7
# Register parallel backend
cl <- makeCluster(cores_to_use)
registerDoParallel(cl)
# Formula
formula <- stars.x = useful + funny + cool + word_count + review_count.x + average_stars + sum_compliments + stars.y + review_count.y + checkin_count
# Use foreach to grow the random forest in parallel
rf_model <- foreach(ntree = rep(200000, cores_to_use), .combine = combine, .packages = "randomForest") %dopar% {
  randomForest(formula, data = review_train, ntree = 1000)
}

#STILL TOO LARGE SAVE MY LAPTOP
