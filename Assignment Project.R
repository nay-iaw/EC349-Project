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

#Handle missing values- replace NA with means/median
user_business_merged2$review_count.x <- ifelse(is.na(user_business_merged2$review_count.x), median(user_business_merged2$review_count.x, na.rm = TRUE), user_business_merged2$review_count.x)
user_business_merged2$average_stars<- ifelse(is.na(user_business_merged2$average_stars), mean(user_business_merged2$average_stars, na.rm = TRUE), user_business_merged2$average_stars)
user_business_merged2$sum_compliments <- ifelse(is.na(user_business_merged2$sum_compliments), median(user_business_merged2$sum_compliments, na.rm = TRUE), user_business_merged2$sum_compliments)
user_business_merged2$checkin_count <- ifelse(is.na(user_business_merged2$checkin_count), median(user_business_merged2$checkin_count, na.rm = TRUE), user_business_merged2$checkin_count)
sum(is.na(user_business_merged2))

#Set seed for reproducibility
set.seed(1) 

#Split the data into test and training
test_obs <- sample(1:nrow(user_business_merged2), 10000)
review_test <- user_business_merged2[test_obs, ]
review_train <- user_business_merged2[-test_obs, ]
reviewx_train <- review_train[,-1]
reviewy_train <- review_train[,1]
reviewx_test <- review_test[,-1]
reviewy_test <- review_test [,1]
reviewy_train <- as.numeric(reviewy_train)

#Subset
userx_business_merged2 <- user_business_merged2 [,-1]
usery_business_merged2 <- user_business_merged2 [,1]

#Multinomial logistic regression
library(nnet)
review_multinom_model <- multinom(stars.x ~ useful + funny + cool + word_count + review_count.x + average_stars + sum_compliments + stars.y + review_count.y + checkin_count, data = review_train)
summary (review_multinom_model)
multinom_prediction <- predict(review_multinom_model, newdata = review_test)
summary (multinom_prediction)
review_test$stars.x <- factor(review_test$stars.x)
summary (review_test$stars.x)

#Ridge with validation
install.packages("glmnet")
library(glmnet) 
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(as.matrix(userx_business_merged2), as.matrix(usery_business_merged2), alpha = 0, lambda = grid, thresh = 1e-12)
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
print(ridge_MSE)#1.481496

#LASSO with validation
cv.out <- cv.glmnet(as.matrix(reviewx_train), as.matrix(reviewy_train), alpha = 1, nfolds = 5)
plot(cv.out)
lambda_LASSO_cv <- cv.out$lambda.min #cross-validation is the lambda minimising empirical MSE in training data
#Re-Estimate Ridge with lambda chosen by Cross validation
LASSO.mod<-glmnet(reviewx_train, reviewy_train, alpha = 1, lambda = lambda_LASSO_cv, thresh = 1e-12)
coef(LASSO.mod) #note that some parameter estimates are set to 0 --> Model selection!
#check_in count set to 0

#Fit on Test Data
LASSO.pred <- predict(LASSO.mod, s = lambda_LASSO_cv, newx = as.matrix(reviewx_test))
str(reviewy_test)
LASSO_MSE<- mean((LASSO.pred - reviewy_test) ^ 2) 
print(LASSO_MSE)#1.477707

#Decision Tree
install.packages("tree")
install.packages("rpart")
install.packages("rpart.plot")
library(tree)
library(rpart)
library(rpart.plot)
table(review_train$stars.x)
table(review_test$stars.x)
reviewtree <- tree(stars.x ~., data = review_train, method = "class")
rpart.plot(reviewtree)

