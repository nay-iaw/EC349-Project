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
