library(ggplot2)
library(Metrics) 
#utilises the power of decision trees to try to classify customers as possible risks
# of leaving the service provider.


customer_data <- read.csv("data/telco_churn.csv", strip.white=TRUE)

#Check summaries and idenitify rows with null values
customer_data[!complete.cases(customer_data),]
customer_data <- customer_data[complete.cases(customer_data),]

#Data summary and exploration
summary(customer_data)

#Split into test and training data
customer_data_train <- customer_data [1:7000,]
customer_data_test <- customer_data[7000:7032,]
