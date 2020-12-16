library(ggplot2)
library(Metrics)
library(C50)
library(gmodels)
library(rpart)
#library(reldist)
#utilises the power of decision trees to try to classify customers as possible risks
# of leaving the service provider.


    #PREPARATION

customer_data <- read.csv("data/telco_churn.csv", strip.white=TRUE)

#Check summaries and idenitify rows with null values
customer_data[1] <- NULL
customer_data[!complete.cases(customer_data),]
customer_data <- customer_data[complete.cases(customer_data),]

#Data summary and exploration
summary(customer_data)
table(customer_data$Churn)


#Convert outcome to a factor
customer_data$Churn <- as.factor(customer_data$Churn)

#GINI check
#gini(customer_data[which(customer_data$Churn == "Yes"), ]$Contract)
#gini(customer_data[which(customer_data$Churn == "No"), ]$Contract)

#Split into test and training data
customer_data_train <- customer_data [1:7000,]
customer_data_test <- customer_data[7000:7032,]

#Proportions
prop.table(table(customer_data_train$Churn))
prop.table(table(customer_data_test$Churn))

#Cost Matrix
error_cost <- matrix(c(0, 1, 3, 0), nrow = 2)




      #MODELLING


  #C50 modelling
#Boosting
telco_model_c50 <- C5.0(Churn ~., data =customer_data_train)
telco_model_c50_cost_matrix <- C5.0(Churn ~., data =customer_data_train, costs = error_cost)
telco_model_c50_10 <- C5.0(Churn ~., data =customer_data_train, trials=10)

#Summaries
summary(telco_model_c50)
summary(telco_model_c50_10)
summary(telco_model_c50_cost_matrix)


  #CART modelling
telco_model_cart <- rpart(Churn ~., data=customer_data_train)
summary(telco_model_cart)

    #EVALUATION OF MODEL


#Predict and evaluate c5.0 
predict_telco_model_c50 <- predict(telco_model_c50, customer_data_test)
predict_telco_model_c50_cost_matrix <- predict(telco_model_c50_cost_matrix, customer_data_test)
predict_telco_model_c50_10 <- predict(telco_model_c50_10, customer_data_test)
predict_telco_model_cart <- predict(telco_model_cart, customer_data_test)

#plots
plot(predict_telco_model_c50)
plot(predict_telco_model_c50_cost_matrix)
plot(predict_telco_model_c50_10)
plot(predict_telco_model_cart)

#Cross tables
CrossTable(predict_telco_model_c50, customer_data_test$Churn,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('predicted default', 'actual default'))
CrossTable(predict_telco_model_c50_10, customer_data_test$Churn,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('predicted default', 'actual default'))
CrossTable(predict_telco_model_c50_cost_matrix, customer_data_test$Churn,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('predicted default', 'actual default'))
CrossTable(predict_telco_model_cart, customer_data_test$Churn,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('predicted default', 'actual default'))
