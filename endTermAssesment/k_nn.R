library(ggplot2)
library(Metrics)
library(class)
library(gmodels)
#utilises the power of kNN Nearest Neighbour Classification to try to classify customers as possible risks
# of leaving the service provider.


#PREPARATION

customer_data <- read.csv("data/telco_churn.csv", strip.white=TRUE, stringsAsFactors = FALSE)

#Check summaries and idenitify rows with null values
customer_data[1] <- NULL
customer_data[!complete.cases(customer_data),]
customer_data <- customer_data[complete.cases(customer_data),]

#Recode categorical variables to integers 

customer_data = as.data.frame(lapply(customer_data[],as.factor))
customer_data = as.data.frame(lapply(customer_data[],as.integer))



#Data summary and exploration
summary(customer_data)
table(customer_data$Churn)


#Convert outcome to a factor
#customer_data$Churn <- as.factor(customer_data$Churn)

#classify
prop.table(table(customer_data$Churn))

# create normalization function
#normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
#customer_data$TotalCharges <- as.data.frame(lapply(customer_data$TotalCharges,normalize))
#customer_data$TotalCharges <-scale(customer_data$TotalCharges)

#Split into test and training data
customer_data_train <- customer_data [1:7000,]
customer_data_test <- customer_data[7000:7032,]
customer_data_train_labels <- customer_data[1:7000,20]
customer_data_test_labels <- customer_data[7000:7032,20]

#Predictions
customer_data_predictions <- knn(train = customer_data_train, test = customer_data_test, cl = customer_data_train_labels, k=2)
customer_data_predictions_k_10 <- knn(train = customer_data_train, test = customer_data_test, cl = customer_data_train_labels, k=10)
customer_data_predictions_k_30 <- knn(train = customer_data_train, test = customer_data_test, cl = customer_data_train_labels, k=30)

#Change labels back to yes / no
customer_data_test_labels <- ifelse(customer_data_test_labels == 1, "no", "yes")
customer_data_predictions <- ifelse(customer_data_predictions == 1, "no", "yes")
customer_data_predictions_k_10 <- ifelse(customer_data_predictions_k_10 == 1, "no", "yes")
customer_data_predictions_k_30 <- ifelse(customer_data_predictions_k_30 == 1, "no", "yes")


#Cross tabulation
CrossTable(customer_data_predictions, customer_data_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
CrossTable(customer_data_predictions_k_10, customer_data_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
CrossTable(customer_data_predictions_k_30, customer_data_test_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
