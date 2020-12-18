library(ggplot2)
library(Metrics)
library(gmodels)
library("foreign")
library(factoextra)
#utilises the power of kMean Classification to try to classify customers within a shopping mall


#PREPARATION

customer_data <- read.csv("data/Mall_Customers.csv", strip.white=TRUE, stringsAsFactors = FALSE)

#Check summaries and idenitify rows with null values
customer_data[1] <- NULL
customer_data[!complete.cases(customer_data),]
customer_data <- customer_data[complete.cases(customer_data),]
customer_data$Gender <- as.integer(as.factor(customer_data$Gender))

#Data summary and exploration
summary(customer_data)
table(customer_data$Spending.Score)
table(customer_data$Gender)
plot(customer_data$Age, customer_data$Spending.Score)
plot(customer_data$Spending.Score, customer_data$Annual.Income)

#classify
prop.table(table(customer_data$Spending.Score))
prop.table(table(customer_data$Gender))

set.seed(1)

#K means clustering 2 k value
model <- kmeans(customer_data, 2)
model
model$cluster
model$tot.withinss
model$centers

table(customer_data$Spending.Score, model$cluster)
plot(customer_data[c("Annual.Income", "Spending.Score", "Age", "Gender")], col = model$cluster)
points(model$centers[,c("Annual.Income", "Spending.Score", "Age", "Gender")], col = 1:3, pch = 8, cex=2)

fviz_cluster(model, data = customer_data[c("Annual.Income", "Spending.Score")])
fviz_cluster(model, data = customer_data)

#K means clustering 5 k value
model <- kmeans(customer_data, 5)
model
model$cluster
model$tot.withinss
model$centers

table(customer_data$Spending.Score, model$cluster)
plot(customer_data[c("Annual.Income", "Spending.Score")], col = model$cluster)
points(model$centers[,c("Annual.Income", "Spending.Score")], col = 1:3, pch = 8, cex=2)

fviz_cluster(model, data = customer_data[c("Annual.Income", "Spending.Score")])
fviz_cluster(model, data = customer_data)

#K means clustering 10 k value
model <- kmeans(customer_data, 10)
model
model$cluster
model$tot.withinss
model$centers

table(customer_data$Spending.Score, model$cluster)
plot(customer_data[c("Annual.Income", "Spending.Score", "Age")], col = model$cluster)
points(model$centers[,c("Annual.Income", "Spending.Score", "Age")], col = 1:3, pch = 8, cex=2)

fviz_cluster(model, data = customer_data[c("Annual.Income", "Spending.Score")])
fviz_cluster(model, data = customer_data)
