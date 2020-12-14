library(ggplot2)
library(Metrics) 
#utilises the power of decision trees to try to classify customers as possible risks
# of leaving the service provider.


customer_data <- read.csv("data/telco_churn.csv", strip.white=TRUE)
