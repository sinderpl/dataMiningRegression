credit_data <- read.table("data/SouthGermanCredit.asc", header=TRUE) 
pairs(credit_data)
colnames(credit_data)
data_model <- lm(kredit~.,data=credit_data)
data_model$coefficients
summary(data_model)
