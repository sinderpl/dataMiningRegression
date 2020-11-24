med_gpa <- read.csv("data/MedGPA.csv", strip.white=TRUE)

# We drop the non numeric variables for pairs generation
updated_med_gpa = subset(med_gpa, select = -c(X,Accept, Sex)) #Delete

pairs(subset(med_gpa, select = -c(X,Accept, Sex)))
colnames(updated_med_gpa)
data_model <- lm(Acceptance~.,data=med_gpa)
data_model$coefficients
summary(data_model)
