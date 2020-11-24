med_gpa <- read.csv("data/MedGPA.csv", strip.white=TRUE)
readable_column_names <- c("Acceptance", "Bio, Chem, Physics, Maths", "GPA", "Verbal \n Reasoning",
                          "Physical \n Science", "Writing \n sample", "Bio\n Science",  "MCAT", "Applications\n Made")
# We drop the non numeric variables for pairs generation
updated_med_gpa = subset(med_gpa, select = -c(X,Accept, Sex)) #Delete

pairs(subset(med_gpa, select = -c(X,Accept, Sex)), labels =readable_column_names)
colnames(updated_med_gpa)
data_model <- lm(Acceptance~.,data=med_gpa)
data_model$coefficients
summary(data_model)
