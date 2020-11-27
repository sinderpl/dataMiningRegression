med_gpa <- read.csv("data/MedGPA.csv", strip.white=TRUE)
readable_column_names <- c("Acceptance", "Bio Chem,\n Physics, Maths", "GPA", "Verbal \n Reasoning",
                          "Physical \n Science", "Writing \n sample", "Bio\n Science",  "MCAT")

# Threshold for model certainity of acceptance
acceptance_certainity = .55

# We drop the non numeric variables for pairs generation
updated_med_gpa = subset(med_gpa, select = -c(X,Accept, Sex, Apps))
#Clean data, ignoring any rows with missing values NA
updated_med_gpa = updated_med_gpa[complete.cases(updated_med_gpa),]

# Graph scatterplots for all the variables to show line of best fit
# I also use more readable names 
pairs( updated_med_gpa, labels=readable_column_names, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})

med_gpa_model <- lm(updated_med_gpa$Acceptance ~.,data=updated_med_gpa)
summary(med_gpa_model)
predicted_gpa_model = predict(med_gpa_model, scale = c[0,1] )

# We treat the predictions as percentages using less than 50% acceptance chance as declined
# anything above that is considered accepted
predicted_gpa_model[predicted_gpa_model >= acceptance_certainity] <- 1
predicted_gpa_model[predicted_gpa_model < acceptance_certainity] <- 0

#We will now compare this to the original data to check the accuracy
model_accurate <- updated_med_gpa$Acceptance == predicted_gpa_model

sprintf( "Model accuracy: %.2f%s",(sum(model_accurate) * 100 ) / length(updated_med_gpa$Acceptance),"%")
