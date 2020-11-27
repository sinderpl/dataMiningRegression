med_gpa <- read.csv("data/MedGPA.csv", strip.white=TRUE)
readable_column_names <- c("Acceptance", "Bio Chem,\n Physics, Maths", "GPA", "Verbal \n Reasoning",
                          "Physical \n Science", "Writing \n sample",  "MCAT", "Chance of accept")
#, "Bio\n Science"

# Threshold for model certainty of acceptance
acceptance_certainity <- .55

# We drop the non numeric / irrelevant variables for pairs generation
updated_med_gpa <- subset(med_gpa, select = -c(X,Accept, Sex, Apps, BS))
#Clean data, ignoring any rows with missing values NA
updated_med_gpa <- updated_med_gpa[complete.cases(updated_med_gpa),]

#Split the data into half for generation and half for testing
#len_gpa <- length(updated_med_gpa$Acceptance)
#updated_med_gpa_one <- updated_med_gpa[1 : floor(len_gpa/2),]
#updated_med_gpa_two <- updated_med_gpa[len_gpa/2+1 : len_gpa,]
#updated_med_gpa_two <- updated_med_gpa_two[complete.cases(updated_med_gpa_two),]

# Graph scatterplots for all the variables to show line of best fit
# I also use more readable names 
pairs( updated_med_gpa, labels=readable_column_names, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})

med_gpa_model <- lm(updated_med_gpa$Acceptance ~.,data=updated_med_gpa)
med_gpa_model$coefficients
summary(med_gpa_model)
new_data <- data.frame(BCPM= c(3.61, 2.5, 4.0),GPA= c(3.8,2.4,3.9), VR= c(12, 8, 5), PS= c(13),WS= c(9),BS= c(15), MCAT= c(40))
predicted_gpa_model <- round(predict(med_gpa_model, updated_med_gpa), digits = 2)

updated_med_gpa$percentage.chance = predicted_gpa_model

predicted_gpa_model[predicted_gpa_model >= 1.0] <- 1.0
predicted_gpa_model[predicted_gpa_model <= 0.0] <- 0.0
pairs( updated_med_gpa, labels=readable_column_names, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})


# We treat the predictions as percentages using less than 50% acceptance chance as declined
# anything above that is considered accepted
predicted_gpa_model[predicted_gpa_model >= acceptance_certainity] <- 1
predicted_gpa_model[predicted_gpa_model < acceptance_certainity] <- 0

#We will now compare this to the original data to check the accuracy
model_accurate <- updated_med_gpa$Acceptance == predicted_gpa_model

sprintf( "Model accuracy: %.2f%s",(sum(model_accurate) * 100 ) / length(updated_med_gpa$Acceptance),"%")
#cor(updated_med_gpa)
