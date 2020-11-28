med_gpa <- read.csv("data/MedGPA.csv", strip.white=TRUE)
readable_column_names <- c("Acceptance", "Bio Chem,\n Physics, Maths", "GPA", "Verbal \n Reasoning",
                          "Physical \n Science", "Writing \n sample",  "MCAT", "Chance of accept")
# Threshold for model certainty of acceptance
acceptance_certainity <- .55

# We drop the non numeric / irrelevant variables for pairs generation
updated_med_gpa <- subset(med_gpa, select = -c(X,Accept, Sex, Apps))

#Clean data, ignoring any rows with missing values NA
updated_med_gpa <- updated_med_gpa[complete.cases(updated_med_gpa),]

# Graph scatterplots for all the variables to show line of best fit
# I also use more readable names 
pairs( updated_med_gpa, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})

# -BS -BCPM - PS - VR
#GPA + VR + PS + BS + MCAT
# GPA + VR + PS + BS - MCAT predicts well for test
med_gpa_model <- lm(updated_med_gpa$MCAT ~ .,data=updated_med_gpa)
med_gpa_model$coefficients
summary(med_gpa_model)

par(mfrow = c(2, 2))
plot(med_gpa_model)

#Residuals
gpa_resid = resid(med_gpa_model)
plot(fitted(med_gpa_model), gpa_resid) 
abline(h=0,lty=2)
plot(gpa_resid)
qqnorm(gpa_resid)

hist(gpa_resid, main="Histogram of Residuals",
     ylab="Residuals")


med_gpa_model$coefficients
summary(med_gpa_model)
new_data <- data.frame(Acceptance = c(1,0,1), BCPM= c(4.0, 2.5, 4.0),GPA= c(3.8,2.4,3.9), VR= c(12, 6, 5), PS= c(13, 8 ,10),WS= c(9, 6, 8),BS= c(15,12,13), MCAT= c(40,10,35))
predicted_gpa_model <- round(predict(med_gpa_model, new_data), digits = 2) * 100
predicted_gpa_model

#errors <- actual - predicted
#squared.errors <- errors ^ 2
#mse <- mean(squared.errors)
#rmse <- sqrt(mse)
#rmse

predicted_gpa_model[predicted_gpa_model >= 100] <- 100
predicted_gpa_model[predicted_gpa_model <= 0] <- 0

updated_med_gpa$percentage.chance = predicted_gpa_model

#updated_med_gpa$percentage.chance4[updated_med_gpa$percentage.chance4 >= acceptance_certainity] <- 1
#updated_med_gpa$percentage.chance4[updated_med_gpa$percentage.chance4 < acceptance_certainity] <- 0


#Graph chance of acceptance again to see the regression
#predicted_gpa_model[predicted_gpa_model >= 1.0] <- 1.0
#predicted_gpa_model[predicted_gpa_model <= 0.0] <- 0.0
pairs( new_data, labels=readable_column_names, panel=function(x,y){
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
