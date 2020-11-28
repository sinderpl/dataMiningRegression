med_gpa <- read.csv("data/machine.data", strip.white=TRUE)
updated_med_gpa <- subset(med_gpa, select = -c(adviser,X32.60))
updated_med_gpa <- updated_med_gpa[complete.cases(updated_med_gpa),]

pairs( updated_med_gpa, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})

med_gpa_model <- lm(updated_med_gpa$X198 ~.,data=updated_med_gpa)
med_gpa_model$coefficients
summary(med_gpa_model)
