#library(moments) 

#Analyses computational power data and attempts to predict the total performance score.

#Read in data and clean unnecessary columns  and null values
machine_information <- read.csv("data/machine.data", strip.white=TRUE)
machine_information <- machine_information[complete.cases(machine_information),]
colnames(machine_information)<- c("Company",
                                  "Machine.name",
                                  "Machine.cycle.time",
                                  "Machine.memory.min",
                                  "Machine.memory.max",
                                  "Machine.memory.cache",
                                  "Machine.channels.min",
                                  "Machine.channels.max",
                                  "Published.performace",
                                  "Estimated.performance")
machine_information <- subset(machine_information, select = -c(Company, Machine.name))



#Leave out some data for testing
machine_information_test_data <-  machine_information[201:208, ]
machine_information <- machine_information[1:200, ]

#Initial analysis
summary(machine_information)
boxplot(machine_information)

#Quick look at the performance data distribution
hist(machine_information$Published.performace)

#Requires the "moments" library at the top
#kurtosis(machine_information$Published.performace)
#skewness(machine_information$Published.performace)

cor(machine_information$Published.performace,machine_information)
pairs( machine_information, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  # In case we want to display line of best fit
  #abline(model, col='red')
}, cex.labels=1)

#Modelling Linear Regresssion
#This is the final highest scoring model
machine_information_model <- lm(machine_information$Published.performace ~
                    +Machine.memory.max*Machine.memory.min
                    -Machine.memory.min
                    +Machine.memory.cache
                    +Machine.channels.max*Machine.channels.min
                    -Estimated.performance #ignore the hardcoded predictions
                    ,data=machine_information)
machine_information_model$coefficients
summary(machine_information_model)

# Second scoring model
machine_information_model2 <- lm(machine_information$Published.performace ~
                                +Machine.memory.max
                                +Machine.memory.min
                                +Machine.memory.cache
                                +Machine.channels.max
                                -Machine.channels.min
                                -Estimated.performance #ignore the hardcoded predictions
                                ,data=machine_information)
machine_information_model2$coefficients
summary(machine_information_model2)

# Third exploratory model
machine_information_model3 <- lm(machine_information$Published.performace ~
                                +Machine.memory.max:Machine.memory.min
                                +Machine.memory.cache
                                +Machine.channels.max:Machine.channels.min
                                -Estimated.performance #ignore the hardcoded predictions
                                ,data=machine_information)
machine_information_model3$coefficients
summary(machine_information_model3)

#Predicitions based on initial modell to compare to the baseline
predicted_machine_model <- round(predict(machine_information_model, machine_information_test_data))
machine_information_test_data$Estimated.My.performance = predicted_machine_model

#Evaluation

#Line of best fit again to see the patterns
pairs( machine_information_test_data, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})


#Residuals
performace_resid = resid(machine_information_model)
par(mfrow = c(2,2))
plot(machine_information_model, which=1)
plot(machine_information_model, which=2)
plot(machine_information_model, which=3)
plot(machine_information_model, which=5)