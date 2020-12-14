#library(moments) 
library(ggplot2)
library(Metrics) 
#Analyses computational power data and attempts to predict the total performance score.
#Uses polynomial regression

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
#Add noise to the data
#machine_information$Published.performace = round(machine_information$Published.performace +  rnorm(length(machine_information$Published.performace), mean=10, sd=20))
#plot(machine_information$Machine.cycle.time,machine_information$Published.performace,col='deepskyblue4',xlab='q',main='Observed data')

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

#Exploration
performance <- data.frame()
for (d in 1:8){
  poly.fit <- lm(machine_information$Published.performace ~
                   polym(
                     +Machine.memory.max+Machine.memory.min
                     -Machine.memory.min
                     +Machine.memory.cache 
                     +Machine.channels.max*Machine.channels.min
                     #-Estimated.performance #ignore the hardcoded predictions
                     ,degree=d)
                 ,data=machine_information)
  performance <- rbind(performance,data.frame(Degree = d,Data = 'Training',RMSE = rmse(machine_information_test_data$Published.performace, predict(poly.fit, machine_information_test_data)))) 
}
ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) + geom_point() +geom_line()

#Modelling Polynomial REgression
machine_information$Published.performace <-
machine_information_model <- lm(machine_information$Published.performace ~
                                polym(
                                +Machine.memory.max*Machine.memory.min
                                -Machine.memory.min 
                                +Machine.memory.cache 
                                +Machine.channels.max*Machine.channels.min
                                ,degree=5)
                                ,data=machine_information)
machine_information_model$coefficients
rnorm(machine_information_model)
summary(machine_information_model)S

# Top scoring model
machine_information_model2 <- lm(machine_information$Published.performace ~
                                polym(
                                 +Machine.memory.max
                                 +Machine.memory.min
                                 +Machine.memory.cache
                                 +Machine.channels.max
                                 -Machine.channels.min
                                 -Estimated.performance #ignore the hardcoded predictions
                                 ,degree=4),data=machine_information)
machine_information_model2$coefficients
rnorm(machine_information_model2)
summary(machine_information_model2)

#COmpare the two models
anova(machine_information_model, machine_information_model2)

#Predicitions based on initial modell to compare to the baseline
predicted_machine_model <- round(predict(machine_information_model2, machine_information_test_data))
machine_information_test_data$Estimated.My.performance = predicted_machine_model

#Evaluation

#Residuals
performace_resid = resid(machine_information_model)
par(mfrow = c(2,2))
plot(machine_information_model, which=1)
plot(machine_information_model, which=2)
plot(machine_information_model, which=3)
plot(machine_information_model, which=5)