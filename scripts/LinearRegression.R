
#Read in data and clean unnecessary columns  and null values
machine_information <- read.csv("data/machine.data", strip.white=TRUE)
#machine_information <- subset(machine_information, select = -c(adviser,X32.60))
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

pairs( machine_information, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='red')
})

machine_information_model <- lm(machine_information$Published.performace ~
                    
                    #.
                    +Machine.cycle.time
                    #
                    +Machine.memory.max
                    +Machine.memory.min
                    +Machine.memory.max*Machine.memory.min
                    #+Machine.memory.max:Machine.memory.min
                    +Machine.memory.cache
                    +Machine.channels.max
                    +Machine.channels.min
                    +Machine.channels.max*Machine.channels.min
                    #+Machine.channels.max:Machine.channels.min
                    -Estimated.performance #ignore the hardcoded predictions
                    #-Estimated.My.performance
                    ,data=machine_information)
machine_information_model$coefficients
summary(machine_information_model)

predicted_machine_model <- round(predict(machine_information_model, machine_information)) 

machine_information$Estimated.My.performance = predicted_machine_model

pairs( machine_information, panel=function(x,y){
  points(x, y)
  model <- lm(x ~ y)
  abline(model, col='blue')
})

#par(mfrow = c(2, 2))
#plot(predicted_machine_model)
