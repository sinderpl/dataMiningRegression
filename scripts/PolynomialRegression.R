
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

poly(machine_information$Published.performace, degree=3)
poly(machine_information$Published.performace, degree=3)