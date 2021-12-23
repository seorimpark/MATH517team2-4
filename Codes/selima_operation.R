
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("../Data/aircrafts_occurrences_merged.csv")
names(data)
dim(data)

# Be careful, there are 16 rows of difference between both csv file, so 16 individuals will 
# not have information about variables from "occurrences.csv". 

type_of_occurence = data$type.of.occurrence
unique(type_of_occurence)

type_operation = data$type_operation
unique(type_operation)

operation_phase = data$operation_phase
unique (operation_phase)

#Evolution of instruction flight in time : 
data_instruction = data [data$type_operation == 'INSTRUCTION',]
barplot(table(data_instruction$type.of.occurrence))
#we see that there are a lot of negligeable type of occurrence : 
table(data_instruction$type.of.occurrence)
table (table (data_instruction$type.of.occurrence)>5) #Only 10 more than 5 
v = table(data_instruction$type.of.occurrence)
v = v[v>5]
#severeg damages : 
unique (data_instruction$damage_level)
data_instruction$damage_level
data_instruction[19,]
dataI_destroyed = data_instruction[data_instructio == 'DESTROYED', ]
dataI_substantial = data_instruction[data_instruction == 'SUBSTANTIAL',] 

dim (dataI_destroyed)
dim(dataI_substantial) 
table (dataI_destroyed$type.of.occurrence)
table ( dataI_substantial$type.of.occurrence)
  
dataI_destroyed
  
  
  














#Can also work on the reports data, how much time it takes to report an accicdent, 
#does the severity of the accident is corrolated 
#How this change chnage in the year, with technologies is it more 
#depends on the  vehicule.. ? 