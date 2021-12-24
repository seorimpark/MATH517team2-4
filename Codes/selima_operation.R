
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

data <- read.csv("../Data/aircrafts_occurrences_merged.csv")
names(data)
dim(data)

# Be careful, there are 16 rows of difference between both csv file, so 16 individuals will 
# not have information about variables from "occurrences.csv". 

type_of_occurence = data$type.of.occurrence
unique(type_of_occurence)

type_operation = data$type_operation
unique(type_operation)

pie(data$type.of.occurrence)
#Categorise the type of operation : 
takeoff <- c("TIRE BURST","LOAD LAUNCH","LEAVING THE TRACK")
during<- c("COLLISION AGAINST OBSTACLE DURING THE FLIGHT","ENGINE FAILURE DURING THE FLIGHT", "LOSS OF CONTROL IN THE AIR",
         "FIRE DURING THE FLIGHT","METEOROLOGICAL PHENOMENOM IN THE AIR","AIR TRAFFIC","LOSS OF COMPONENT DURING THE FLIGHT",
         "COLLISION AGAINST BIRD","ABOUT PASSENGERS/CREW DURING THE FLIGHT","AIRCRAFTS COLLISION IN THE AIR","SPATIAL UNAWARENESS",
         "FLIGHT COMMANDS","COLLISION DURING THE FLIGHT AGAINST TOWED OBJECT","AIRCRAFT HIT BY OBJECT")
landing <-c("ABOUT LANDING GEAR","LANDING ON UNPREDICTABLE PLACE","EXPLOSIVE / NOT INTENTIONAL DECOMPRESSION","HARD LANDING","SLOW LANDING",
           "LANDING WITHOUT LANDING GEAR" ,"LANDING BEFORE THE TRACK AREA")
ground <-c("METEOROLOGICAL PHENOMENOM ON THE GROUND","COLLISION AGAINST OBSTACLE ON THE GROUND","LOSS OF CONTROL ON THE GROUND",
           "FIRE ON THE GROUND","COMPONENT LOSS ON THE GROUND","ENGINE FAILURE IN THE GROUND","VEHICLE COLLISION AGAINST AIRCRAFT",
           "TERRAIN COLLISION")
unknown<- c("ABOUT ROTOR","FOD - DAMAGE CAUSED BY UNKNOWN OBJECT","SMOKE IN THE CABIN","LOW ALTITUDE MANEUVERS" ,"STRUCTURAL FAILURE",
                "UNKNOWN","SYSTEM / COMPONENT FAILURE","ANOTHER TYPES","INVOLUNTARY ENGINE CUT OFF","ABOUT PROPELLER","FLUID LEAKS",
                "FUEL STARVATION","PHYSIOLOGICAL PROBLEMS","TRACK INCURSION","ABOUT WINDOWS / DOORS / WINDSHIELD")
data_takeoff <- data[data$type.of.occurrence %in% takeoff,]
dim(data_takeoff)

data_during <- data[data$type.of.occurrence %in% during,]
dim(data_during)

data_landing <- data[data$type.of.occurrence %in% landing,]
dim(data_landing)

data_ground <- data[data$type.of.occurrence %in% ground,]
dim(data_ground)

data_unknown <- data[data$type.of.occurrence %in% unknown,]
dim(data_unknown)


incident <- c("take off", "during the flight", "landing", "on the ground", "unknown")
dimension<- c(nrow(data_takeoff),nrow(data_during),nrow(data_landing),nrow(data_ground),nrow(data_unknown)) 
proportions <- data.frame(incident, dimension)
pie(proportions$dimension, labels = incident)

nb_accident = nrow (data_takeoff[data_takeoff$classification == "ACCIDENT",])
nb_serious = nrow (data_takeoff[data_takeoff$classification=="SERIOUS INCIDENT",])
takeoffserverity = data.frame (c(nb_accident,nb_serious), c("ACCIDENT", "SERIOUS INCIDENT"))
pie (takeoffserverity$c.nb_accident..nb_serious., labels = takeoffserverity$c..ACCIDENT....SERIOUS.INCIDENT..)

nb_accident = nrow (data_during[data_during$classification == "ACCIDENT",])
nb_serious = nrow (data_during[data_during$classification=="SERIOUS INCIDENT",])
takeoffserverity = data.frame (c(nb_accident,nb_serious), c("ACCIDENT", "SERIOUS INCIDENT"))
pie (takeoffserverity$c.nb_accident..nb_serious., labels = takeoffserverity$c..ACCIDENT....SERIOUS.INCIDENT..)

nb_accident = nrow (data_landing[data_landing$classification == "ACCIDENT",])
nb_serious = nrow (data_landing[data_landing$classification=="SERIOUS INCIDENT",])
takeoffserverity = data.frame (c(nb_accident,nb_serious), c("ACCIDENT", "SERIOUS INCIDENT"))
pie (takeoffserverity$c.nb_accident..nb_serious., labels = takeoffserverity$c..ACCIDENT....SERIOUS.INCIDENT..)

nb_accident = nrow (data_ground[data_ground$classification == "ACCIDENT",])
nb_serious = nrow (data_ground[data_ground$classification=="SERIOUS INCIDENT",])
takeoffserverity = data.frame (c(nb_accident,nb_serious), c("ACCIDENT", "SERIOUS INCIDENT"))
pie (takeoffserverity$c.nb_accident..nb_serious., labels = takeoffserverity$c..ACCIDENT....SERIOUS.INCIDENT..)

nb_accident = nrow (data_unknown[data_unknown$classification == "ACCIDENT",])
nb_serious = nrow (data_unknown[data_unknown$classification=="SERIOUS INCIDENT",])
takeoffserverity = data.frame (c(nb_accident,nb_serious), c("ACCIDENT", "SERIOUS INCIDENT"))
pie (takeoffserverity$c.nb_accident..nb_serious., labels = takeoffserverity$c..ACCIDENT....SERIOUS.INCIDENT..)

severity = data$classification
unique (severity)

data_accident = data[data$classification == "ACCIDENT",]
data_serious = data[data$classification == "SERIOUS INCIDENT",]

data_accident_takeoff=data_accident[data_accident$type.of.occurrence %in% takeoff,]
data_accident_during =data_accident[data_accident$type.of.occurrence %in% during,]
data_accident_landing=data_accident[data_accident$type.of.occurrence %in% landing,]
data_accident_ground=data_accident[data_accident$type.of.occurrence %in% ground,]
data_accident_unknown = data_accident[data_accident$type.of.occurrence %in% unknown,] 

incident <- c("take off", "during the flight", "landing", "on the ground", "unknown")
dimension<- c(nrow(data_accident_takeoff),nrow(data_accident_during),nrow(data_accident_landing),nrow(data_accident_ground)
              ,nrow(data_accident_unknown)) 
proportions <- data.frame(incident, dimension)
pie(proportions$dimension, labels = incident)

data_serious_takeoff=data_serious[data_serious$type.of.occurrence %in% takeoff,]
data_serious_during =data_serious[data_serious$type.of.occurrence %in% during,]
data_serious_landing=data_serious[data_serious$type.of.occurrence %in% landing,]
data_serious_ground=data_serious[data_serious$type.of.occurrence %in% ground,]
data_serious_unknown = data_serious[data_serious$type.of.occurrence %in% unknown,] 

incident <- c("take off", "during the flight", "landing", "on the ground", "unknown")
dimension<- c(nrow(data_serious_takeoff),nrow(data_serious_during),nrow(data_serious_landing),nrow(data_serious_ground),
              nrow(data_serious_unknown)) 
proportions <- data.frame(incident, dimension)
pie(proportions$dimension, labels = incident)

operation_phase = data$operation_phase
unique (operation_phase)


type_operation = unique (data$type_operation)



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
  
  
serious_destroyed = data_serious[data_serious$damage_level =="DESTROYED",]
serious_substantial = data_serious[data_serious$damage_level =="SUBSTANTIAL",]
serious_light = data_serious[data_serious$damage_level =="LIGHT",]
serious_none = data_serious[data_serious$damage_level == "NONE", ]
serious_unknown= data_serious[data_serious$damage_level == "UNKNOWN",]


incident <- c("DESTROYED", "SUBSTANTIAL", "LIGHT", "NONE", "UNKNOWN")
dimension<- c(nrow(serious_destroyed),nrow(serious_substantial),nrow(serious_light),nrow(serious_none),
              nrow(serious_unknown)) 
proportions <- data.frame(incident, dimension)
pie(proportions$dimension, labels = incident)



accident_destroyed = data_accident[data_accident$damage_level =="DESTROYED",]
accident_substantial = data_accident[data_accident$damage_level =="SUBSTANTIAL",]
accident_light = data_accident[data_accident$damage_level =="LIGHT",]
accident_none = data_accident[data_accident$damage_level == "NONE", ]
accident_unknown= data_accident[data_accident$damage_level == "UNKNOWN",]


incident <- c("DESTROYED", "SUBSTANTIAL", "LIGHT", "NONE", "UNKNOWN")
dimension<- c(nrow(accident_destroyed),nrow(accident_substantial),nrow(accident_light),nrow(accident_none),
              nrow(accident_unknown)) 
proportions <- data.frame(incident, dimension)
pie(proportions$dimension, labels = incident)




#Can also work on the reports data, how much time it takes to report an accicdent, 
#does the severity of the accident is corrolated 
#How this change chnage in the year, with technologies is it more 
#depends on the  vehicule.. ? 

