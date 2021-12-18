
library(ggplot2)
library(geobr)
library(stargazer)
library(cowplot)
library(snakecase)
library(stringi)
library(stringr)
library(ggmap)
library(nominatim)
library(leaflet)

data = read.csv("../Data/aircrafts_occurrences_merged.csv")
names(data)
dim(data)

# Select needed variables
data = data[, c("localization", "fu", "country", "aerodrome", "origin_flight", 
                "destination_flight", "registration_country", "classification", 
                "registration", "manufacturer", "model", "operation_phase", 
                "damage_level", "type.of.occurrence", "occurrence_day", "time")]
names(data)
dim(data)

# Check NAs
table(data$localization)
table(data$fu) # 2 NAs
data[data$fu == "***", "localization"] # 1x non identified et 1x international waters
table(data$country)

# Formatting
data$classification = tolower(data$classification)
table(data$classification)
unique(data$registration)
data$manufacturer[data$manufacturer == "***"] = "unknown"
unique(data$manufacturer)
data$model[data$model == "***"] = "unknown"
unique(data$model)
data$operation_phase = tolower(data$operation_phase)
data$operation_phase[data$operation_phase == ""]  = "unknown"
unique(data$operation_phase)
data$damage_level = tolower(data$damage_level)
unique(data$damage_level)
data$type.of.occurrence = tolower(data$type.of.occurrence)
unique(data$type.of.occurrence)
unique(data$occurrence_day)
unique(data$time)

# Too much NAs
table(data$aerodrome)
table(data$origin_flight)
table(data$destination_flight)
table(data$registration_country)


#### Accidents per state ####

# Comparing mild and serious accidents
brazil = data[data$fu != "***" & data$fu != "EX",]
brazil_mild = brazil[brazil$classification == "accident",]
brazil_severe = brazil[brazil$classification == "serious incident",]

# Map function
map <- function(brazil, title="Brazilian Aeronautics Accidents") {
  
  # Accidents per state
  brazil = data.frame(table(brazil$fu))
  colnames(brazil) = c("fu", "accidents")
  max = max(brazil$accidents)
  
  # Brazilian states
  states <- read_state(year=2019)
  
  # Join the databases
  states <- dplyr::left_join(states, brazil, by = c("abbrev_state" = "fu"))
  states$accidents[is.na(states$accidents)] = 0
  
  # Map
  ggplot() +
    geom_sf(data=states, aes(fill=accidents), color= "black", size=.15) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Accidents", 
                         limits=c(0,max)) +
    ggtitle(title) +
    theme_minimal() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
}

# Overall accidents
map(brazil)

# External accidents + NAs
external = t(data.frame(table(data[data$fu == "***" | data$fu == "EX", "country"])))
rownames(external) = c("Countries", "Accidents")
external
stargazer(external)

# Comparing mild and serious accidents
map1 <- map(brazil_mild, title = "Brazilian Aeronautics Accidents")
map2 <- map(brazil_severe, title = "Brazilian Aeronautics Serious Incidents")
plot_grid(map1, map2)


#### Geolocating with ggmap (Google API) - TRY ####

# API key
register_google(key = "") # Cf. Google Cloud Platform

# Geolocating
names = c("NEUCHÂTEL", "EPFL")
names_lower = str_to_title(names)
places = geocode(names_lower)
table = cbind(names_lower, places)
table

# Map
leaflet(data = table) %>% addTiles() %>%
  addMarkers(~lon, ~lat, label = ~as.character(names_lower))

#### Localizing ggmap + Nominatim ####

# Preparing data before geolocating
brazil = data[data$fu != "***" & data$fu != "EX" & data$localization != "NÃO IDENTIFICADA",]
names = unique(brazil$localization)
names_lower = to_title_case(names)
names_lower_brazil = paste(names_lower, ", Brazil", sep = "")
length(names_lower_brazil)

# # Geolocating ggmap
# places = geocode(names_lower_brazil)
# table = cbind(names_lower, places)
# names(table)[1] = "names"
# 
# # Saving or loading
# saveRDS(table, file = "localization_ggmap.rds")
# 
# # Loading ggmap and nominatim positions
# table = readRDS(file = "localization_ggmap.rds")
# table_nom = readRDS(file = "localization_nominatim.rds")[, 1:3]
# 
# # Order
# diff = table_nom[order(table_nom$names),]
# table = table[order(table$names),]
# 
# # Take difference > 1 for lon or lat
# diff[,c("lon", "lat")] = abs(diff[,c("lon", "lat")] - table[,c("lon", "lat")])
# diff = diff[diff$lon>1 | diff$lat>1,]
# dim(diff)
# diff
# 
# # Prepare to locate with Nominatim
# names_brazil = paste(diff$names, ", Brazil", sep = "")
# length(names_brazil)
# 
# # Geolocating Nominatim
# places = osm_search(names_brazil, key = "BEPERuyOgxAQ5JX0H7oOHfAFmyb4tBnx")
# table_change = cbind(diff$names, places[c("lon", "lat")])
# names(table_change)[1] = "names"
# 
# # Map check
# leaflet(data = table_change) %>% addTiles() %>%
#   addMarkers(~lon, ~lat, label = ~as.character(names))
# 
# # To change
# change = c("Alenquer", "Arroio Grande", "Jurua", "Nova Esperanca", "Novo Aripuana", 
#            "Novo Planalto", "Pedra Preta", "Prata", "Ponta De Pedras", "Pindorama", 
#            "Sao Paulo", "Sao Pedro", "Sao Felix Do Xingu", "Sao Sebastiao Da Boa Vista", 
#            "Senador Jose Porfirio")
# 
# length(change)
# 
# # Locate with ggmap
# places = geocode(paste(change, ", Brazil", sep = ""))
# change = cbind(change, places)
# names(change)[1] = "names"
# dim(change)
# change
# 
# # Map check
# leaflet(data = change) %>% addTiles() %>%
#   addMarkers(~lon, ~lat, label = ~as.character(names))
# 
# # Order
# table_change = table_change[order(table_change$names),]
# change = change[order(change$names),]
# 
# # Add changes from ggmap
# table_change[table_change$names %in% change$names, c("lon", "lat")] = change[, c("lon", "lat")]
# table_change
# 
# # Take names with changed location with accents
# names_change = table[stri_trans_general(str = table$names, id = "Latin-ASCII") %in% table_change$names, "names"]
# names_change
# 
# # Change postitions according to ggmap and Nominatim
# table[is.element(table$names, names_change), c("lon", "lat")] = table_change[, c("lon", "lat")]
# 
# Specific changes
# specific = osm_search("Goianira", key = "BEPERuyOgxAQ5JX0H7oOHfAFmyb4tBnx")
# table[table$names == "Goianira", c("lon", "lat")] = specific[c("lon", "lat")]
# 
# specific = geocode("Brasília, Brazil")
# table[table$names == "Brasília", c("lon", "lat")] = specific[c("lon", "lat")]
# 
# # Saving or loading
# saveRDS(table, file = "localization.rds")
table = readRDS(file = "localization.rds")

# Check
names(table)
dim(table)
head(table)

######################################

# Map check
leaflet(data = table) %>% addTiles() %>%
  addMarkers(~lon, ~lat, label = ~as.character(names))

# Map check cluster
leaflet(data = table) %>% addTiles() %>%
  addMarkers(~lon, ~lat, label = ~as.character(names), 
             clusterOptions = markerClusterOptions())

# Adding location
brazil$localization = to_title_case(brazil$localization)
(n = length(brazil$localization))
loc = 0

for (i in 1:n) {
  loc = rbind(loc, table[table$names==brazil$localization[i], c("lon", "lat")])
}

brazil = cbind(brazil, loc[-1,])
dim(brazil)
names(brazil)
head(brazil)

# Label + popup
label <- sprintf("%s<br/>%s<br/>%s<br/>%s",
                  brazil$model,
                  brazil$classification,
                  brazil$occurrence_day,
                  brazil$localization
                  ) %>% lapply(htmltools::HTML)

popup <- paste(sep = "<br/>", 
               paste("Model:", brazil$model),
               paste("Manufacturer:", brazil$manufacturer),
               paste("Registration:", brazil$registration),
               paste("Classification:", brazil$classification),
               paste("Damage level:", brazil$damage_level),
               paste("Operation phase:", brazil$operation_phase),
               paste("Type of occurrence:", brazil$type.of.occurrence),
               paste("Occurrence date:", brazil$occurrence_day),
               paste("Occurrence time:", brazil$time)
               )

# Map
cluster <- leaflet(data = brazil) %>% addTiles() %>% 
  addMarkers(~lon, ~lat, popup = popup, label = label, clusterOptions = markerClusterOptions())

# Brazil states
brazil_fu = data[data$fu != "***" & data$fu != "EX",]

# Accidents per state
brazil_fu = data.frame(table(brazil_fu$fu))
colnames(brazil_fu) = c("fu", "accidents")

# Brazilian states
states <- read_state(year=2019)

# Join the databases
states <- dplyr::left_join(states, brazil_fu, by = c("abbrev_state" = "fu"))
states$accidents[is.na(states$accidents)] = 0

# Map
labels <- sprintf(
  "<strong>%s</strong><br/>%g accidents",
  states$name_state, states$accidents
) %>% lapply(htmltools::HTML)

bins <- seq(0,500,100)
pal <- colorBin("YlOrRd", domain = states$accidents, bins = bins)

fu <- leaflet(states) %>% addTiles() %>% addPolygons(
  fillColor = ~pal(accidents),
  weight = 1,
  opacity = 1,
  color = "grey",
  dashArray = "3",
  fillOpacity = 0.4, 
  layerId = ~geom,
  highlightOptions = highlightOptions(
    weight = 3,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.6,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal, values = ~accidents, opacity = 0.7, title = "Accidents",
            position = "bottomright")

# Display
cluster
fu
fu %>% addMarkers(data = brazil, ~lon, ~lat, popup = popup, label = label, 
                  clusterOptions = markerClusterOptions())

# Cities with most accidents
cities = t(data.frame(sort(table(brazil$localization), decreasing = TRUE)[1:10]))
rownames(cities) = c("Cities", "Accidents")
cities
stargazer(cities)
