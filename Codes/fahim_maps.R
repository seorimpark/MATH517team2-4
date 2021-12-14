
library(ggplot2)
library(geobr)
library(stargazer)
library(cowplot)
library(stringr)
library(nominatim)
# devtools::install_github("hrbrmstr/nominatim")
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
data$manufacturer = str_to_title(iconv(data$manufacturer, to = "ASCII//TRANSLIT"))
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
external["Countries",] = str_to_title(external["Countries",])
external
stargazer(external)

# Comparing mild and serious accidents
map1 <- map(brazil_mild, title = "Brazilian Aeronautics Accidents")
map2 <- map(brazil_severe, title = "Brazilian Aeronautics Serious Incidents")
plot_grid(map1, map2)


#### Geolocating with Nominatim - TRY ####

# Geolocating
names = c("Neuchâtel", "EPFL")
names_ascii = iconv(names, to = "ASCII//TRANSLIT")
places = osm_search(names_ascii, key = "BEPERuyOgxAQ5JX0H7oOHfAFmyb4tBnx")
table = cbind(names, places[, c("lon", "lat", "display_name")])
table

# Map
leaflet(data = table) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(display_name), label = ~as.character(names))

#### Accidents per localization ####

# Preparing data before geolocating
brazil = data[data$fu != "***" & data$fu != "EX" & data$localization != "NÃO IDENTIFICADA",]
names = unique(brazil$localization)
names_ascii = str_to_title(iconv(names, to = "ASCII//TRANSLIT"))
names_ascii_brazil = paste(names_ascii, ", Brazil", sep = "")
length(names_ascii_brazil)

# # Geolocating
# places = osm_search(names_ascii_brazil, key = "BEPERuyOgxAQ5JX0H7oOHfAFmyb4tBnx")
# table = cbind(names_ascii, places[c("lon", "lat", "display_name")])
# names(table)[1] = "names"
# 
# # Saving or loading
#  saveRDS(table, file = "localization.rds")
table = readRDS(file = "localization.rds")

# Check
names(table)
dim(table)
head(table[,1:3])

# Map check
leaflet(data = table) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(display_name), label = ~as.character(names))

# Map check cluster
leaflet(data = table) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(display_name), label = ~as.character(names), 
             clusterOptions = markerClusterOptions())

# Adding location
brazil$localization = str_to_title(iconv(brazil$localization, to = "ASCII//TRANSLIT"))
(n = length(brazil$localization))
loc = 0

for (i in 1:n) {
  loc = rbind(loc, table[table$names==brazil$localization[i], c("lon", "lat", "display_name")])
}

brazil = cbind(brazil, loc[-1,])
dim(brazil)
names(brazil)
head(brazil)

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





