
data1 = read.csv("aircrafts.csv")
names(data1)
dim(data1)
max(table(data1$model))
table(data1$type_operation)

data2 = read.csv("occurrences.csv")
names(data2)
dim(data2)
unique(data2$fu)

data <- merge(data1, data2, by="occurrence_id")
names(data)
dim(data)

#################################

# install.packages("geobr")
library(geobr)
library(ggplot2)

df <- utils::read.csv(system.file("extdata/br_states_lifexpect2017.csv", package = "geobr"), encoding = "UTF-8")

states <- read_state(year=2019)

states$name_state <- tolower(states$name_state)
df$uf <- tolower(df$uf)

# join the databases
states <- dplyr::left_join(states, df, by = c("name_state" = "uf"))

ggplot() +
  geom_sf(data=states, aes(fill=ESPVIDA2017), color= NA, size=.15) +
  labs(subtitle="Life Expectancy at birth, Brazilian States, 2014", size=8) +
  scale_fill_distiller(palette = "Blues", name="Life Expectancy", limits = c(65,80)) +
  theme_minimal() +
  theme_void()
