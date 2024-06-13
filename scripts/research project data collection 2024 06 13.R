############
############ LIBRARIES
############

install.packages('tidyverse')
install.packages('tidycensus')
install.packages('osmdata')
library(tidycensus)
library(tidyverse)
library(osmdata)

############
############ PLACE-BASED DATA
############

##### tidycensus
# https://walker-data.com/tidycensus/articles/spatial-data.html

# 1. Get an API key:
#  Go here: https://api.census.gov/data/key_signup.html
# Sign-up. Check your email. Get your API key.

# 2. Load API
census_api_key("YOUR API KEY GOES HERE", install = TRUE) # run once!
readRenviron("~/.Renviron")

# 3. Generate a table of all available variables
census.variables.2022 <- load_variables(2022, "acs5", cache = TRUE)

# 4. Query a variable for Fairfax City
fairfax.income.block <- get_acs(
  state = "VA",
  county = "Fairfax City",
  geography = "block group",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2022
)

# 5. Visualize the Census query
fairfax.income.block %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

write.csv(fairfax.income.block, "/Users/matthewdanna/Desktop/fairfax income.csv",
          row.names = FALSE)

##### Open Street Maps (OSM)
# https://rspatialdata.github.io/osm.html

# 1. Find features
available_features()
available_tags("amenity")

# 2. Create a Fairfax study area
fairfax.bb <- getbb("Fairfax, VA")

# 3. Write a query
fairfax.fuel <- fairfax.bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "fuel") %>%
  osmdata_sf()

# 4. map data
ggplot() +
  geom_sf(data = fairfax.fuel$osm_points)

# 5. create a data frame, and subset to Virginia
fairfax.fuel.df <- as.data.frame(fairfax.fuel$osm_points)
fairfax.fuel.df <- subset(fairfax.fuel.df, fairfax.fuel.df$`addr:state` == 'VA')

# 6. create latitude and longitude fields
fairfax.fuel.df$geometry <- gsub("POINT ", "", fairfax.fuel.df$geometry)
fairfax.fuel.df <- separate(fairfax.fuel.df, geometry, into = c("LON", "LAT"), 
                            sep = ", ")
fairfax.fuel.df$LAT <- gsub("\\)", "", fairfax.fuel.df$LAT)
fairfax.fuel.df$LON <- gsub("c\\(", "", fairfax.fuel.df$LON)

write.csv(fairfax.fuel.df, "/Users/matthewdanna/Desktop/fuel.csv", row.names = FALSE)

# Tigris

############
############ CRIMINAL-EVENT DATA
############

# 1. Get data
arrests <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "1YdW0ov1OIm9MNmPLIomfpAy8RO4p6gxG"))
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "1X48tHqgfDVTbPTQ4wKxVPFazLyGjOfxz"))
crashes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "118xvIlrqOQFsrPVh35CvPmPkNXElenK1"))
crime <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "1Sg7lkf6EioI87dZiGDsn03un8gxT4zw6"))

# 2. Summaries
summary.calls <- calls %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count/sum(count)*100,2))

summary.crime <- crime %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count/sum(count)*100,2))

summary.arrests <- arrests %>%
  group_by(crime.code.description) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count/sum(count)*100,2))

summary.crashes <- crashes %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count/sum(count)*100,2))

# 3. Select/filter for your activity type
### using larceny as an example
subset.calls <- subset(calls, calls$type == 'LARCENY' | calls$type == 'SHOPLIFTER')

subset.crime <- subset(crime, crime$type == 'Larceny Report' |
                         crime$type == 'Larceny in Progress' |
                         crime$type == 'Shoplifter Report' |
                         crime$type == 'Shoplifte Resisting' |
                         crime$type == 'Shoplifter in Custody')

subset.arrests <- subset(arrests, arrests$crime.code.description == 'LARCENY:OTHER' |
                           arrests$crime.code.description == 'SHOPLIFTING')

# 4. add a dataset column
subset.calls$source <- "Calls"
subset.crime$source <- "Crime"
subset.arrests$source <- "Arrests"

# 5. remove extra columns
subset.new.calls <- subset.calls[c(1:4,6:10)]
subset.new.crime <- subset.crime[c(1:2,6,11:12,15:18)]
subset.new.arrests <- subset.arrests[c(1:2,5,7:8,10:13)]

# 6. rename common columns
colnames(subset.new.calls)
colnames(subset.new.crime)
colnames(subset.new.arrests)

names(subset.new.crime) <- c("lat", "lon", "type", "date", "time", "hour", "year", 
                             "ID", "source")
names(subset.new.arrests) <- c("lat", "lon", "type", "date", "time", "hour", "year",
                               "ID", "source")

# 7. merge the tables together
event.data <- rbind(subset.new.calls, subset.new.crime, subset.new.arrests)

### If you wanted to analyze traffic-event data, you would probably incorporate:
# calls for traffic stops, accident-no injury, disabled, tow, etc.....
# the entire crashes dataset
# crimes for Hit & Run, Traffic Stop, etc....
# arrests for DUI

# 8. Exporting!
# Mac (replace YOUR USER NAME with your actual computer's user name)
write.csv(event.data, "/Users/YOUR USER NAME/Downloads/event.data.csv", row.names = FALSE)
# Windows (replace YOUR USER NAME with your actual computer's user name)
write.csv(event.data, "C:/Users/YOUR USER NAME/Downloads/events.data.csv", row.names = FALSE)


