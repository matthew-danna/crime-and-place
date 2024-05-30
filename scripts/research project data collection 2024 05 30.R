install.packages('tidyverse')
library(tidyverse)

##### tidycensus
# https://walker-data.com/tidycensus/articles/spatial-data.html
# 1.install and load
install.packages('tidycensus')
library(tidycensus)

# 2. Get an API key:
#  Go here: https://api.census.gov/data/key_signup.html
# Sign-up. Check your email. Get your API key.

# 3. Load API
census_api_key("YOUR API KEY GOES HERE", install = TRUE) # run once!
readRenviron("~/.Renviron")

# 4. Generate a table of all available variables
census.variables.2022 <- load_variables(2022, "acs5", cache = TRUE)

# 5. Query a variable for Fairfax City
fairfax.income.block <- get_acs(
  state = "VA",
  county = "Fairfax City",
  geography = "block group",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2022
)

# 6. Visualize the Census query
fairfax.income.block %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

write.csv(fairfax.income.block, "/Users/matthewdanna/Desktop/fairfax income.csv",
          row.names = FALSE)

##### Open Street Maps (OSM)
# https://rspatialdata.github.io/osm.html

# 1. install and load
install.packages('osmdata')
library(osmdata)

# 2. Find features
available_features()
available_tags("amenity")

# 3. Create a Fairfax study area
fairfax.bb <- getbb("Fairfax, VA")

# 4. Write a query
fairfax.fuel <- fairfax.bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "fuel") %>%
  osmdata_sf()

# 5. map data
ggplot() +
  geom_sf(data = fairfax.fuel$osm_points)

# 6. create a data frame, and subset to Virginia
fairfax.fuel.df <- as.data.frame(fairfax.fuel$osm_points)
fairfax.fuel.df <- subset(fairfax.fuel.df, fairfax.fuel.df$`addr:state` == 'VA')

# 7. create latitude and longitude fields
fairfax.fuel.df$geometry <- gsub("POINT ", "", fairfax.fuel.df$geometry)
fairfax.fuel.df <- separate(fairfax.fuel.df, geometry, into = c("LON", "LAT"), 
                            sep = ", ")
fairfax.fuel.df$LAT <- gsub("\\)", "", fairfax.fuel.df$LAT)
fairfax.fuel.df$LON <- gsub("c\\(", "", fairfax.fuel.df$LON)

write.csv(fairfax.fuel.df, "/Users/matthewdanna/Desktop/fuel.csv", row.names = FALSE)

# Tigris