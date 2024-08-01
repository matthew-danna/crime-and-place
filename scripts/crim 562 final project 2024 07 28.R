install.packages('leaflet')
library(leaflet)
library(tidyverse)
library(tigris)
library(sf)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)

##### 1. Get the data

arrests <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                            "1MomN2YEg1mVIBMXbnJHl9iGkYXmx5lnx"))
crimes <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                           "1bSfGCFzzbRVh9zStuwEXplj6GwiS1CHm"))
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1gniFP35GEfxycgpKGHAzkgSYmjTteTKm"))

##### 2. Subsetting the data
# subset by type
unique(calls$type)

type.calls <- subset(calls, calls$type == 'TRESPASSING' |
                       calls$type == 'DISORDERLY' |
                       calls$type == 'DIP' |
                       calls$type == 'LARCENY' |
                       calls$type == 'SUSPICIOUS' |
                       calls$type == 'WELFARE CHECK')

# subset by time
time.calls <- subset(type.calls, type.calls$year > 2022)

# subset by location
fc.calls <- subset(time.calls, time.calls$lat > 38.859298 & time.calls$lon > -77.283521)

# map for testing my subset
leaflet(fc.calls) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = "",
             clusterOptions = markerClusterOptions())

# subset for during the operation
calls.during <- subset(fc.calls, fc.calls$date > '2023-08-31' &
                         fc.calls$date < '2023-12-01')

##### 3. Mapping hotspots
# export to csv, load into ArcGIS, and map
write.csv(fc.calls, "/Users/matthewdanna/Downloads/fc.calls.csv", row.names = FALSE)

# mapping in R
# reference data
va.roads <- roads("VA", "Fairfax City")
va.outline <- county_subdivisions("VA", "Fairfax City")

# transparent points
ggplot() +
  geom_sf(data = va.outline) +
  geom_sf(data = va.roads) +
  geom_point(aes(x = lon, y = lat), data = fc.calls, color = "orange", alpha = 0.2)

# polygons
ggplot() +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = 0.01),  
                 size = 0.01, bins = 4, data = fc.calls, geom = "polygon") +
  theme(legend.position = "none")

# hexes
ggplot() +
  geom_hex(aes(x = lon, y = lat), data = fc.calls, bins = 15) +
  scale_fill_continuous(type = "viridis")

# add formatting
ggplot() +
  geom_sf(data = va.outline) +
  geom_hex(aes(x = lon, y = lat), data = fc.calls, bins = 15) +
  scale_fill_continuous(type = "viridis") +
  geom_sf(data = va.roads, color = "grey", linewidth = 0.25) +
  theme_bw() +
  ggtitle("title hereD")

ggplot() +
  geom_sf(data = va.outline) +
  geom_hex(aes(x = lon, y = lat), data = fc.calls, bins = 15) +
  scale_fill_continuous(type = "viridis") +
  geom_sf(data = va.roads, color = "grey", linewidth = 0.25) +
  theme_bw() +
  ggtitle("title here") +
  facet_wrap(~ source, nrow = 3)

# Facet map by year
ggplot() +
  geom_sf(data = va.outline) +
  geom_hex(aes(x = lon, y = lat), data = fc.calls, bins = 15) +
  scale_fill_continuous(type = "viridis") +
  geom_sf(data = va.roads, color = "grey", linewidth = 0.25) +
  theme_bw() +
  ggtitle("title here") +
  facet_wrap(~ year)

##### 4. t tests
# add a year month column
fc.calls$year.month <- substr(fc.calls$date,0,7)

# summarized by year month
sum.calls <- fc.calls %>%
  group_by(year.month) %>%
  summarise(call.count = n())

# bin before, during, after
sum.during <- subset(sum.calls, sum.calls$year.month > '2023-08' &
                       sum.calls$year.month < '2023-12')
sum.before <- subset(sum.calls, sum.calls$year.month > '2023-01' &
                       sum.calls$year.month < '2023-09')
sum.after <- subset(sum.calls, sum.calls$year.month > '2023-11')

### t tests
# Before to During
t.test(sum.before$call.count, sum.during$call.count, var.equal = TRUE)

# During to After
t.test(sum.during$call.count, sum.after$call.count, var.equal = TRUE)

# Before to After
t.test(sum.before$call.count, sum.after$call.count, var.equal = TRUE)

##### 5. percent changes
write.csv(sum.calls, "/Users/matthewdanna/Downloads/sum.calls.csv", row.names = FALSE)

##### 6. forecasting
# subset by location
model.calls <- subset(type.calls, type.calls$lat > 38.859298 & type.calls$lon > -77.283521)

model.calls$date <- as.Date(model.calls$date)

calls.day <- model.calls %>%
  group_by(date) %>%
  summarise(COUNT = n())

calls.day <- calls.day %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(date, label = T, week_start = 1), 
         MONTH = lubridate::month(date, label = T, abbr = F),
         WEEK = isoweek(date),
         DAY = day(date),
         YEAR = year(date))

calls.day <- replace(calls.day, is.na(calls.day), 0)

cleaned.data <- zoo(calls.day$COUNT, 
                    seq(from = as.Date(min(calls.day$date)), 
                        to = as.Date(max(calls.day$date)), by = 1))

summary(cleaned.data)
plot(cleaned.data)
title("Insert A Title Here") # this adds a title to your graph

stationary.data <- diff(cleaned.data)
plot(stationary.data)

adf.test(as.matrix(stationary.data)) 

arima.data <- auto.arima(stationary.data)
arima.data

month01 <- as.Date("2024-09-01")
month30 <- as.Date("2024-09-30")
start <- as.numeric(month01 - max(calls.day$date))
end <- as.numeric(month30 - max(calls.day$date))
days <- c(start:end)

forecast00 <- forecast(arima.data, h=(days[1]-1))
forecast00 <- round(sum(forecast00$upper[,2]),0)
forecast01 <- forecast(arima.data, h=days[1])
forecast01 <- round(sum(forecast01$upper[,2]),0)
forecast02 <- forecast(arima.data, h=days[2])
forecast02 <- round(sum(forecast02$upper[,2]),0)
forecast03 <- forecast(arima.data, h=days[3])
forecast03 <- round(sum(forecast03$upper[,2]),0)
forecast04 <- forecast(arima.data, h=days[4])
forecast04 <- round(sum(forecast04$upper[,2]),0)
forecast05 <- forecast(arima.data, h=days[5])
forecast05 <- round(sum(forecast05$upper[,2]),0)
forecast06 <- forecast(arima.data, h=days[6])
forecast06 <- round(sum(forecast06$upper[,2]),0)
forecast07 <- forecast(arima.data, h=days[7])
forecast07 <- round(sum(forecast07$upper[,2]),0)
forecast08 <- forecast(arima.data, h=days[8])
forecast08 <- round(sum(forecast08$upper[,2]),0)
forecast09 <- forecast(arima.data, h=days[9])
forecast09 <- round(sum(forecast09$upper[,2]),0)
forecast10 <- forecast(arima.data, h=days[10])
forecast10 <- round(sum(forecast10$upper[,2]),0)
forecast11 <- forecast(arima.data, h=days[11])
forecast11 <- round(sum(forecast11$upper[,2]),0)
forecast12 <- forecast(arima.data, h=days[12])
forecast12 <- round(sum(forecast12$upper[,2]),0)
forecast13 <- forecast(arima.data, h=days[13])
forecast13 <- round(sum(forecast13$upper[,2]),0)
forecast14 <- forecast(arima.data, h=days[14])
forecast14 <- round(sum(forecast14$upper[,2]),0)
forecast15 <- forecast(arima.data, h=days[15])
forecast15 <- round(sum(forecast15$upper[,2]),0)
forecast16 <- forecast(arima.data, h=days[16])
forecast16 <- round(sum(forecast16$upper[,2]),0)
forecast17 <- forecast(arima.data, h=days[17])
forecast17 <- round(sum(forecast17$upper[,2]),0)
forecast18 <- forecast(arima.data, h=days[18])
forecast18 <- round(sum(forecast18$upper[,2]),0)
forecast19 <- forecast(arima.data, h=days[19])
forecast19 <- round(sum(forecast19$upper[,2]),0)
forecast20 <- forecast(arima.data, h=days[20])
forecast20 <- round(sum(forecast20$upper[,2]),0)
forecast21 <- forecast(arima.data, h=days[21])
forecast21 <- round(sum(forecast21$upper[,2]),0)
forecast22 <- forecast(arima.data, h=days[22])
forecast22 <- round(sum(forecast22$upper[,2]),0)
forecast23 <- forecast(arima.data, h=days[23])
forecast23 <- round(sum(forecast23$upper[,2]),0)
forecast24 <- forecast(arima.data, h=days[24])
forecast24 <- round(sum(forecast24$upper[,2]),0)
forecast25 <- forecast(arima.data, h=days[25])
forecast25 <- round(sum(forecast25$upper[,2]),0)
forecast26 <- forecast(arima.data, h=days[26])
forecast26 <- round(sum(forecast26$upper[,2]),0)
forecast27 <- forecast(arima.data, h=days[27])
forecast27 <- round(sum(forecast27$upper[,2]),0)
forecast28 <- forecast(arima.data, h=days[28])
forecast28 <- round(sum(forecast28$upper[,2]),0)
forecast29 <- forecast(arima.data, h=days[29])
forecast29 <- round(sum(forecast29$upper[,2]),0)
forecast30 <- forecast(arima.data, h=days[30])
forecast30 <- round(sum(forecast30$upper[,2]),0)

forecast.list <- c(forecast00, forecast01, forecast02, forecast03, 
                   forecast04, forecast05, forecast06, forecast07, 
                   forecast08, forecast09, forecast10, forecast11, 
                   forecast12, forecast13, forecast14, forecast15, 
                   forecast16, forecast17, forecast18, forecast19, 
                   forecast20, forecast21, forecast22, forecast23, 
                   forecast24, forecast25, forecast26, forecast27, 
                   forecast28, forecast29, forecast30)

forecasts.tmp <- data.frame(c(start-1,days))
colnames(forecasts.tmp) <- "DAYS"
forecasts.tmp$ID <- seq.int(nrow(forecasts.tmp))
forecasts.tmp$DATE <- forecasts.tmp$DAYS + max(calls.day$date)
forecasts.tmp$TOTAL <- forecast.list
forecasts <- subset(forecasts.tmp, forecasts.tmp$ID > 1)
forecasts$ID <- seq.int(nrow(forecasts))
forecasts.tmp <- forecasts.tmp[c(2,4)]
names(forecasts.tmp) <- c("ID", "YESTERDAY")
forecasts <- forecasts %>% left_join(forecasts.tmp, by = "ID")

forecasts$TODAY <- forecasts$TOTAL - forecasts$YESTERDAY
forecasts <- forecasts[c(2,3,6)]

#####
#####
##### EXAMPLE FOR WORKING WITH ONE CALL TYPE COUNTING EVENTS BY DAY
##### FROM CLASS ON 8/1
#####

# get the data
calls <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",
                          "1gniFP35GEfxycgpKGHAzkgSYmjTteTKm"))

# format the date
calls$date <- as.Date(calls$date)

# subset to trespasses
calls.trespass <- subset(calls, calls$type == 'TRESPASSING')

# subset Fairfax Circle
calls.trespass.fc <- subset(calls.trespass, calls.trespass$lat > 38.859298 & 
                              calls.trespass$lon > -77.283521)
# export the data for ArcGIS
write.csv(calls.trespass.fc, "/Users/matthewdanna/Downloads/calls 2024 08 01.csv",
          row.names = FALSE)

# group by date
calls.trespass.fc.day <- calls.trespass.fc %>%
  group_by(date) %>%
  summarise(COUNT = n())

# filling in blank dates
calls.trespass.fc.day <- calls.trespass.fc.day %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  mutate(WEEKDAY = lubridate::wday(date, label = T, week_start = 1), 
         MONTH = lubridate::month(date, label = T, abbr = F),
         WEEK = isoweek(date),
         DAY = day(date),
         YEAR = year(date))

# replacing NAs with 0s
calls.trespass.fc.day <- replace(calls.trespass.fc.day, 
                                 is.na(calls.trespass.fc.day), 0)

# trespasses in Fairfax Circle DURING the operation
calls.trespass.fc.during <- subset(calls.trespass.fc.day, 
                                   calls.trespass.fc.day$date > '2023-08-31' &
                                     calls.trespass.fc.day$date < '2023-12-01')

# trespasses in Fairfax Circle BEFORE the operation
calls.trespass.fc.before <- subset(calls.trespass.fc.day, 
                                   calls.trespass.fc.day$date > '2023-01-31' &
                                     calls.trespass.fc.day$date < '2023-09-01')

# trespasses in Fairfax Circle AFTER the operation
calls.trespass.fc.after <- subset(calls.trespass.fc.day, 
                                  calls.trespass.fc.day$date > '2023-11-30')


ggplot(sum.calls, aes(x = year.month, y = call.count)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x= element_text(angle = 90)) +
  geom_vline()
  geom_vline(xintercept = '2023-09', colour = "blue") +
  geom_vline(xintercept = '2023-11', color = "red")

ggplot() +
  geom_bar(data = sum.calls, aes(x = year.month, y = call.count))
  geom_line(data = www, aes(x=, y =)) +
  geom_line()

