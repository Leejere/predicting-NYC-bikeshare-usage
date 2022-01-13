# Libraries####
library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(gganimate)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(FNN)

library(spdep)
library(caret)
library(ckanr)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(scales)
# Setup####
options(tigris_class = "sf")
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
crs = 'EPSG:2263'
census_api_key("b33ec1cb4da108659efd12b3c15412988646cbd8", overwrite = TRUE)

windowsFonts(font = windowsFont('Open Sans'))
mapTheme = function(base_size = 9, title_size = 8) {
  theme(
    text = element_text(family = 'font', color = "black"),
    plot.title = element_text(family = 'font', size = title_size,colour = "black"),
    plot.subtitle = element_text(family = 'font', face="italic"),
    plot.caption=element_text(family = 'font', hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=.2),
    strip.text.x = element_text(family = 'font', size = 10),
    legend.text = element_text(family = 'font', size=6),
    legend.title = element_text(family = 'font', size=7),
    legend.background = element_blank(),
    legend.key.size = unit(.3, 'line'))
}

plotTheme = function(base_size = 9, title_size = 10){
  theme(
    text = element_text(family = 'font', color = "black"),
    plot.title = element_text(family = 'font',
                              size = title_size, colour = "black", hjust = 0.5), 
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(family = 'font', hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=.5),
    strip.background = element_blank(),
    strip.text = element_text(family = 'font', size=9),
    axis.title = element_text(family = 'font', size=9),
    axis.text = element_text(family = 'font', size=9),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(family = 'font', colour = "black", face = "italic"),
    legend.text = element_text(family = 'font', colour = "black", face = "italic"),
    strip.text.x = element_text(family = 'font', size = 9),
    legend.key.size = unit(.3, 'line')
  )
}

color1 = '#F3F3F3'
color2 = '#FC6D5C'
color3 = '#31649D'

color4 = '#0AA400'

# Data wrangling####
citibikeRaw = read.csv('citibikeRaw.csv') 

citibikeInterval = citibikeRaw %>%
  filter(., rideable_type == 'classic_bike') %>%
  mutate(startHour = ymd_h(substr(started_at, 1, 13)),
         endHour = ymd_h(substr(ended_at, 1, 13))) %>%
  dplyr::select(startHour, endHour,
                start_station_id, start_lat, start_lng,
                end_station_id, end_lat, end_lng)

# All the stations
stations = rbind(
  dplyr::select(citibikeInterval, 
                station = start_station_id, 
                lat = start_lat, 
                lng = start_lng),
  dplyr::select(citibikeInterval,
                station = end_station_id,
                lat = end_lat,
                lng = end_lng)
) %>%
  group_by(station) %>%
  summarize(lat = first(lat),
            lng = first(lng)) %>%
  filter(., station != '') %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326, agr = 'constant') %>%
  st_transform(crs)


# All the hours
hours = rbind(
  dplyr::select(citibikeInterval,
                hour = startHour),
  dplyr::select(citibikeInterval,
                hour = endHour)
) %>%
  group_by(hour) %>%
  summarize(hour = first(hour)) %>%
  mutate(hour = as.POSIXct(hour)) %>%
  filter(., hour < as.Date('2021-11-02'))

# Empty panel
panel = 
  expand.grid(hour = hours$hour,
              station = stations$station) %>%
  left_join(stations, by = 'station') %>%
  st_sf() %>%
  mutate(week = week(hour),
         dayOfWeek = wday(hour, label = T))

aggByStart = citibikeInterval %>%
  group_by(startHour, start_station_id) %>%
  summarize(count = n()) %>%
  rename(hour = startHour, station = start_station_id,
         starts = count) %>%
  filter(., station != '')

aggByEnd = citibikeInterval %>%
  group_by(endHour, end_station_id) %>%
  summarize(count = n()) %>%
  rename(hour = endHour, station = end_station_id,
         ends = count) %>%
  filter(., station != '')

ridePanel = panel %>%
  left_join(aggByStart,
            by = c('hour' = 'hour',
                   'station' = 'station')) %>%
  left_join(aggByEnd,
            by = c('hour' = 'hour',
                   'station' = 'station')) %>%
  replace(is.na(.), 0)

# Weather####

weatherNetworks = riem_networks()
weatherStations = riem_stations(network = "NY_ASOS")

weather = riem_measures(
  station = 'NYC',
  date_start = '2021-10-01',
  date_end = '2021-11-01'
) %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(hour = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(hour),
         dayOfWeek = wday(hour, label = T)) %>%
  group_by(hour) %>%
  summarize(temp = max(tmpf),
            percip = sum(p01i),
            windSpeed = max(sknt)) %>%
  mutate(temp = ifelse(temp == 0, 42, temp)) %>%
  mutate(percip = ifelse(percip > 0.2, 'Rain/Snow', 'None'))

rideWeatherPanel = 
  ridePanel %>%
  left_join(weather, by = 'hour') %>%
  mutate(temp = ifelse(is.na(temp), mean(weather$temp), temp),
         percip = ifelse(is.na(percip), 'None', percip),
         windSpeed = ifelse(is.na(windSpeed), 0, windSpeed))

# Spatial information of stations####
nyWAC = read.csv('nyWAC.csv') %>%
  mutate(GEOID = as.character(w_geocode) %>%
           substr(1, 11)) %>%
  dplyr::select(GEOID, jobs = C000) %>%
  group_by(GEOID) %>%
  summarize(jobs = sum(jobs, na.rm = T))

nyRAC = read.csv('nyRAC.csv') %>%
  mutate(GEOID = as.character(h_geocode) %>%
           substr(1, 11)) %>%
  dplyr::select(GEOID, homes = C000) %>%
  group_by(GEOID) %>%
  summarize(homes = sum(homes, na.rm = T))

nyTracts = tracts(state = 'NY',
                  county = c('Bronx', 'Kings', 'New York', 'Queens', 'Richmond')) %>%
  st_transform(crs)

stationsInfo = stations %>%
  st_join(nyTracts, join = st_within) %>%
  mutate(cvID = sample(100, size = nrow(stationsInfo), replace = T))

rideWeatherInfoPanel = rideWeatherPanel %>%
  left_join(st_drop_geometry(stationsInfo), by = 'station') %>%
  rename(tract = GEOID)

# Time lag####
finalPanel = 
  rideWeatherInfoPanel %>%
  arrange(station, hour) %>%
  group_by(station) %>%
  mutate(startsLag1Hour = dplyr::lag(starts, 1),
         startsLag2Hours = dplyr::lag(starts, 2),
         startsLag3Hours = dplyr::lag(starts, 3),
         endsLag1Hour = dplyr::lag(ends, 1),
         endsLag2Hours = dplyr::lag(ends, 2),
         endsLag3Hours = dplyr::lag(ends, 3)) %>%
  ungroup()
