library(tidyverse)
library(plotly)
library(lubridate) #for graphic
library(ggplot2) # for make_date()

library(sf)
library(leaflet)

parks_long <- parks |>
  filter(PARKCODE=='HOBE')|>
  select(-X,-PARKCODE)|>
  gather(key = "Month", value = "Value", -YEAR)

#create a map to make Month column numeric
month_mapping <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
                   JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)

parks_long <- parks_long %>%
  mutate(MonthNumeric = month_mapping[Month])

# Create a new 'Date' column
parks_long <- parks_long %>%
  mutate(Date = make_date(YEAR, MonthNumeric))



parks_long %>% 
  arrange(Date)|>
  filter(YEAR >= 2012 & YEAR <= 2014)|>
  ggplot(aes(x = Date, y = Value)) + 
  geom_point()


fire_incidents <-read.csv('../data/Wildland_Fire_Incidents.csv')

leaflet(data = fire_incidents|>
          filter(IncidentSize>=1000)) |>  
  addTiles() |> 
  addMarkers(~X, 
             ~Y,
             clusterOptions = markerClusterOptions()
  )


park_boundary <- st_read('../data/nps_boundary/nps_boundary.shp')



park_boundary |> 
  ggplot() +
  geom_sf() +
  geom_point(data = fire_incidents |> 
               filter(IncidentSize>=1000),
             aes(x = X, y = Y),
             size = 0.1)

fire_incidents_geo <- st_as_sf(fire_incidents,
                          coords = c('X', 'Y'),
                          crs = st_crs(park_boundary)
)


park_boundary |>
  ggplot() +
  geom_sf()+
  geom_sf(data = fire_incidents_geo)


park_fire_incidents <- st_join(fire_incidents_geo, park_boundary, join = st_within)

park_boundary |>
  ggplot() +
  geom_sf() +
  geom_sf(data = park_fire_incidents,
          aes(color = `IncidentTypeKind`))
