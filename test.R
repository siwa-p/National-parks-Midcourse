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



# parks_long %>% 
#   arrange(Date)|>
#   filter(YEAR >= 2012 & YEAR <= 2014)|>
#   ggplot(aes(x = Date, y = Value)) + 
#   geom_point()


fire_incidents <-read.csv('../data/Wildland_Fire_Incidents.csv')

#This bit of code is used to visualize fire incidents data in clusters on a map

# leaflet(data = fire_incidents|>
#           filter(IncidentSize>=1000)) |>  
#   addTiles() |> 
#   addMarkers(~X, 
#              ~Y,
#              clusterOptions = markerClusterOptions()
#   )


park_boundary <- st_read('../data/nps_boundary/nps_boundary.shp')


# 
# park_boundary |> 
#   ggplot() +
#   geom_sf() +
#   geom_point(data = fire_incidents |> 
#                filter(IncidentSize>=1000),
#              aes(x = X, y = Y),
#              size = 0.1)

#This bit of code, while it works perfectly, the data points are all merged at X=0
# The reason for this is the metric is different in the fire_incidents data (lat long in degrees)
# while it's in meters (pseudo-Mercator projection) in the boundary file.


#Use transform to transform park boundary to lat-long in degrees 
park_boundary_m <- st_transform(park_boundary, crs=4326)
park_boundary_m<-st_make_valid(park_boundary_m)

saveRDS(park_boundary_m, "../data/park_boundary_m.rds")
# Not sure why I needed this?

# make a geodataframe
fire_incidents_geo <- st_as_sf(fire_incidents,
                               coords = c('X', 'Y'),
                               crs = st_crs(park_boundary_m)
)


# Use a st_join to filter to fire_incidents inside the park boundary
park_fire_incidents <- st_join(fire_incidents_geo, park_boundary_m, join = st_within, left = FALSE)


#this bit of code will make a cluster plot of all the park_fire_incidents data along with 
# park boundary polygons on a leaflet.

leaflet()|>
  addTiles()|>
  addMarkers(data = park_fire_incidents|>
               filter(IncidentSize>1000), clusterOptions = markerClusterOptions(), popup = ~IncidentName)|>
  addPolygons(data = park_boundary_m, color = "blue", fillOpacity = 0.2, popup =~PARKNAME
  )

# make a new date column with dates of fire incidents

park_fire_incidents<- park_fire_incidents|>
  mutate(parsed_datetime  = ymd_hms(FireDiscoveryDateTime, tz="UTC" ))
park_fire_incidents<- park_fire_incidents|>
  mutate(parsed_date  = as.Date(parsed_datetime))

# save the park_fire_incidents into a csv 
saveRDS(park_fire_incidents, "../data/park_fire_incidents.rds")


# Histogram plot of fire size
# park_fire_incidents |>
#   filter(IncidentSize>1000) |>
#   filter(IncidentSize<100000)|>
#   ggplot(aes(x=IncidentSize))+
#   geom_histogram(binwidth = 1000, fill = "blue", color = "black", alpha = 0.7) +
#   labs(title = "Histogram of incident Size", x = "incident Size", y = "Frequency")

selected_fire_incidents <- park_fire_incidents_r %>%
    filter(complete.cases(IncidentSize), IncidentSize > 100)
#lets look at the distribution of this
selected_fire_incidents |>
  ggplot(aes(x=IncidentSize))+
  geom_histogram(binwidth = 5000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Wildfire Size", x = "Size in Acres", y = "Frequency")

parks_n_2 <- parks_n|>
  filter(PARKCODE == "GRSM") |>
  select(-X, -PARKCODE, -TOTAL)|>
  gather(key = "Month", value = "Value", -YEAR, -fullName)|>
  mutate(MonthNumeric = month_mapping[Month])|>
  mutate(Date = make_date(YEAR, MonthNumeric))|>
  arrange(Date)|>
  filter(Date > as.Date("2016-01-01")& Date< as.Date("2020-02-14"))
# parks_n_2|>
#   ggplot(aes(x = Date, y = Value))+
#   geom_point()
# date_diff <- as.numeric(difftime(as.Date("2016-01-01"), as.Date("2020-02-14"), units = "days"))
# 
# # Calculate frequency based on the number of days
# frequency <- abs(round(date_diff / 30))
visitation_data_1 <- ts(
  parks_n_2$Value, 
  start = c(year(as.Date("2016-01-01")),month(as.Date("2016-01-01"))),
  end = c(year(as.Date("2020-02-14")),month(as.Date("2020-02-14"))+1),
  frequency = 12)
seasonal_arima_model <- auto.arima(visitation_data_1)
seasonal_arima_forecast <- forecast(seasonal_arima_model, 48, level= c(80,95))
autoplot(seasonal_arima_forecast)

forecast_values <- forecast(seasonal_arima_model, 12)

forecast_df <- data.frame(
  date = seq(tail(parks_n_2$Date,1)+1, length.out = length(forecast_values$mean), by = "month"),
  values = forecast_values$mean
)

ggplot() +
  geom_line(data = parks_n_2, aes(x = Date, y = Value), color = "blue") +
  geom_line(data = forecast_df, aes(x = date, y = values), color = "red", linetype = "dashed") +
  labs(
    title = "Original Data with ARIMA Forecast",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal()


parks_n_2 <- parks_n|>
  filter(PARKCODE == "GRSM") |>
  select(-X, -PARKCODE, -TOTAL)|>
  gather(key = "Month", value = "Value", -YEAR, -fullName)|>
  mutate(MonthNumeric = month_mapping[Month])|>
  mutate(Date = make_date(YEAR, MonthNumeric))|>
  arrange(Date)|>
  filter(Date <= as.Date("2022-02-01"))
visitation_data_1 <- ts(
  parks_n_2$Value,
  start = c(2010, 01),
  end = c(2020, 01),
  frequency = 12)
seasonal_arima_model <- auto.arima(visitation_data_1)
seasonal_arima_forecast <- forecast(seasonal_arima_model, 12, level= c(90,95))

(seasonal_arima_forecast$mean - mean(visitation_data_1)) / sqrt(seasonal_arima_forecast$sigma2)

parks_n|>
  filter(PARKCODE == "GRSM") |>
  select(-X, -PARKCODE, -TOTAL)|>
  gather(key = "Month", value = "Value", -YEAR, -fullName)|>
  mutate(MonthNumeric = month_mapping[Month])|>
  mutate(Date = make_date(YEAR, MonthNumeric))|>
  arrange(Date)|>
  filter(Date >= as.Date("2010-01-01"))|>
  ggplot(aes(x=Date, y= Value))+
  geom_line()+
  geom_point()+
  labs(
    title = "Recreational monthly visitor data in Smokey Mountains",
    x = "Date",
    y = "Number of Visitors (in 100,000)"
  )+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-5)) +
  
  theme(
    axis.title = element_text(size = 14),  # Font size for axis titles
    axis.text = element_text(size = 12)    # Font size for tick labels
  )
