library(shiny)
library(tidyverse)
library(plotly)
library(lubridate) #for graphic
library(ggplot2) # for make_date()
library(dplyr)
library(purrr)
library(sf)
library(leaflet)

parks <- read.csv("data/park_visitor_data.csv")
parks <- parks|>
  mutate(across(-PARKCODE, ~as.integer(gsub(",", "", .))))
  
# nps <- read.csv('data/NPS-Unit-List.csv')
# nps <- nps |>
#   select(Name, Park.Code)|>
#   slice(1:427)
# nps <- na.omit(nps)

parks_info <- read.csv("data/park_info.csv")
parks_info <- parks_info |>
  select(fullName, parkCode)|>
  distinct()

parks_info$parkCode <- toupper(parks_info$parkCode)

#merge parks and nps to make a parks_n

parks_n <- parks |>
  left_join(parks_info, by = c("PARKCODE" = "parkCode"))


#saveRDS(parks_n,'data/parks_n.rds')
selected_parks <- parks_n %>%
  filter(YEAR == "2022")|>
  arrange(desc(TOTAL))|>
  slice_head(n=30)

#choices_dict <- setNames(parks_30$PARKCODE, parks_30$Name)
# 
park_choices <- parks |>
  pull(PARKCODE) |>
  unique()|>
  sort()

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

Date_choices<- parks_long %>%
  filter(format(Date, "%m") == "01") %>%
  pull(Date)

# park_fire_incidents_r <-read.csv('data/park_fire_incidents.csv', check.names = FALSE)
# 
# park_fire_incidents_r <- park_fire_incidents_r|>
#   mutate(parsed_datetime  = ymd_hms(FireDiscoveryDateTime, tz="UTC" ))
# park_fire_incidents_r <- park_fire_incidents_r |>
#   mutate(parsed_date  = as.Date(parsed_datetime))

park_fire_incidents_r <- readRDS('data/park_fire_incidents.rds')

park_boundary_m <- readRDS('data/park_boundary_m.rds')

Incidents_date_choices<- park_fire_incidents_r %>%
  filter(format(parsed_date, "%m") == "01") %>%
  pull(parsed_date)

