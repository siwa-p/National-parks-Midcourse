library(shiny)
library(tidyverse)
library(plotly)
library(lubridate) # for graphic
library(sf)
library(leaflet)

# Read and process park visitor data
parks <- read.csv("data/park_visitor_data.csv") %>%
  mutate(across(-PARKCODE, ~as.integer(gsub(",", "", .))))

# Read and process park information
parks_info <- read.csv("data/park_info.csv") %>%
  select(fullName, parkCode) %>%
  distinct() %>%
  mutate(parkCode = toupper(parkCode))

# Merge parks and parks_info to create parks_n
parks_n <- parks %>%
  left_join(parks_info, by = c("PARKCODE" = "parkCode"))

# Select the top 30 parks by total visitors in 2022
selected_parks <- parks_n %>%
  filter(YEAR == 2022) %>%
  arrange(desc(TOTAL)) %>%
  slice_head(n = 30)

# Create choices for park codes
park_choices <- parks %>%
  pull(PARKCODE) %>%
  unique() %>%
  sort()

# Transform parks data to long format for plotting
parks_long <- parks %>%
  filter(PARKCODE == 'HOBE') %>%
  select(-X, -PARKCODE) %>%
  pivot_longer(cols = -YEAR, names_to = "Month", values_to = "Value")

# Create a numeric mapping for months
month_mapping <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
                   JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)

# Add numeric month and date columns
parks_long <- parks_long %>%
  mutate(
    MonthNumeric = month_mapping[Month],
    Date = make_date(YEAR, MonthNumeric)
  )

# Create choices for Date based on January months
Date_choices <- parks_long %>%
  filter(format(Date, "%m") == "01") %>%
  pull(Date)

# Load pre-processed data if available
park_fire_incidents_r <- readRDS('data/park_fire_incidents.rds')
park_boundary_m <- readRDS('data/park_boundary_m.rds')

# Create choices for Incident dates based on January months
Incidents_date_choices <- park_fire_incidents_r %>%
  filter(format(parsed_date, "%m") == "01") %>%
  pull(parsed_date)