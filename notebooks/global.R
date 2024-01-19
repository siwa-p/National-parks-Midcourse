library(shiny)
library(tidyverse)
library(plotly)
library(lubridate) #for graphic
library(ggplot2) # for make_date()
library(dplyr)
library(purrr)

parks <- read.csv("../data/park_visitor_data.csv")
parks <- parks|>
  mutate(across(-PARKCODE, ~as.integer(gsub(",", "", .))))
  

parks_classes <- map_df(parks,class)

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
