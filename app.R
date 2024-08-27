# app.R

library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)
library(sf)
library(leaflet)
library(forecast)

# Load your data
parks <- read.csv("data/park_visitor_data.csv")
parks_info <- read.csv("data/park_info.csv")
park_fire_incidents_r <- readRDS('data/park_fire_incidents.rds')
park_boundary_m <- readRDS('data/park_boundary_m.rds')

# Process your data
parks <- parks %>%
  mutate(across(-PARKCODE, ~as.integer(gsub(",", "", .))))
parks_info <- parks_info %>%
  select(fullName, parkCode) %>%
  distinct()
parks_info$parkCode <- toupper(parks_info$parkCode)
parks_n <- parks %>%
  left_join(parks_info, by = c("PARKCODE" = "parkCode"))
selected_parks <- parks_n %>%
  filter(YEAR == "2022") %>%
  arrange(desc(TOTAL)) %>%
  slice_head(n = 30)
month_mapping <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
                   JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)

# Define UI
ui <- fluidPage(
  titlePanel("National Parks"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Select a park:",
                  choices = setNames(selected_parks$PARKCODE, selected_parks$fullName)),
      sliderInput("date_slider", "Date Range:",
                  min = min(park_fire_incidents_r$parsed_date), max = max(park_fire_incidents_r$parsed_date),
                  value = c(min(park_fire_incidents_r$parsed_date), max(park_fire_incidents_r$parsed_date)),
                  step = 1),
      selectInput("min_date", "Date From", choices = unique(park_fire_incidents_r$parsed_date)),
      selectInput("incident_dates", "Select an event:", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Main",
                           fluidRow(leafletOutput("mapplot")),
                           fluidRow(
                             column(6, plotOutput("linePlot")),
                             column(6, plotOutput("linePlot2"))
                           ),
                           fluidRow(textOutput("ARIMA-Summary"))
                  ),
                  tabPanel("Forecast", plotOutput("forecastPlot"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Place your server logic here, just like you did in the separate server.R
  # This includes reactive expressions, output rendering, etc.
  
  # For example:
  fire_incidence_filtered_data <- reactive({
    park_fire_incidents_r %>%
      filter(complete.cases(IncidentSize), IncidentSize > 100) %>%
      filter(UNIT_CODE == input$park, parsed_date >= input$min_date)
  })
  
  # And the rest of your server logic...
}

# Run the application 
shinyApp(ui = ui, server = server)
