library(shiny)
library(DT)

# Define UI for the application
fluidPage(
  
  # Application title
  titlePanel("National Parks"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("park", "Select a Park:",
                  choices = setNames(selected_parks$PARKCODE, selected_parks$fullName)),
      
      sliderInput("date_slider", "Date Range:",
                  min = min(Incidents_date_choices), 
                  max = max(Incidents_date_choices),
                  value = c(min(Incidents_date_choices), max(Incidents_date_choices)),
                  step = 1),
      
      selectInput("min_date", "Date From:",
                  choices = Date_choices),
      
      selectInput("incident_dates", "Select an Event:",
                  choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Main",
                           fluidRow(
                             leafletOutput("mapplot", height = 400)
                           ),
                           fluidRow(
                             column(6, plotOutput("linePlot")),
                             column(6, plotOutput("linePlot2"))
                           ),
                           fluidRow(
                             textOutput("ARIMA-Summary")
                           )
                  ),
                  tabPanel("Forecast",
                           plotOutput("forecastPlot")
                  )
      )
    )
  )
)
