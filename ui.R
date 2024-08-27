library(shiny)
library(DT)

# Define UI for application

fluidPage(
  
  # Application title
  titlePanel("National Parks"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("park","select a park:",
                  choices = setNames(selected_parks$PARKCODE,selected_parks$fullName)),
      
      sliderInput("date_slider", "Date Range:",
                  min = min(Incidents_date_choices), max(Incidents_date_choices),
                  value = c(min(Incidents_date_choices), max(Incidents_date_choices)),
                  step = 1),

      selectInput("min_date",
                  label = "Date From",
                  choices = Date_choices),
      
      selectInput("incident_dates","select an event:",
                  choices = NULL),
      
      # selectInput("range", "Select a range:",
      #             choices = unique(Incidents_date_choices),
      #             selected = c(min(Incidents_date_choices), max(Incidents_date_choices)),
      #             multiple = TRUE),

    ),
    
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Main",
                           fluidRow(
                             leafletOutput("mapplot")),
                           fluidRow(
                             column(6,plotOutput("linePlot")),
                             column(6, plotOutput("linePlot2"))
                           ),
                           fluidRow(
                             textOutput("ARIMA-Summary")
                           ),
                           
                           # fluidRow(
                           # DT::dataTableOutput("filtered_data")
                           # ),
                           
                  ),
                  tabPanel("Forecast",
                           plotOutput(
                             "forecastPlot"
                           ))
      )
    )
  )
)