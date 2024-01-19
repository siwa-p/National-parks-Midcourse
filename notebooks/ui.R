library(shiny)


# Define UI for application

fluidPage(
  
  # Application title
  titlePanel("National Parks"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("park",
                  label = 'select a park',
                  choices = park_choices),
      selectInput("min_date",
                  label = "Date From",
                  choices = Date_choices)
    ),
    
    mainPanel(
      plotOutput("linePlot")
    )
  )
)