library(shiny)


function(input,output,session){
  
  output$linePlot <- renderPlot({
    parks |>
      filter(PARKCODE == input$park) |>
      select(-X,-PARKCODE)|>
      gather(key = "Month", value = "Value", -YEAR)|>
      mutate(MonthNumeric = month_mapping[Month])|>
      mutate(Date = make_date(YEAR, MonthNumeric))|>
      arrange(Date)|>
      filter(Date >= input$min_date) |> 
      ggplot(aes(x = Date, y = Value)) + 
      geom_point()
      
      
  })
}