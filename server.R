library(shiny)
library(forecast)

function(input,output,session){

  fire_incidence_filtered_data <- reactive({
    park_fire_incidents_r %>%
      filter(complete.cases(IncidentSize), IncidentSize > 100)|>
      filter(UNIT_CODE == input$park, parsed_date >= input$min_date)
  })
  observe({
    new_filtered_data <- fire_incidence_filtered_data()|>
      arrange(desc(parsed_date))
    updateSelectInput(session, "incident_dates", choices = new_filtered_data$parsed_date)
  })
  
  vline_position <- reactive({
    fire_incidence_filtered_data()$parsed_date
  })
  
  output$linePlot <- renderPlot({
    ggplot(parks_n|>
             filter(PARKCODE == input$park) |>
             select(-X, -PARKCODE, -TOTAL)|>
             gather(key = "Month", value = "Value", -YEAR, -fullName)|>
             mutate(MonthNumeric = month_mapping[Month])|>
             mutate(Date = make_date(YEAR, MonthNumeric))|>
             arrange(Date)|>
             filter(Date >= input$min_date), aes(x = Date, y = Value))+
      geom_line()+
      labs(
        title = "Visitor data by Date",
        x = "Date",
        y = "Number of Visitors (in 100,000)"
      )+
      scale_y_continuous(labels = scales::comma_format(scale = 1e-5)) +
      
      theme(
        axis.title = element_text(size = 14),  # Font size for axis titles
        axis.text = element_text(size = 12)    # Font size for tick labels
      )+
      geom_vline(
        data = fire_incidence_filtered_data(),
        aes(xintercept = parsed_date),
        linetype = "dashed", color = "red"
      )
    
  })
  
  
  output$linePlot2 <- renderPlot({
    parks_n_2 <- parks_n|>
             filter(PARKCODE == input$park) |>
             select(-X, -PARKCODE, -TOTAL)|>
             gather(key = "Month", value = "Value", -YEAR, -fullName)|>
             mutate(MonthNumeric = month_mapping[Month])|>
             mutate(Date = make_date(YEAR, MonthNumeric))|>
             arrange(Date)|>
             filter(Date >= input$min_date & Date <= input$incident_dates)
    visitation_data_1 <- ts(
      parks_n_2$Value,
      start = c(year(input$min_date), month(input$min_date)),
      end = c(year(input$incident_dates), month(input$incident_dates)+1),
      frequency = 24)
    seasonal_arima_model <- auto.arima(visitation_data_1)
    forecast_values <- forecast(seasonal_arima_model, 24,level= c(90,95))
    # coef_summary <- summary(seasonal_arima_model)$coefficients
    # p_values <- paste("p-value: ", formatC(coef_summary[, "Pr(>|z|)"], digits = 3, format = "f"))
    # print(forecast_values$index)
    # print(forecast_values$mean)
    forecast_df <- data.frame(
      date = seq(tail(parks_n_2$Date,1)+1, length.out = length(forecast_values$mean), by = "month"),
      values = forecast_values$mean
    )
    forecast_df$lower_90 <- forecast_values$lower[, 1]  # Adjust column index according to your data structure
    forecast_df$upper_90 <- forecast_values$upper[, 1]  # Adjust column index according to your data structure
    forecast_df$lower_95 <- forecast_values$lower[, 2]  # Adjust column index according to your data structure
    forecast_df$upper_95 <- forecast_values$upper[, 2]
    forecast_df$z_score_90 <- (forecast_df$values - mean(visitation_data_1)) / sqrt(seasonal_arima_model$sigma2)
    forecast_df$p_value_90 <- 2 * (1 - pnorm(abs(forecast_df$z_score_90)))
    ggplot() +
      geom_point(data = parks_n|>
                  filter(PARKCODE == input$park) |>
                  select(-X, -PARKCODE, -TOTAL)|>
                  gather(key = "Month", value = "Value", -YEAR, -fullName)|>
                  mutate(MonthNumeric = month_mapping[Month])|>
                  mutate(Date = make_date(YEAR, MonthNumeric))|>
                  arrange(Date)|>
                  filter(Date >= input$min_date & Date <= as.Date(input$incident_dates)+months(24)), aes(x = Date, y = Value), color = "blue") +
      geom_line(data = parks_n|>
                   filter(PARKCODE == input$park) |>
                   select(-X, -PARKCODE, -TOTAL)|>
                   gather(key = "Month", value = "Value", -YEAR, -fullName)|>
                   mutate(MonthNumeric = month_mapping[Month])|>
                   mutate(Date = make_date(YEAR, MonthNumeric))|>
                   arrange(Date)|>
                   filter(Date >= input$min_date & Date <= as.Date(input$incident_dates)+months(24)), aes(x = Date, y = Value), color = "blue")+
      geom_line(data = forecast_df, aes(x = date, y = values), color = "red", linetype = "dashed") +
      geom_ribbon(data = forecast_df,
        aes(
          x=date,
          ymin = lower_90,
          ymax = upper_90
        ),
        fill = "blue", alpha = 0.3
      ) +
      labs(
        title = "Visitor data and forecasting for selected Date Range",
        x = "Date",
        y = "Number of Visitors (in 100,000)",
        subtitle = paste(
          "P-values (90%):", format(forecast_df$p_value_90, digits = 3, scientific = FALSE))
      )+
      scale_y_continuous(labels = scales::comma_format(scale = 1e-5)) +
      
      theme(
        axis.title = element_text(size = 14),  # Font size for axis titles
        axis.text = element_text(size = 12))
  })
  
  # selected_dates <- reactive({
  #   park_fire_incidents_r|>
  #     filter(between(parsed_date, input$date_slider[1], input$date_slider[2]))
  # })
  
  # # Output selected dates
  # output$selected_dates <- renderText({
  #   paste("Selected Dates:", paste(selected_dates()$parsed_date))
  # })
  # 
  
  #this will render the table output
  
  # output$filtered_data <- DT::renderDataTable({
  #   park_fire_incidents_r|>
  #     filter(between(parsed_date, input$date_slider[1], input$date_slider[2]))
  #   # selected_dates()
  # })
  # 
  # 
  output$mapplot <- renderLeaflet({
    leaflet()|>
      addTiles()|>
      addMarkers(data = park_fire_incidents_r,
                 clusterOptions = markerClusterOptions(),
                 popup = ~IncidentName)|>
      addPolygons(data = park_boundary_m, color = "blue", fillOpacity = 0.2, popup =~PARKNAME
      )
  })
  
  observe({
    leafletProxy("mapplot")|>
      clearShapes()|>
      addPolygons(data = park_boundary_m|>
                    filter(UNIT_CODE == input$park), fillColor = "red", fillOpacity = 0.7, popup = ~PARKNAME)
  })
  
  observe({
    leafletProxy("mapplot")|>
      clearMarkers()|>
      clearMarkerClusters()|>
      addMarkers(data = park_fire_incidents_r|>
                   filter(IncidentSize > 100)|>
                   filter(between(parsed_date, 
                                  input$date_slider[1], 
                                  input$date_slider[2])), 
                 clusterOptions = markerClusterOptions(),
                 popup = ~UNIT_NAME)
    
  })
  
  output$forecastPlot <- renderPlot({
    parks_new <- parks_n|>
      filter(PARKCODE == input$park) |>
      select(-TOTAL, -PARKCODE,-fullName)|>
      gather(key = "Month", value = "Value", -YEAR)|>
      mutate(MonthNumeric = month_mapping[Month])|>
      mutate(Date = make_date(YEAR, MonthNumeric))|>
      arrange(Date)
    visitation_data <- ts(
      parks_new$Value, 
      start = c(1979, 1),
      end = c(2023, 12),
      frequency = 12)
    seasonal_arima_model <- auto.arima(visitation_data)
    seasonal_arima_forecast <- forecast(seasonal_arima_model, 48, level= c(50,95))
    autoplot(seasonal_arima_forecast)
    
  })
}