library(shiny)
library(forecast)
library(ggplot2)
library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

shinyServer(function(input, output, session) {
  
  # Reactive expression to filter fire incidents
  fire_incidence_filtered_data <- reactive({
    park_fire_incidents_r %>%
      filter(complete.cases(IncidentSize), IncidentSize > 100, 
             UNIT_CODE == input$park, parsed_date >= input$min_date)
  })
  
  # Update incident date choices based on filtered data
  observe({
    new_filtered_data <- fire_incidence_filtered_data() %>%
      arrange(desc(parsed_date))
    updateSelectInput(session, "incident_dates", choices = new_filtered_data$parsed_date)
  })
  
  # Render main plot
  output$linePlot <- renderPlot({
    park_data <- parks_n %>%
      filter(PARKCODE == input$park) %>%
      select(-X, -PARKCODE, -TOTAL) %>%
      gather(key = "Month", value = "Value", -YEAR, -fullName) %>%
      mutate(MonthNumeric = month_mapping[Month],
             Date = make_date(YEAR, MonthNumeric)) %>%
      filter(Date >= input$min_date) %>%
      arrange(Date)
    
    ggplot(park_data, aes(x = Date, y = Value)) +
      geom_line() +
      geom_vline(data = fire_incidence_filtered_data(), aes(xintercept = parsed_date), linetype = "dashed", color = "red") +
      labs(title = "Visitor Data by Date", x = "Date", y = "Number of Visitors (in 100,000)") +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-5)) +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  })
  
  # Render forecast plot
  output$linePlot2 <- renderPlot({
    park_data <- parks_n %>%
      filter(PARKCODE == input$park) %>%
      select(-X, -PARKCODE, -TOTAL) %>%
      gather(key = "Month", value = "Value", -YEAR, -fullName) %>%
      mutate(MonthNumeric = month_mapping[Month],
             Date = make_date(YEAR, MonthNumeric)) %>%
      filter(Date >= input$min_date & Date <= input$incident_dates) %>%
      arrange(Date)
    
    visitation_data_1 <- ts(park_data$Value,
                            start = c(year(input$min_date), month(input$min_date)),
                            end = c(year(input$incident_dates), month(input$incident_dates) + 1),
                            frequency = 24)
    
    seasonal_arima_model <- auto.arima(visitation_data_1)
    forecast_values <- forecast(seasonal_arima_model, 24, level = c(90, 95))
    
    forecast_df <- data.frame(
      date = seq(tail(park_data$Date, 1) + 1, length.out = length(forecast_values$mean), by = "month"),
      values = forecast_values$mean,
      lower_90 = forecast_values$lower[, 1],
      upper_90 = forecast_values$upper[, 1],
      lower_95 = forecast_values$lower[, 2],
      upper_95 = forecast_values$upper[, 2],
      z_score_90 = (forecast_values$mean - mean(visitation_data_1)) / sqrt(seasonal_arima_model$sigma2),
      p_value_90 = 2 * (1 - pnorm(abs((forecast_values$mean - mean(visitation_data_1)) / sqrt(seasonal_arima_model$sigma2))))
    )
    
    ggplot() +
      geom_point(data = park_data, aes(x = Date, y = Value), color = "blue") +
      geom_line(data = park_data, aes(x = Date, y = Value), color = "blue") +
      geom_line(data = forecast_df, aes(x = date, y = values), color = "red", linetype = "dashed") +
      geom_ribbon(data = forecast_df, aes(x = date, ymin = lower_90, ymax = upper_90), fill = "blue", alpha = 0.3) +
      labs(title = "Visitor Data and Forecasting for Selected Date Range", 
           x = "Date", 
           y = "Number of Visitors (in 100,000)", 
           subtitle = paste("P-values (90%):", format(forecast_df$p_value_90, digits = 3, scientific = FALSE))) +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-5)) +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  })
  
  # Render Leaflet map with reactive data
  output$mapplot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = park_fire_incidents_r, clusterOptions = markerClusterOptions(), popup = ~IncidentName) %>%
      addPolygons(data = park_boundary_m, color = "blue", fillOpacity = 0.2, popup = ~PARKNAME)
  })
  
  observe({
    leafletProxy("mapplot") %>%
      clearShapes() %>%
      addPolygons(data = park_boundary_m %>%
                    filter(UNIT_CODE == input$park), fillColor = "red", fillOpacity = 0.7, popup = ~PARKNAME)
  })
  
  observe({
    leafletProxy("mapplot") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(data = park_fire_incidents_r %>%
                   filter(IncidentSize > 100) %>%
                   filter(between(parsed_date, input$date_slider[1], input$date_slider[2])), 
                 clusterOptions = markerClusterOptions(), popup = ~UNIT_NAME)
  })
  
  # Render forecast plot for all data
  output$forecastPlot <- renderPlot({
    parks_new <- parks_n %>%
      filter(PARKCODE == input$park) %>%
      select(-TOTAL, -PARKCODE, -fullName) %>%
      gather(key = "Month", value = "Value", -YEAR) %>%
      mutate(MonthNumeric = month_mapping[Month],
             Date = make_date(YEAR, MonthNumeric)) %>%
      arrange(Date)
    
    visitation_data <- ts(parks_new$Value, start = c(1979, 1), end = c(2023, 12), frequency = 12)
    seasonal_arima_model <- auto.arima(visitation_data)
    seasonal_arima_forecast <- forecast(seasonal_arima_model, 48, level = c(50, 95))
    
    autoplot(seasonal_arima_forecast)
  })
})
