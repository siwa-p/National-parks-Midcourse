# Wildland Fires and their effect on US National Park visitation

## Motivation

Forest fires (periodic burns) are an important natural process in a variety of forests and can be beneficial to the ecosystem. Warmer and drier climates due to climate change have made wildfires larger and difficult to control causing extended damage to both forests and human settlements.

This shiny app lets us visualize wildfires incidents recorded in national parks in the United States. The user selects the national park from the drop-down menu. Once selected, the park is highlighted in the map.
And the date range can be selected from the slider which will then display the number of such events in the date range.

There are two time series plots below the map. The one on the left shows the visitor data from the data chosed from the dropdown on the left. Fire events are shown with dashed line in the time series plot.

The time-series plot on the right narrows down on one such event (chosen by user from available events) and attempts a seasonal ARIMA forecast on the visitor data immediately after the incident. A p-value of the fit is displayed. 