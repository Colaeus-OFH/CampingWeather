## app.R ##

library(shinydashboard)
library(timeDate)
library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)

# Get todays date for plot
setRmetricsOptions(myFinCenter = "Halifax")
today <- as.Date(Sys.timeDate())

ui <- dashboardPage(
    dashboardHeader(title = "Camping Weather"),

    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        dateInput("startDate", "Start Date", value = today),
        menuItem("Map", tabName = "Map", icon = icon("map")),
        menuItem("Temperature", tabName = "Temp", icon = icon("fas fa-temperature-low"))
      )
    ),
  dashboardBody(

    tabItems(
      # First tab content
      tabItem(tabName = "Map",
              fluidRow(
                box(title = "Map",
                  collapsible = TRUE,
                  leafletOutput("NSmap")
                  )
              )
      ),

      # Second tab content
      tabItem(tabName = "Temp",
              h2("Temperature data goes here")
      )
    )
))

server <- function(input, output) {

  # Load the labels pulled from finding the weather stations via google street view
  NS_PW_labels <- read.csv("/home/doug/RNS_data.csv",header = TRUE, stringsAsFactors = FALSE)

  output$NSmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(NS_PW_labels) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng=-63.611, lat=44.5, zoom = 8) %>%
      addMarkers(lng=NS_PW_labels$Long, lat=NS_PW_labels$Lat, popup=NS_PW_labels$Site_Name)
})}

shinyApp(ui, server)
