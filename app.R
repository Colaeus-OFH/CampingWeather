## app.R ##

library(shinydashboard)
library(timeDate)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(leaflet)
library(DBI)
library(RSQLite)
library(RSocrata)
# library(openair)

# Get todays date for plot
setRmetricsOptions(myFinCenter = "Halifax")
today <- as.Date(Sys.timeDate())

con <- dbConnect(RSQLite::SQLite(),"~/CampingWeather.db")

NSWXList <- dbGetQuery(con, "SELECT Site_Name from RNS_data ORDER BY Site_Name")
dbDisconnect(con)

ui <- dashboardPage(
    dashboardHeader(title = "Camping Weather"),

    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        dateInput("startDate", "Start Date", value = today),
        selectInput("NSWXPick","Weather Station",NSWXList),
        menuItem("Map", tabName = "Map", icon = icon("map")),
        menuItem("Synopsis", tabName = "Synop", icon = icon("far fa-file-alt"))
      )
    ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Map",
              fluidRow(
                box(title = "Map",collapsible = TRUE, leafletOutput("NSmap")),
                box(title = "Temp",collapsible = TRUE, plotOutput("NStemp"))
      )),

# Second tab content
      tabItem(tabName = "Synop",
              fluidRow(
                box(title = "Synopsis",
                    collapsible = TRUE,
#                    plotOutput("NStemp")
                )
              )
      )
    )
))

server <- function(input, output) {

  # Load the labels pulled from finding the weather stations via google street view
  con <- dbConnect(RSQLite::SQLite(),"~/CampingWeather.db")
  
  NS_PW_labels <- dbGetQuery(con, "SELECT Site_Name, Long, Lat from RNS_data WHERE Data = 'x' ORDER BY Site_Name")
  dbDisconnect(con)
  
  output$NSmap <- renderLeaflet(
    {
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(NS_PW_labels) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng=-63.611, lat=44.5, zoom = 8) %>%
      addMarkers(lng=as.numeric(NS_PW_labels$Long), lat=as.numeric(NS_PW_labels$Lat), popup=NS_PW_labels$Site_Name)
  }
  )
  
  output$NStemp <- renderPlot(
    {
    con <- dbConnect(RSQLite::SQLite(),"~/CampingWeather.db")
    startMonth = month(input$startDate)
    startDay = day(input$startDate)
    qryStr <- paste("SELECT StationName, DateTime_LST, Year, Month, Day, MeanTemp_C from EC_dly_recs WHERE Month =", startMonth, "AND Day =", startDay ,sep = " ")    
    NS_Temps <- dbGetQuery(con,  qryStr)
    dbDisconnect(con)
    hist(NS_Temps$MeanTemp_C)
  }
  )
  }

shinyApp(ui, server)
