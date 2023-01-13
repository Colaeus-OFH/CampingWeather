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
library(vetiver)

# library(openair)

# Get todays date for plot
setRmetricsOptions(myFinCenter = "Halifax")
today <- as.Date(Sys.timeDate())

# Create camping database of EC and lat/long station data to pull from

con <- dbConnect(RSQLite::SQLite(),"~/EnvCanDB.db")
NSWXList <- dbGetQuery(con, "SELECT Site_Name from RNS_data WHERE Data = 'x' ORDER BY Site_Name")

dbDisconnect(con)

ui <- dashboardPage(
    dashboardHeader(title = "Camping Weather"),

    ## Sidebar content
    ## Note the data pull varies depending on if you choose a PW (Public Works)
    ## or EC (Environment Canada) weather station
    dashboardSidebar(
      sidebarMenu(
        dateInput("startDate", "Start Date", value = today),
        selectInput("NSWXPick","Weather Station",NSWXList),
        menuItem("Map", tabName = "Map", icon = icon("map"))
      )
    ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Map",
              fluidRow(
                box(title = "Map",collapsible = TRUE, leafletOutput("NSmap")),
                box(title = "Temp",collapsible = TRUE, plotOutput("NStemp"))
              ),
              fluidRow(
                box(title = "Wind",collapsible = TRUE, plotOutput("NSwind")),
                box(title = "Precip",collapsible = TRUE, textOutput("NSprecip"))
              )
      )
    )
))

server <- function(input, output) {

  # Load the labels pulled from finding the weather stations via google street view
  con <- dbConnect(RSQLite::SQLite(),"~/EnvCanDB.db")
  
  NS_PW_labels <- dbGetQuery(con, "SELECT Site_Name, SiteID, Long, Lat from RNS_data WHERE Data = 'x' ORDER BY Site_Name")
  dbDisconnect(con)
  
  output$NSmap <- renderLeaflet(
    {

    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(NS_PW_labels) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng=with(NS_PW_labels,Long[Site_Name == input$NSWXPick]), lat=with(NS_PW_labels,Lat[Site_Name == input$NSWXPick]), zoom = 9) %>%
      addMarkers(lng=as.numeric(NS_PW_labels$Long), lat=as.numeric(NS_PW_labels$Lat), popup=NS_PW_labels$Site_Name)
  }
  )
  
  output$NStemp <- renderPlot(
    {
      curStation <-paste("'",with(NS_PW_labels,SiteID[Site_Name == input$NSWXPick]),"'",sep="")
      startMonth = month(input$startDate)
      startDay = day(input$startDate)
      
      if (substr(curStation,2,4) == "RNS") {
        # SOCRATA lookup
        qryStr <- paste("https://data.novascotia.ca/resource/kafq-j9u4.json?$select=site_id,datetimeutc,air_temperature,date_extract_m(datetimeutc) as month,date_extract_d(datetimeutc) as theday&$where=month =", startMonth, " AND theday =", startDay, " AND site_id=", curStation, sep = " ")
        df <- read.socrata(
          qryStr, app_token = "YIJmci7v0Fd0eHtco6IXgFBuP"
        )
        hist(as.numeric(df$air_temperature), breaks = c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), plot = TRUE)
      } else {
        con <- dbConnect(RSQLite::SQLite(),"~/EnvCanDB.db")
        qryStr <- paste("SELECT Station, DateTime_LST, Year, Month, Day, Temp_C from EC_temps WHERE Station =", curStation," AND Month =", startMonth, "AND Day =", startDay ,sep = " ")    
        NS_Temps <- dbGetQuery(con,  qryStr)
        dbDisconnect(con)
        hist(as.numeric(NS_Temps$Temp_C), breaks = c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), plot = TRUE)
      }
    }
  )
  
  output$NSwind <- renderPlot(
    {
      curStation <-paste("'",with(NS_PW_labels,SiteID[Site_Name == input$NSWXPick]),"'",sep="")
      startMonth = month(input$startDate)
      startDay = day(input$startDate)
      
      if (substr(curStation,2,4) == "RNS") {
        # SOCRATA lookup
        qryStr <- paste("https://data.novascotia.ca/resource/kafq-j9u4.json?$select=site_id,datetimeutc,air_temperature,date_extract_m(datetimeutc) as month,date_extract_d(datetimeutc) as theday&$where=month =", startMonth, " AND theday =", startDay, " AND site_id=", curStation, sep = " ")
        df <- read.socrata(
          qryStr, app_token = "YIJmci7v0Fd0eHtco6IXgFBuP"
        )
        hist(as.numeric(df$air_temperature), breaks = c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), plot = TRUE)
      } else {
        con <- dbConnect(RSQLite::SQLite(),"~/EnvCanDB.db")
        qryStr <- paste("SELECT Station, DateTime_LST, Year, Month, Day, Temp_C from EC_temps WHERE Station =", curStation," AND Month =", startMonth, "AND Day =", startDay ,sep = " ")    
        NS_Temps <- dbGetQuery(con,  qryStr)
        dbDisconnect(con)
        hist(as.numeric(NS_Temps$Temp_C), breaks = c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), plot = TRUE)
      }
    }
  )
  
  output$NSprecip <- renderText(
  length(NS_PW_labels)
)
  }

shinyApp(ui, server)
