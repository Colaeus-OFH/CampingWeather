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

# Get todays date for plot
setRmetricsOptions(myFinCenter = "Halifax")
today <- as.Date(Sys.timeDate())

# Load the sqlite EnvCanDB.db data into the con data.frame
con <- dbConnect(RSQLite::SQLite(),"EnvCanDB.db")

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
                box(title = "Precip (if no data is blank)",collapsible = TRUE, plotOutput("NSprecip"))
              )
      )
    )
))

server <- function(input, output) {

  # Load the labels pulled from finding the weather stations via google street view
  con <- dbConnect(RSQLite::SQLite(),"/home/rstudio/CampingWeather/EnvCanDB.db")
  
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
        df$theHour <- hour(df$datetimeutc)
#        df$theZone <- cut(df$theHour,c(0,6,12,18,24))
        #hist(as.numeric(df$air_temperature), breaks = c(-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40), plot = TRUE)
        yearRange <- paste(min(unique(year(df$datetimeutc))), "to", max(unique(year(df$datetimeutc))), sep = " ")
        if (median(as.numeric(df$air_temperature),na.rm = TRUE)<=0) bpcol = "light blue" else bpcol = "light pink"
        boxplot(as.numeric(df$air_temperature), data = df, ylim = c(-20,30),ylab = "Temperature C", xlab = paste("Chosen date:",input$startDate, "over years",yearRange),col=bpcol)
        abline(h=0)
      } else {
        con <- dbConnect(RSQLite::SQLite(),"/home/rstudio/CampingWeather/EnvCanDB.db")
        qryStr <- paste("SELECT Station, DateTime_LST, Year, Month, Day, Temp_C from EC_temps WHERE Station =", curStation," AND Month =", startMonth, "AND Day =", startDay ,sep = " ")    
        NS_Temps <- dbGetQuery(con,  qryStr)
        dbDisconnect(con)
        yearRange <- paste(min(unique(NS_Temps$Year)), "to", max(unique(NS_Temps$Year)), sep = " ")
        if (median(NS_Temps$Temp_C,na.rm = TRUE) <= 0) bpcol = "light blue" else bpcol = "light pink"
        boxplot(as.numeric(NS_Temps$Temp_C), ylim = c(-20,30),ylab = "Temperature C", xlab = paste("Chosen date:",input$startDate, " with data for that day compiled over years",yearRange),col=bpcol)
        abline(h=0)
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
        qryStr <- paste("https://data.novascotia.ca/resource/kafq-j9u4.json?$select=site_id,datetimeutc,avg_wind_speed,avg_wind_direction,max_wind_gust_speed,date_extract_m(datetimeutc) as month,date_extract_d(datetimeutc) as theday&$where=month =", startMonth, " AND theday =", startDay, " AND site_id=", curStation, sep = " ")
        df <- read.socrata(
          qryStr, app_token = "YIJmci7v0Fd0eHtco6IXgFBuP"
        )
        yearRange <- paste(min(unique(year(df$datetimeutc))), "to", max(unique(year(df$datetimeutc))), sep = " ")
        #if (median(as.numeric(df$max_wind_gust_speed),na.rm = TRUE)<=10) bpcol = "light blue" else bpcol = "light pink"
#        bpcol = "light blue"
#        boxplot(as.numeric(df$max_wind_gust_speed), ylim = c(0,90),ylab = "Max Wind Gust", xlab = paste("Chosen date:",input$startDate, "over years",yearRange),col=bpcol)
        if (sum(as.numeric(df$max_wind_gust_speed),na.rm=TRUE) > 0) {
          hist(as.numeric(df$max_wind_gust_speed),breaks = c(0,5,10,15,20,30), plot = TRUE, xlab = "Wind Gust Speed (km/hr)", main = "Wind Speed histogram",sub="red line marks max gust")
          abline(v=max(as.numeric(df$max_wind_gust_speed),na.rm=TRUE),col = "red")
        }
      } else {
        con <- dbConnect(RSQLite::SQLite(),"/home/rstudio/CampingWeather/EnvCanDB.db")
        qryStr <- paste("SELECT Station, DateTime_LST, Year, Month, Day, WindSpd_kmh from EC_temps WHERE Station =", curStation," AND Month =", startMonth, "AND Day =", startDay ,sep = " ")    
        NS_Temps <- dbGetQuery(con,  qryStr)
        dbDisconnect(con)
        hist(as.numeric(NS_Temps$WindSpd_kmh, na.rm=TRUE),breaks = c(0,5,10,15,20,30), plot = TRUE,  main = "Wind Speed histogram",sub="red line marks max gust", xlab = "Average Wind Speed (km/hr)")
        abline(v=max(as.numeric(NS_Temps$WindSpd_kmh),na.rm=TRUE),col = "red")
      }
    }
  )
  
  output$NSprecip <- renderPlot(
    {
    curStation <-paste("'",with(NS_PW_labels,SiteID[Site_Name == input$NSWXPick]),"'",sep="")
    startMonth = month(input$startDate)
    startDay = day(input$startDate)
    
    if (substr(curStation,2,4) == "RNS") {
      # SOCRATA lookup
      qryStr <- paste("https://data.novascotia.ca/resource/kafq-j9u4.json?$select=site_id,datetimeutc,precipitation_1_hour,date_extract_m(datetimeutc) as month,date_extract_d(datetimeutc) as theday&$where=month =", startMonth, " AND theday =", startDay, " AND site_id=", curStation, sep = " ")
      df <- read.socrata(
        qryStr, app_token = "YIJmci7v0Fd0eHtco6IXgFBuP"
      )
      yearRange <- paste(min(unique(year(df$datetimeutc))), "to", max(unique(year(df$datetimeutc))), sep = " ")
      #if (median(as.numeric(df$max_wind_gust_speed),na.rm = TRUE)<=10) bpcol = "light blue" else bpcol = "light pink"
      #        bpcol = "light blue"
      #        boxplot(as.numeric(df$max_wind_gust_speed), ylim = c(0,90),ylab = "Max Wind Gust", xlab = paste("Chosen date:",input$startDate, "over years",yearRange),col=bpcol)
      if (sum(as.numeric(df$precipitation_1_hour),na.rm=TRUE) > 0) { hist(as.numeric(df$precipitation_1_hour,na.rm=TRUE),breaks = "Sturges", plot = TRUE,main = "Rainfall over 1 hour",xlab = "1 hour precipitation")}
    } else {
      con <- dbConnect(RSQLite::SQLite(),"/home/rstudio/CampingWeather/EnvCanDB.db")
      qryStr <- paste("SELECT Station, DateTime_LST, Year, Month, Day, PrecipAmount_mm from EC_temps WHERE Station =", curStation," AND Month =", startMonth, "AND Day =", startDay ,sep = " ")    
      NS_Temps <- dbGetQuery(con,  qryStr)
      dbDisconnect(con)
      if (sum(as.numeric(NS_Temps$PrecipAmount_mm),na.rm=TRUE) > 0) { hist(as.numeric(NS_Temps$PrecipAmount_mm,na.rm=TRUE),breaks = "Sturges", plot = TRUE,main = "Rainfall over 1 day",xlab = "1 day precipitation")}
    }
}
)
  }

shinyApp(ui, server)
