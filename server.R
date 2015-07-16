
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(jsonlite)
library(leaflet)
library(dygraphs)

load('seconds_status.RData')
seconds_status <- seconds_status %>%
  within(expr={
    timestamp<-timestamp %>%
      as.character %>%
      as.numeric %>%
      "/"(b=1000) %>%
      as.POSIXct(origin="1970-01-01")
  })

station<-readLines('BikeStation.json') %>%
  fromJSON(simplifyVector = FALSE) %>%
  lapply(as.data.frame) %>%
  do.call(what=rbind)

colnames(station)[4:5]<-c('lng', 'lat')

shinyServer(function(input, output) {
  
  timestamps<-unique(seconds_status$timestamp)
  
  cur_time_tbl<-reactive({
    cur_time<-subset(seconds_status, timestamp == timestamps[input$time_slider_idx])
    merge(cur_time, station, by='NO')
  })
  
  output$map<-renderLeaflet({
    leaflet() %>%
      setView(lng=108.38, lat=22.8, zoom=12) %>%
      addTiles()
  })
  
  observe({
    leafletProxy('map', data=cur_time_tbl()) %>%
      clearShapes() %>%
      addCircles(
        radius= 90,
        fillOpacity = ~R)
  })
  
  output$cur_time<-renderText({
    as.character(timestamps[input$time_slider_idx])
  })
  
  output$date_slider<-renderUI({
    sliderInput('time_slider_idx', NULL,
                min=1,
                max=length(timestamps),
                value=1,
                step=1,
                animate=animationOptions(interval = 1))
  })
  
  output$broadband_tbl<-DT::renderDataTable({
    dt<-cur_time_tbl()
    dt$X<-NULL
    dt$Y<-NULL
    dt
  })
  
})
