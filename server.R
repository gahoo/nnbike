
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
library(reshape2)
library(ggplot2)

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

colnames(station)[4:5]<-c('longitude', 'latitude')

shinyServer(function(input, output) {
  
  timestamps<-unique(seconds_status$timestamp)
  
  bound_station<-reactive({
    subset(station,
           latitude < input$map_bounds$north &
             latitude > input$map_bounds$south &
             longitude < input$map_bounds$east &
             longitude > input$map_bounds$west)
  })
  
  cur_time_tbl<-reactive({
    cur_time<-subset(seconds_status, timestamp == timestamps[input$time_slider_idx])
    merge(cur_time, bound_station(), by='NO')
  })
  
  station_tbl<-reactive({
    subset(seconds_status, NO == input$map_shape_click$id)
    
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
        radius= ~sqrt(R * 100) * 10,
        popup = ~ NO,
        layerId = ~ NO,
        stroke = F,
        fillOpacity = 0.3)
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
  
  status_tbl<-reactive({
    if(input$tbl_bounds){
      dt<-cur_time_tbl()
    }else{
      dt<-station_tbl()
    }
    
    dt$Addr<-NULL
    dt$Name<-NULL
    dt
  })
  
  status_date_range<-reactive({
    if(input$tbl_bounds){
      dt<-subset(seconds_status, NO %in% bound_station()$NO)
    }else{
      dt<-station_tbl()
    }
    
    date_range<-as.POSIXct(input$date_range,origin="1970-01-01") + c(-8, 16) * 3600
    status_date<-subset(dt, timestamp >= min(date_range) &
             timestamp < max(date_range))
    status_date
  })
  
  output$station_tbl<-DT::renderDataTable({
    status_tbl()
  })
  
  output$date_range_picker<-renderUI({
    dateRangeInput('date_range', NULL,
                   start = min(timestamps),
                   end = max(timestamps))
  })
  
  output$plot<-renderPlot({
    columns<-c('NO', 'timestamp', 'E', 'F', 'T')
    status_molen<-melt(status_date_range()[columns], id=c('NO', 'timestamp'))
    p<-ggplot(status_molen) + aes(x=timestamp, y=value)
    
    if(input$tbl_bounds){
      p +
        aes(color=NO, group=NO) +
        geom_line() +
        facet_grid(variable ~ ., scale='free_y')
    }else{
      p +
        aes(fill=variable) +
        geom_area()
    }
    
  })
  
  output$helper<-renderText({
    date_range<-as.POSIXct(input$date_range, origin="1970-01-01") + c(-8, 16) * 3600
    str(date_range)
    as.character(date_range)
  })
  
})
