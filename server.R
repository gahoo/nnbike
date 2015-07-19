
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
library(showtext)

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

showtext.auto()

rgbColors<-function(option){
  total<-option$all
  option$all<-NULL
  option<-lapply(option, function(x){
    num<-x/total
    inf_idx<-is.infinite(num)
    nan_idx<-is.nan(num)
    num[inf_idx|nan_idx]<-1
    num
    })
  color<-try(do.call('rgb', option))
  color
}

removeNoChangeTimestamp<-function(df){
  record_cnts<-nrow(df)
  records<-as.matrix(df[c('E', 'T', 'F')])
  records_change<-records[2:record_cnts,]-records[1:(record_cnts-1),]
  no_change_idx<-!apply(records_change,1,function(x)(sum(x!=0)))==0
  no_change_region_idx<-no_change_idx[1:(record_cnts-2)]|no_change_idx[2:(record_cnts-1)]
  df_no_change_idx<-c(T,no_change_region_idx,T)#no_change_idx[record_cnts-1]
  df[df_no_change_idx,]
}

shinyServer(function(input, output) {
  
  timestamps<-unique(seconds_status$timestamp)
  
  bound_station<-reactive({
    subset(station,
           latitude < input$map_bounds$north &
             latitude > input$map_bounds$south &
             longitude < input$map_bounds$east &
             longitude > input$map_bounds$west)
  })
  
  certain_time_tbl<-reactive({
    subset(seconds_status, timestamp == timestamps[input$time_slider_idx])
  })
  
  range_time_tbl<-reactive({
    date_range<-as.POSIXct(input$date_range,origin="1970-01-01") + c(-8, 16) * 3600
    subset(seconds_status, timestamp >= min(date_range) &
             timestamp < max(date_range))
  })
  
  status_tbl<-reactive({
    if(input$use_range){
      status_tbl<-range_time_tbl()
    }else{
      status_tbl<-certain_time_tbl()
    }
    
    if(input$bound_map){
      status_tbl<-subset(status_tbl, NO %in% bound_station()$NO)
    }else{
      status_tbl<-subset(status_tbl, NO == input$map_shape_click$id)
    }
    
    merge(status_tbl, station, by='NO')
  })
  
  output$map<-renderLeaflet({
    leaflet() %>%
      setView(lng=108.38, lat=22.8, zoom=12) %>%
      addTiles()
  })
  
  observe({
    circle_tbl<-merge(certain_time_tbl(), station, by='NO')
    leafletProxy('map', data=circle_tbl) %>%
      clearShapes() %>%
      addCircles(
        #radius= ~sqrt(R * 100) * 10,
        radius= ~sqrt(A) * 10,
        popup = ~ Name,
        layerId = ~ NO,
        fillColor = ~rgbColors(list(red=E, green=F, blue=T, all=A)),
        stroke = F,
        #fillOpacity = ~rev(R)
        fillOpacity = 0.8
        )
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
                animate=animationOptions(interval = 500))
  })
  
  output$date_range_picker<-renderUI({
    dateRangeInput('date_range', NULL,
                   start = min(timestamps),
                   end = max(timestamps))
  })

  output$status_tbl<-DT::renderDataTable({
    status_tbl()
  })
  
  output$plot<-renderPlot({
    columns<-c('NO', 'Name', 'timestamp', 'E', 'F', 'T', 'R')
    status_molen<-melt(status_tbl()[columns], id=c('NO', 'Name', 'timestamp', 'R'))
    p<-ggplot(status_molen) + aes(x=timestamp, y=value)
    
    if(input$bound_map & input$use_range){
      p +
        aes(color=Name, group=Name) +
        geom_line() +
        facet_grid(variable ~ ., scale='free_y')
    }else if(!input$bound_map & input$use_range){
      p +
        aes(fill=variable) +
        geom_area()
    }else{
      p +
        aes(x=Name, fill=variable, alpha=R) +
        geom_bar(stat='identity') +
        coord_flip()
    }
    
  })
  
  output$dygraph<-renderDygraph({
    if(input$bound_map | !input$use_range){
      return(NULL)
    }
    columns<-c('timestamp', 'E', 'F', 'T')
    dy_tbl<-status_tbl()[columns]
    
    rownames(dy_tbl)<-dy_tbl$timestamp
    dy_tbl$timestamp<-NULL
    
    dygraph(dy_tbl) %>%
      dySeries("E", label = "Error") %>%
      dySeries("T", label = "Occupied") %>%
      dySeries("F", label = "Empty") %>%
      #dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height = 20)
    
  })
  
  output$helper<-renderText({
    date_range<-as.POSIXct(input$date_range, origin="1970-01-01") + c(-8, 16) * 3600
    str(date_range)
    as.character(date_range)
  })
  
})
