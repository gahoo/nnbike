
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(ggvis)
library(leaflet)
library(dygraphs)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, width='30%',
                tabsetPanel(
                  tabPanel('Ctrl',
                           checkboxInput('use_range', 'Ranges:', value=F),
                           conditionalPanel(
                             condition = "input.use_range == true",
                             uiOutput('date_range_picker')
                           ),
                           conditionalPanel(
                             condition = "input.use_range == false",
                             strong(textOutput('cur_time')),
                             uiOutput('date_slider')
                           ),
                           checkboxInput('bound_map', 'Bound Map', value=T),
                           checkboxInput('show_data', 'Show Data', value=F)
                  ),
                  tabPanel('About'#,
                          # wellPanel(includeHTML('about.html'))
                  )
                ),
                textOutput('helper')
  ),
  conditionalPanel(condition = "input.show_data == true",
                   absolutePanel(bottom = 10, left = 10,
                                 tabsetPanel(
                                   tabPanel('Table',
                                            DT::dataTableOutput('status_tbl')),
                                   tabPanel('ggplot',
                                            plotOutput('plot')),
                                   tabPanel('dygraph',
                                            style = "background-color: #FFFFFF",
                                            conditionalPanel(
                                              condition = "input.use_range == false | input.bound_map == true",
                                              strong('only works when use range not bound map')
                                            ),
                                            dygraphOutput('dygraph'))
                                 )
                   )
  )
))
