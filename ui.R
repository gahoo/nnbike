
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
                           strong(textOutput('cur_time')),
                           uiOutput('date_slider'),
                           checkboxInput('tbl_bounds', 'Bound Map to Table', value=T),
                           checkboxInput('show_data', 'Show Data', value=F)
                  ),
                  tabPanel('About'#,
                          # wellPanel(includeHTML('about.html'))
                  )
                )
                #textOutput('helper')
  ),
  conditionalPanel(condition = "input.show_data == true",
                   absolutePanel(bottom = 10, left = 10,
                                 tabsetPanel(
                                   tabPanel('Table',
                                            DT::dataTableOutput('broadband_tbl')),
                                   tabPanel('Barplot',
                                            plotOutput('cities_bar_plot', height="640px", width='640px')),
                                   tabPanel('Dotplot',
                                            plotOutput('cities_dot_plot', height="640px", width='480px')),
                                   tabPanel('ggvisDotplot',
                                            style = "background-color: #FFFFFF",
                                            uiOutput("dot_plot_ui"),
                                            ggvisOutput("ggvis_cities_dot_plot")
                                   )
                                   #dygraphs
                                 )
                   )
  )
))
