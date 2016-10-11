library(igraph)
library(shiny)
library(shinydashboard)
source('attorney.plot.R')
source('SubPlots.R')
library(visNetwork)



dashboardPage(
  dashboardHeader(
    
  ),
  dashboardSidebar(
    sidebarMenu(
      
      selectizeInput("Attorney",label="Which Attorney:"
                     ,choices=uiNames
                     ,selected=NULL
                     ,options = list(maxItems = 1, placeholder = "Select an attorney")
                     ,multiple=FALSE)
      
    )),
  dashboardBody(
    column(12, 
           tags$style(type='text/css', '#shiny_return {background-color: rgba(0,0,255,0.20); color: black;
                      position: absolute;
                      bottom: 0;
                      right: 0;
                      width: 100%;
                      max-width: 320px;
                      padding: 10px;
                      bottom: 0;}'), 
           tags$style(type = "text/css", "#networkZoom {height: calc(100vh - 80px) !important;}"),
           visNetworkOutput("networkZoom"),
           verbatimTextOutput("shiny_return")
    )
  )
  #column(6,
  #plotOutput("networkZoom")
  #     visNetworkOutput("networkZoom")
  #       )
  #)
)