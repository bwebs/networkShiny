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
      selectizeInput("nameSelection",label="Which Professional:"
                  ,choices=uiNames
                  ,multiple=TRUE)
    
  )),
  dashboardBody(
    column(12, 
      
      tags$style(type = "text/css", "#networkZoom {height: calc(100vh - 80px) !important;}"),
      visNetworkOutput("networkZoom")
      #plotOutput("network",
      #brush = brushOpts(
        #id = "plot_brush",
        #resetOnNew = TRUE)
    
      )
    )
    #column(6,
          #plotOutput("networkZoom")
    #     visNetworkOutput("networkZoom")
    #       )
  #)
)