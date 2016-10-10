

function(input, output, session) {


  #output$network = renderPlot({
  #    overall.plot(g, clr, g_layout_new)  
  #})
  output$networkZoom = renderVisNetwork({

    visIgraph(g, layout = "layout_with_drl", randomSeed = 1, idToLabel=FALSE, type='full') %>% 
      visNodes(title = actors$title) %>% 
      visEdges(width=.05)
  })
  observe({
    if (!is.null(input$nameSelection) & length(input$nameSelection)==1)
    {
      visNetworkProxy('networkZoom') %>%
        visSetSelection(nodesId=input$nameSelection)
      #visFocus(id = input$nameSelection
      #         ,scale = 20
      #         ,animation = list(duration = 100, easingFunction = "linear")) %>%
      #visSelectNodes(id = input$nameSelection
      #               , highlightEdges=TRUE
      #               , clickEvent=FALSE) # %>%
      #visSelectEdges(id = input$nameSelection)
    }
  })
}

