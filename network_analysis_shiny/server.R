

function(input, output, session) {
  
  
  #output$network = renderPlot({
  #    overall.plot(g, clr, g_layout_new)  
  #})
  output$networkZoom = renderVisNetwork({

    withProgress(message = 'Making plot', value = .5, {
      visIgraph(igraph=g, layout = "layout_with_drl", randomSeed = 1
                , idToLabel=FALSE, type='square'
      ) %>% 
        visNodes(value=5) %>%
        
        #toVisNetworkData(idToLabel = FALSE) %>%
        # visNodes(title = actors$title) %>% 
        # visEdges(width=.05) %>%
        visInteraction(hover = TRUE) %>%
        visEvents(hoverNode = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes);
                  ;}") %>%
        visPhysics(stabilization = FALSE) %>%
        
        visGroups(groupname = "Contacts", color="#FF0000",value=3) %>%
        visGroups(groupname = "Alumni", color="#ffc0cb",value=3) %>%
        visGroups(groupname = "Top Clients", color="#008000") %>%
        visGroups(groupname = "Attorneys", color="#0000ff") 
      
    })
  })
  output$shiny_return = renderText({
    paste(input$current_node_id
          , nodes$fullname[which(nodes$id %in% input$current_node_id)] 
          , nodes$Job.Title[which(nodes$id %in% input$current_node_id)], sep='\n')
  })
  observe({
    gr <- input$Attorney
    isolate({
      if(gr %in% nodes$id){
        #nodes <- dataan()$nodes
        id <- nodes$id[nodes$id%in%gr]
        visNetworkProxy("networkZoom") %>%
          visFocus(id = id, animation=list(duration=500, easingFunction='linear'))
      }else{
        id <- NULL
      }
    })
  })
  
}

