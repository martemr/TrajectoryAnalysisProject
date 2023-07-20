##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : Server de la page "Etude détaillée"
##---------------------------------------------

detailledStudyServer <- function(input, output){
  #==========================================
  # Type d'étude : CLASSE
  #==========================================
  # Trajectoires
  output$classTrajectoire <- renderCachedPlot({
      print("Chargement trajectoire")
      LocId <- input$LocalisationId
      class <- input$classId
      drawTrajectories(LocId, AllTrajectoriesOnOneGraph = TRUE, StudiedClass = class)
      print(" → done")
  }, cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  # Groupes de trajectoires
  output$clusterTrajectoire <- renderCachedPlot({
    print("Chargement clusters")
    LocId <- input$LocalisationId
    class <-input$classId
    drawClusters(LocId, clusters, clusterMeta,selectedClass = class, AllTrajectoriesOnOneGraph = TRUE)
    print(" → done")
  }, cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  # Flux
  output$flowDiagram <- renderCachedPlot({
    print("Chargement diagramme de flux")
    LocId <- input$LocalisationId
    class <-input$classId
    flowDiagram(selectedClass=class,LocationId=LocId,clusterMeta, WithAnnotations=FALSE, allFlowOnOneGraph=TRUE)
    print(" → done")
  }, cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  # Circulations sur la chausée
  output$onRoadPlot <- renderCachedPlot({
    print("Chargement ")
    LocId <- input$LocalisationId
    class <-input$classId
    drawOnRoad(trajectoriesDataset, studiedClass=class, LocId)
    print(" → done")
  }, cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  #  Vitesse
  #  Zone d'attente
  #  Inclure annotations
  
  #==========================================
  # Type d'étude : FREQUENTATION
  #==========================================
  #  Tableau des comptages
  output$countArray <- renderDataTable({
    LocId <- input$LocalisationId
    totalTime = unlist(sum(recordingMeta[locationId==LocId,'duration']))/60
    cl <- input$countClass
    #print(cl)
    data.table("Type"=c(cl, "Total"),
               "Comptage" = c(lapply(cl, function(x) paste(round((n_distinct(trajectoriesDataset[locationId==LocId & class == x, 'trackId'])/ totalTime), 0), 'usagers/heure')),
                                     paste(round((n_distinct(trajectoriesDataset[locationId==LocId,trackId])/totalTime),0),'usagers/heure')))
  })
  
  #  Diagramme en camembert des comptes
  output$countChart <- renderPlot({
    LocId <- input$LocalisationId
    drawPieChart(LocId)
  }, width = 900, height=600)
  
  #==========================================
  # Type d'étude : INTERRACTIONS
  #==========================================
  # Tracé d'interactions
  output$interractionPlot <- renderPlot({
    tId <- as.numeric(input$trackIdInterraction)
    rId <- as.numeric(input$recordingInput)
    drawAllInteractionsTrack(tId)
  }, width = 900, height=600)
  
}
  