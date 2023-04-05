generalServer <- function(input, output){

  # Informations
  output$infoArray <- renderDataTable({
    LocId <- input$LocalisationId
    recordingMeta[locationId==LocId,
                  .("ID"=recordingId,
                    "Jour d'enregistrement"=weekday,
                    'Heure de début'=paste(startTime,"h00",sep=""),
                    'Durée (min)'=paste(round(duration,0),"min"),
                    'Usagers observés'=numVehicles)]
  })
  
  # Zone d'étude
  output$zoneEtude <- renderPlot({
    LocId <- input$LocalisationId
    initPlotImage(LocId, dosinit)
    drawEmptyPlot("Zone d'étude")
  }, width = 900, height = 600)
  
  # Trajectoires
  output$trajectoires <-
    renderCachedPlot({
      LocId <- input$LocalisationId
      #par(mfrow = c(2, 2))
      drawTrajectories(LocId, dosinit, AllTrajectoriesOnOneGraph = TRUE)
    }, sizePolicy = sizeGrowthRatio(width= 900, height = 600, growthRate = 1),
    cacheKeyExpr = {list(input$LocalisationId)})
  
}

  

