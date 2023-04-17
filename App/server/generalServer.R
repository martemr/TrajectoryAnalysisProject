generalServer <- function(input, output){

  convertDay <- function(day){
    switch(day,
      "monday"   = "lundi",
      "tuesday"   = "mardi",
      "tuesdays"   = "mardi",
      "wednesday"= "mercredi",
      "thursday" = "jeudi",
      "friday"   = "vendredi",
      "saturday" = "samedi",
      "sunday"   = "dimanche",
      "inconnu"
    )
  }
  # Informations
  output$infoArray <- renderDataTable({
    LocId <- input$LocalisationId
    recordingMeta[locationId==LocId,
                  .("ID"=recordingId,
                    "Jour d'enregistrement"=lapply(weekday, convertDay),
                    'Heure de début'=lapply(startTime,function(x) if(is.na(x))"Inconnu" else paste(x,"h00",sep="")),
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
      drawTrajectories(LocId, dosinit, AllTrajectoriesOnOneGraph = TRUE, legend=TRUE)
    }, sizePolicy = sizeGrowthRatio(width= 900, height = 600, growthRate = 1),
    cacheKeyExpr = {list(input$LocalisationId)})
  
}

  

