loadAll <- function(input, output){
  # Définition des variables étudiés par les widgets
  print("Initialisation des fonctions")
  source("~/TrajectoryAnalysisProject/code/00_init.r", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/01_plotUtils.R", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/02_visualisationUtils.R", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/clustering/04_origine-destinationClustering.r", echo=FALSE)
  print(" → Fonctions initialisés")
  
  # Chargement des données
  output$waitingText <- renderText({
    start_time <- Sys.time()
    dosinit <<- "../data/"
    loadData(dosinit)
    cleanDataset()
    end_time <- Sys.time()
    paste(nrow(tracks), "points chargés en", round(end_time-start_time,2), "secondes")
  })
  
  output$null <- renderText({
    LocId <- input$LocalisationId
    createAllClustersOfLocalisation(LocId, tracksMeta)
    ""
  })
  
  initDone <<- TRUE
}