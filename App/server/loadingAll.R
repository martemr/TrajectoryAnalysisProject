##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : Chargement de toutes les données et des fonctions
##---------------------------------------------

loadAll <- function(input, output){
  withProgress(message = 'Chargement des données', value = 0, {
    nEtapes <- 5
    
    # Définition des variables étudiés par les widgets
    incProgress(1/5, detail = paste("Chargement des fonctions")) # Barre de progression
    source("~/TrajectoryAnalysisProject/code/01_init.r", echo = FALSE)
    source("~/TrajectoryAnalysisProject/code/02_plotUtils.R", echo = FALSE)
    source("~/TrajectoryAnalysisProject/code/03_visualisationUtils.R", echo = FALSE)
    source("~/TrajectoryAnalysisProject/code/clustering/04_origine-destinationClustering.r", echo = FALSE)
    #source("~/TrajectoryAnalysisProject/code/analysis/07_interractions.R", echo = FALSE)
    
    # Chargement des données
    incProgress(1/nEtapes, detail = paste("Téléchargement des données"))
    start_time <- Sys.time()
    dosinit <<- "../data/"
    loadData(dosinit)
    
    # Nettoyage des données
    incProgress(1/nEtapes, detail = paste("Nettoyage des données"))
    cleanDataset()
    
    # Classification par origine destination
    incProgress(1/nEtapes, detail = paste("Classification des données"))
    for (loc in unique(tracksMeta$locationId)){
      createAllClustersOfLocalisation(loc, tracksMeta)
    }
    
    end_time <- Sys.time()
    paste(nrow(tracks),"points chargés en",round(end_time - start_time, 2),"secondes")
   
    incProgress(1/nEtapes, detail = paste("done"))
  })
  
}