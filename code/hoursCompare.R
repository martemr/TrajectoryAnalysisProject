##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Initialisation des données
##---------------------------------------------

#==========================================
# Parametres
#==========================================

for (i in seq(0,6)){
  commandArgs <- function(...) i
  source("~/Courbes_trajectoires/code/00_init.r", echo=FALSE)
  source("~/Courbes_trajectoires/code/clustering/04_origine-destinationClustering.r", echo=FALSE)
  source("~/Courbes_trajectoires/code/visualisation/00_visualisationInit.r", echo=FALSE)
  source("~/Courbes_trajectoires/code/visualisation/04_movesDiagram.R", echo=FALSE)
}

