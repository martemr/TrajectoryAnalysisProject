##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 23th
# Description : 
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init
# Run a clustering method

#==========================================
# Parametres
#==========================================
if(!exists("indMaxAnnotation")) indMaxAnnotation = 0.50
if(!exists("clusters")) stop("Faire tourner une méthode de clustering d'abord")
if(!exists("detailledMovesDiagram")) detailledMovesDiagram <- TRUE
if(!exists("WithAnnotations")) WithAnnotations <- TRUE


#==========================================
# Librairies
#==========================================
library(dplyr)
library(arrow)

#==========================================
# Fonction qui trace un cluster en fonction de ses métadonnées
#==========================================
plotCluster <- function(clusterId, size, representativeId, color){
  lines(unlist(trajectoriesDataset[trackId == as.numeric(representativeId), "xCenter"]),
        unlist(trajectoriesDataset[trackId == as.numeric(representativeId), "yCenter"]),
        lwd = as.numeric(size)*30/nrow(clusters), col = color)
  addArrow(as.numeric(representativeId), color, as.numeric(size)/nrow(clusters))
}

#==========================================
# Tracé des courbes : Tous sur un graphe
#==========================================
drawEmptyPlot("Diagramme des déplacements")
# Tracé des clusters
apply(clusterMeta,1, function(x) plotCluster(unlist(x[1]), unlist(x[2]), unlist(x[3]), unlist(x[4])))
if(WithAnnotations){
  addAnnotations(clusterMeta, indMaxAnnotation, Type="veh/h")
}


#==========================================
# Tracé des courbes : Un graphe par cluster
#==========================================
if(detailledMovesDiagram){
  # Tracé des clusters
  apply(clusterMeta, 1, function(x) {
    drawEmptyPlot(paste("Trajectoire", x[1]))
    plotCluster(unlist(x[1]), unlist(x[2]), unlist(x[3]), unlist(x[4]))
    addAnnotations(clusterMeta = (
      data.table(
        clusterId = as.numeric(x[1]),
        size = as.numeric(x[2]),
        representativeId = as.numeric(x[3]),
        color = x[4]
      )
    ),
    indMaxAnnotation,
    Type = "veh/h")
  })
}
