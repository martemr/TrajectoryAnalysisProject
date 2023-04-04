##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 23th
# Description : Diagramme de flux sur la carte
##---------------------------------------------

#==========================================
# Librairies
#==========================================
library(dplyr)
library(arrow)

#==========================================
# Fonction qui trace un cluster en fonction de ses métadonnées
#==========================================
plotCluster <- function(clusterId, size, representativeId, color, drawBackground=FALSE, addAnnotation=FALSE){
  if(drawBackground) drawEmptyPlot(paste("Flux cluster", clusterId))  
  lines(unlist(trajectoriesDataset[trackId == as.numeric(representativeId), "xCenter"]),
        unlist(trajectoriesDataset[trackId == as.numeric(representativeId), "yCenter"]),
        lwd = as.numeric(size)*30/nrow(clusters), col = color)
  addArrow(as.numeric(representativeId), color, as.numeric(size)/nrow(clusters))
  if(addAnnotation) addAnnotations(clusterMeta, indMaxAnnotation, Type="veh/h")
}

#==========================================
# Tracé des courbes : Tous sur un graphe
#==========================================
flowDiagram <- function(clusterMeta, indMaxAnnotation=0, WithAnnotations=TRUE, allFlowOnOneGraph=TRUE){
  setorder(clusterMeta, cols = -"size") # Tri par taille
  if(allFlowOnOneGraph) {
    drawEmptyPlot("Flux de déplacements")
    apply(clusterMeta,1, function(x) plotCluster(unlist(x[1]), unlist(x[2]), unlist(x[3]), unlist(x[4]), drawBackground = FALSE, addAnnotation=FALSE))
    if(WithAnnotations) addAnnotations(clusterMeta, indMaxAnnotation, Type="veh/h")
  } else {
    apply(clusterMeta,1, function(x) plotCluster(unlist(x[1]), unlist(x[2]), unlist(x[3]), unlist(x[4]), drawBackground = TRUE, addAnnotation=WithAnnotations))
  }
}