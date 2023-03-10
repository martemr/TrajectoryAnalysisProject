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

#==========================================
# Librairies
#==========================================
library(dplyr)
library(arrow)

#==========================================
# Functions
#==========================================
chooseRepresentativeId <- function(idList, method){
  if (method=='max'){
    tId = unlist(idList[idList$distanceTraveled == max(idList$distanceTraveled) & (idList$class == "car"), 'trackId'])
  } else if (method=='random'){
    tId=sample(idList$trackId,1)
  } else if (method=='mean_interpolation'){
    stop("Method not implemented yet")
  } else if (method=='mean_distance'){
    tId=idList[which.min(abs(idList$distanceTraveled-mean(idList$distanceTraveled))), trackId]
  }
}


#==========================================
# Tracé des courbes
#==========================================
drawEmptyPlot("Diagramme des déplacements")
colors = rainbow(n = n_distinct(clusters$clusterId))
initLabels()
for (id in unique(clusters$clusterId)) {
  weigth = length(unlist(clusters[clusterId == id, 'trackId'])) / nrow(clusters)
  color = colors[id]
  idList = tracksMeta[trackId %in% unlist(clusters[clusterId == id, 'trackId']), .(trackId,distanceTraveled,class)]
  
  tId = chooseRepresentativeId(idList, "mean_distance")
  x = unlist(trajectoriesDataset[trackId == tId, "xCenter"])[1:5]
  y = unlist(trajectoriesDataset[trackId == tId, "yCenter"])[1:5]
  
  lines(x, y, lwd = weigth*30, col = color)
  addArrow(tId, color, weigth)
  
  # Store values of annotations
  storeAnnotations(tId, color)
}


#==========================================
# Ajout des annotations
#==========================================
addAnnotations(indMaxAnnotation)

