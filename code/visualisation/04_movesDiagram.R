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
if(!exists("indMaxAnnotation")) indMaxAnnotation = 10

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
for (cId in unique(clusters$clusterId)) {
  weigth = length(unlist(clusters[clusterId == cId, 'trackId'])) / nrow(clusters)
  color = colors[cId]
  idList = tracksMeta[trackId %in% unlist(clusters[clusterId == cId, 'trackId']), .(trackId,distanceTraveled,class)]
  
  tId = chooseRepresentativeId(idList, "mean_distance")
  x = unlist(trajectoriesDataset[trackId == tId, "xCenter"])
  y = unlist(trajectoriesDataset[trackId == tId, "yCenter"])
  
  lines(x, y, lwd = weigth*30, col = color)
  addArrow(tId, color, weigth)
  
  # Store values of annotations
  storeAnnotations(tId, cId, color, Type="veh/h")
}


#==========================================
# Ajout des annotations
#==========================================
addAnnotations(indMaxAnnotation)

