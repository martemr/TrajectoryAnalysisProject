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


#==========================================
# Librairies
#==========================================
library(dplyr)
library(png)

#==========================================
# Functions
#==========================================
labels <- data.frame(matrix(nrow = 0, ncol = 4))
initLabels <- function(){
  labels <<- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(labels) <<- c('lab','x','y','color')
}

minDistance <- function(xLab, yLab, lx, ly) {
  if(length(lx) == 0) {
    return(Inf)
  }
  min(sqrt((lx - xLab)^2 + (ly - yLab)^2))
}



# Store values of annotations
storeAnnotations <- function(tId, cId, color='black', Type){
  # Détermine la valeur à afficher
  if(Type=='percentages'){
    weigth = paste(
      round(
        length(unlist(clusters[clusterId==cId,trackId])) *100/ nrow(clusters), 2),
      "%", sep="")
  } else if (Type=='veh/h'){
    weigth = paste(round(length(unlist(clusters[clusterId == cId, trackId])) / 
                           (unlist(sum(recordingMeta[locationId == LocationId, 'duration'])) / 3600), 0),
    "veh/h", sep = "")
  }
  
  # Détermine la position de l'annotation
  x = unlist(trajectoriesDataset[trackId == tId, "xCenter"])
  y = unlist(trajectoriesDataset[trackId == tId, "yCenter"])
  
  # Cherche une position sans conflits
  pos = 1
  xLab <- unlist(x[pos])
  yLab <- unlist(y[pos])
  while(minDistance(xLab,yLab,labels$x,labels$y) < 30){
    pos = pos+5
    if(pos>length(x)) break
    xLab <- unlist(x[pos])
    yLab <- unlist(y[pos])
  }
  
  ind=nrow(labels)+1
  labels[ind,'lab']   <<-  weigth
  labels[ind,'x']     <<-  xLab
  labels[ind,'y']     <<-  yLab
  labels[ind,'color'] <<-  color
}

addArrow <- function(tId, color='black', weigth){
  x = unlist(trajectoriesDataset[trackId == tId, "xCenter"])
  y = unlist(trajectoriesDataset[trackId == tId, "yCenter"])
  #weigth = length(clusters[clusters$clusterId == id, 'trackId']) / nrow(clusters)
  arrows(
    x0 = tail(x, n = 2)[1],
    x1 = tail(x, n = 2)[2],
    y0 = tail(y, n = 2)[1],
    y1 = tail(y, n = 2)[2],
    length = 0.2,
    lwd = weigth*30,
    col = color
  )
}

addAnnotations <- function(indMaxAnnotation){
  frsz <- 0.5
  #labels <- subset(labels, labels$lab>indMaxAnnotation)
  if(nrow(labels)>0){
    #labels$lab <- paste(labels$lab,"%")
    rect(
      labels$x - strwidth (labels$lab)/2 - frsz,
      labels$y - strheight(labels$lab)/2 - frsz,
      labels$x + strwidth (labels$lab)/2 + frsz,
      labels$y + strheight(labels$lab)/2 + frsz,
      col=labels$color
    )
    text(
      x = labels$x,
      y = labels$y,
      labels = labels$lab,
      col = 'black'
    )
  }
  
}

