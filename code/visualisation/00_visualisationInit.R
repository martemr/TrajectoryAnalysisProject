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
#annotations <<- data.frame(matrix(nrow = 0, ncol = 4))

resetAnnotations <- function(){
  annotations <<- data.table(matrix(nrow = 0, ncol = 4))
}


# Store values of annotations
storeAnnotations <- function(tId, cId, color='black', Type="veh/h"){
  # Détermine la valeur à afficher
  if(Type=="percentages"){
    weigth = paste(
      round(
        length(unlist(clusters[clusterId==cId,trackId])) *100/ nrow(clusters), 2),
      "%", sep="")
  } else if (Type=='veh/h'){
    weigth = paste(round(length(unlist(clusters[clusterId == cId, trackId])) / 
                           (unlist(sum(recordingMeta[locationId == LocationId, 'duration'])) / 60), 0),
    "veh/h", sep = "")
  }
  
  # Détermine la position de l'annotation
  x = unlist(trajectoriesDataset[trackId == tId, "xCenter"])
  y = unlist(trajectoriesDataset[trackId == tId, "yCenter"])
  
  # Cherche une position sans conflits avec d'autres annotations
  pos = seq(6,length(x),5)
  if(nrow(annotations)==0){
    annotations <<- data.table(lab=weigth,x=x[1],y=y[1],color=color)
  } else{
    minList <- min(sqrt((annotations$x - x[1])^2 + (annotations$y - y[1])^2))
    for(p in pos){
      minList <- c(minList, 
                   min(sqrt((annotations$x - x[p])^2 + (annotations$y - y[p])^2)))
    }
    posOptimale <- (which(minList == max(minList))[1]*5)-4
    annotations <<- rbind(annotations,
                          list(weigth,x[posOptimale], y[posOptimale], color))
    
  }
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
  colnames(annotations) <<- c('lab','x','y','color') # Forcer le nom du dataframe
  annotations <- subset(annotations, annotations$lab>indMaxAnnotation)
  if(nrow(annotations)>0){
    #annotations$lab <- paste(annotations$lab,"%")
    rect(
      annotations$x - strwidth (annotations$lab)/2 - frsz,
      annotations$y - strheight(annotations$lab)/2 - frsz,
      annotations$x + strwidth (annotations$lab)/2 + frsz,
      annotations$y + strheight(annotations$lab)/2 + frsz,
      col=annotations$color
    )
    text(
      x = annotations$x,
      y = annotations$y,
      labels = annotations$lab,
      col = 'black'
    )
  }
  
}

