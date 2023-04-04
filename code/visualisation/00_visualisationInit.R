##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Création de fonctions pour otimiser le tracé
##---------------------------------------------

#==========================================
# Librairies
#==========================================
library(dplyr)
library(png)

#==========================================
# Find best positions to add annotations and add them
#==========================================
addAnnotations <- function(clusterMeta, indMaxAnnotation, Type="veh/h"){
  LocationId=clusterMeta
  # Détermine la valeur à afficher
  if(Type=="percentages"){
    clusterMeta[,'annotation'] <- paste(round(clusterMeta$size*100/nrow(clusters),2),"%", sep="")
  } else if (Type=='veh/h'){
  #  clusterMeta[,'annotation'] <- paste(round(clusterMeta$size/(unlist(sum(recordingMeta[locationId == LocationId,'duration']))/60), 0),"veh/h", sep = "")
  }
  
  
  # Cherche une position sans conflits avec d'autres annotations
  xyAnnotations <- matrix(ncol=2, nrow=nrow(clusterMeta))
  # On place la première position
  xyAnnotations[1,] <- unlist(trajectoriesDataset[trackId==clusterMeta[1,representativeId],.(xCenter,yCenter)][1])
  # Pour chaque point suivant
  if(nrow(clusterMeta)>1){
    for (i in seq(2, nrow(clusterMeta))){
      # Calcul de la distance de toutes les positions potentielles aux points existants
      potentialPositions <-  apply(trajectoriesDataset[trackId==clusterMeta[i,representativeId],.(xCenter, yCenter)], 1,function(x) c(x))
      distances <- matrix(ncol=ncol(potentialPositions), nrow=nrow(xyAnnotations))
      for (ind in seq(1,nrow(xyAnnotations))){
        if(ind==i) next
        if(is.na(xyAnnotations[ind])) break
        distances[ind,] <- apply(potentialPositions, 2, function(x) euclidean(xyAnnotations[ind,], x))
      }
      xyAnnotations[i,] <- potentialPositions[,which(max(colMeans(distances, na.rm=T))==colMeans(distances, na.rm=T))]
    }
  }
  
  # Ajoute les annotations au graphique
  frsz <- 0.5
  rect(
    xyAnnotations[,1] - strwidth (clusterMeta$annotation) / 2 - frsz,
    xyAnnotations[,2] - strheight(clusterMeta$annotation) / 2 - frsz,
    xyAnnotations[,1] + strwidth (clusterMeta$annotation) / 2 + frsz,
    xyAnnotations[,2] + strheight(clusterMeta$annotation) / 2 + frsz,
    col = clusterMeta$color
  )
  text(
    x = xyAnnotations[,1],
    y = xyAnnotations[,2],
    labels = clusterMeta$'annotation(veh/h)',
    col = 'black'
  )
}

