##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 29th
# Description : Outils de visualisation
##---------------------------------------------

#==========================================
# Librairies
#==========================================
library(arrow)

#==========================================
# Fonction qui ajoute une flèche
#==========================================
addArrow <- function(tId, color='black', weigth=4/30){
  x = tail(unlist(trajectoriesDataset[trackId == tId, "xCenter"]), n=2)
  y = tail(unlist(trajectoriesDataset[trackId == tId, "yCenter"]), n=2)
  arrows( #x[1], x[2], y[1], y[2],
    x0 = tail(x, n = 2)[1],
    x1 = tail(x, n = 2)[2],
    y0 = tail(y, n = 2)[1],
    y1 = tail(y, n = 2)[2],
    length = 0.2,
    lwd = weigth*30,
    col = color
  )
}

#==========================================
# Fonction qui trouve la meilleure annotation et l'ajoute aux courbes
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

#==========================================
# Fonction qui trace un cluster en fonction de ses métadonnées
#==========================================
plotCluster <- function(clusterId, size, representativeId, color, totalFlowSize=1000, drawBackground=FALSE, addAnnotation=FALSE){
  if(drawBackground) drawEmptyPlot(paste("Flux cluster", clusterId))  
  lines(unlist(trajectoriesDataset[trackId == as.numeric(representativeId), "xCenter"]),
        unlist(trajectoriesDataset[trackId == as.numeric(representativeId), "yCenter"]),
        lwd = as.numeric(size)*30/totalFlowSize, col = color)
  addArrow(as.numeric(representativeId), color, as.numeric(size)/totalFlowSize)
  if(addAnnotation) addAnnotations(clusterMeta, indMaxAnnotation, Type="veh/h")
}

#==========================================
# Fonction de tracé des flux
#==========================================
flowDiagram <- function(selectedClass="car", LocationId, clusterMeta, indMaxAnnotation=0, WithAnnotations=TRUE, allFlowOnOneGraph=TRUE){
  locationClusterMeta <- clusterMeta[locationId==LocationId & class==selectedClass,]
  setorder(locationClusterMeta, cols = -"size") # Tri par taille
  if(allFlowOnOneGraph) {
    drawEmptyPlot(LocationId, "Flux de déplacements")
    apply(locationClusterMeta,1, function(x) plotCluster(unlist(x[3]), unlist(x[4]), unlist(x[5]), unlist(x[6]), totalFlowSize = sum(locationClusterMeta$size), drawBackground = FALSE, addAnnotation=FALSE))
    if(WithAnnotations) addAnnotations(locationClusterMeta, indMaxAnnotation, Type="veh/h")
  } else {
    apply(locationClusterMeta,1, function(x) plotCluster(unlist(x[1]), unlist(x[2]), unlist(x[3]), unlist(x[4]), drawBackground = TRUE, addAnnotation=WithAnnotations))
  }
}
