##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 22th
# Description : Etude des changements brutaux de direction dans le recording 00, problème de detection
# ___________DEPRECATED___________
##---------------------------------------------

#==========================================
# Parametres
#==========================================
range = c(166,205,322)

#==========================================
# Librairies
#==========================================
library(dbscan)
library(factoextra)
library(png)
library(tidyverse)

#==========================================
# Tracé des changements de directions (virages)
#==========================================
plot(NULL, xlim=c(-180,180), ylim=c(0,50),xlab="Heading", ylab="Time", main="Variations of heading")
for (ind in range){
  id=ind
  h0 = unlist(tracks[tracks$trackId==id,"heading"])[1]
  points(unlist(tracks[tracks$trackId==id,"heading"])-h0,
         unlist((tracks[tracks$trackId==id,"trackLifetime"]-1)/5), col='black', pch=20)
  points(unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"heading"])-h0,
         unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"group"]), col='red',pch=20)
}

#==========================================
# Tracé 1 graphe par cluster
#==========================================
# Création des sous groupes
for (clusterId in unique(clusters$clusterId)){
  idList = clusters[clusters$clusterId==clusterId,'trackId']
  plot (NULL,
        xlim=c(0,200),ylim=c(-120,0),
        axes=T,ylab="Y",xlab="X",main=paste("Cluster", clusterId))
  for (id in idList){
      print(id)
      lines(unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"xCenter"]),
            unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"yCenter"]), 
            col=colors[clusters[clusters$trackId==id,"clusterId"]])
  }
}

