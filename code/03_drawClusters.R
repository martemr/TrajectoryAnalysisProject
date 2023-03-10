##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 23th
# Description : Draw clusters
##---------------------------------------------

#==========================================
# Parametres
#==========================================
EraseOldGraphs = TRUE
DetailledGraphCluster = TRUE

#==========================================
# Tracé des clusters (Tout dans un seul graphe)
#==========================================
if(!exists("clusters")) stop("Il n'y a pas de clusters existants, faire tourner un algo de clustering d'abord")
drawEmptyPlot("Clusters de trajectoires")
colors = rainbow(n = n_distinct(clusters$clusterId)+10)
for (id in unique(clusters$trackId)){
  color=colors[unlist(clusters[clusters$trackId==id,"clusterId"])]
  lines(unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"xCenter"]),
        unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"yCenter"]), 
        col=color)
}

#==========================================
# Tracé 1 graphe par cluster
#==========================================
# Création des sous groupes
if(DetailledGraphCluster){
  for (cId in unique(clusters$clusterId)){
    #print(clusterId)
    if(is.na(cId)) next
    #initLabels()
    idList = unlist(clusters[clusterId==cId,'trackId'])
    drawEmptyPlot(paste("Cluster", cId))
    color=colors[cId]
    for (id in unlist(idList)) {
      lines(unlist(trajectoriesDataset[trajectoriesDataset$trackId == id, "xCenter"]),
            unlist(trajectoriesDataset[trajectoriesDataset$trackId == id, "yCenter"]),
            col = color)
    }
    
    #storeAnnotations(idList[1], color)
    #addAnnotations(0)
  }
}





