##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 21th
# Description : Make trajectories cluster by origine/destination
##---------------------------------------------
# 
# #==========================================
# # Parametres
# #==========================================
# if(!exists("ClusteringClass")) ClusteringClass = 'car' # 'ALL' for all class
# if(!exists("recordingIdToSelect")) recordingIdToSelect <- unique(trajectoriesDataset$recordingId)
# clusterDataset <- trajectoriesDataset[recordingId %in% recordingIdToSelect & class==ClusteringClass,]

#==========================================
# Librairies
#==========================================
library(dbscan)
library(factoextra)

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
# Clustering par origine et destinations
#==========================================
createClusters <- function(tracksMeta, minSizecluster=4, ClusteringClass='car'){
  clusterDataset <- trajectoriesDataset[class==ClusteringClass,] #recordingId %in% recordingIdToSelect & 
  
  # Origine
  tracksMetaCluster <- tracksMeta[clusterDataset[trackLifetime<5,.(origineHeading=mean(heading)),by = "trackId"]
    , on='trackId'
    , nomatch=0]
  
  # Destination
  tracksMetaCluster <- tracksMetaCluster[
    clusterDataset[tracksMeta[,.(trackId, lastFrame5=finalFrame-25)], on="trackId"][frame>lastFrame5,.(destinationHeading=mean(heading)),by=trackId], 
    on="trackId",
    nomatch=0]
  
  # Clustering
  clusteringResult <- dbscan::dbscan(scale(tracksMetaCluster[,.(origineHeading,destinationHeading)]), eps = 0.20, minPts =  1)
  #fviz_cluster(clusteringResult, data = scale(tracksMetaCluster[,.(origineHeading,destinationHeading)])) # Affichage des clusters
  
  # Récupération des clusters
  clusters <- data.table('trackId'=tracksMetaCluster$trackId, 'clusterId'=clusteringResult$cluster)
  
  # On ne garde que les clusters avec plus de 4 courbes
  clusters <- clusters[clusterId %in% clusters[,.(number=.N),by="clusterId"][number>minSizecluster,clusterId]]
  clusters <- data.table(clusterId=match(clusters$clusterId, unique(clusters$clusterId)), trackId=clusters$trackId) # Replace clusters values by 1 -> max values
  #print(paste("Il y a", n_distinct(clusters$clusterId), "trajectoires différentes"))
  #clusterDataset <- clusterDataset[clusters, on=.(trackId)]
  clusters
}


#==========================================
# Ajout des métadonnées des clusters
#==========================================
getClusterMeta <- function(tracksMeta, clusters){
  clusterMeta <- data.table(clusterId=numeric(), size=numeric(),representativeId=numeric(), color=character())
  colors=rainbow(n_distinct(clusters$clusterId))
  for (cId in unique(clusters$clusterId)) {
    size = length(unlist(clusters[clusterId == cId, 'trackId']))
    color = colors[cId]
    idList = tracksMeta[trackId %in% unlist(clusters[clusterId == cId, 'trackId']), .(trackId,distanceTraveled,class)]
    tId = chooseRepresentativeId(idList, "mean_distance")
    clusterMeta <- rbind(clusterMeta, list(cId,size,tId,color))
  }
  clusterMeta
}
