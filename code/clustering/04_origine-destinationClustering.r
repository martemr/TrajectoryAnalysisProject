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
    tId = unlist(idList[idList$distance == max(idList$distance) & (idList$class == "car"), 'trackId'])
  } else if (method=='random'){
    tId=sample(idList$trackId,1)
  } else if (method=='mean_interpolation'){
    stop("Method not implemented yet")
  } else if (method=='mean_distance'){
    tId=idList[which.min(abs(idList$distance-mean(idList$distance))), trackId]
  }
}

#==========================================
# Clustering par origine et destinations
#==========================================
createAllClustersOfLocalisation <- function(LocationId, tracksMeta){
  if(!exists("clusters")) clusters <<- data.table(trackId=tracksMeta$trackId, locationId=tracksMeta$locationId, class=tracksMeta$class)   # INITIAL 
  
  for (cl in unique(clusters$class)) clusters <<- createClusters(LocationId, cl, clusters)
  
  #tracksMetaBis <<- tracksMeta[clusters[,.(trackId,clusterId)], on=('trackId'), all.x=TRUE]
  # tracksMetaBis <- merge(tracksMeta, clusters, by=('trackId'), all.x=TRUE)
  # tracksMeta <<- tracksMetaBis
  clusterMeta <<- getClusterMeta(tracksMeta, clusters)
  
}

createClusters <- function(LocId, ClusteringClass="car", clusters, minSizecluster=4, eps=0.20){
  clusterDataset <- trajectoriesDataset[trackId %in% clusters[locationId==LocId & class==ClusteringClass, trackId],]
  
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
  clusteringResult <- dbscan::dbscan(scale(tracksMetaCluster[,.(origineHeading,destinationHeading)]), eps = 0.20, minPts = minSizecluster)
  #fviz_cluster(clusteringResult, data = scale(tracksMetaCluster[,.(origineHeading,destinationHeading)])) # Affichage des clusters
  
  # Récupération des clusters
  clusters <- clusters[trackId %in% trajectoriesDataset$trackId,]# Permet d'enlever les trajectoires non souhaités
  clusters[locationId==LocId & class==ClusteringClass, 'clusterId'] <- clusteringResult$cluster+1
  clusters
}


#==========================================
# Ajout des métadonnées des clusters
#==========================================
getClusterMeta <- function(tracksMeta, clusters){
  clusterMeta <- data.table(locationId=numeric(), class=character(), clusterId=numeric(), size=numeric(),representativeId=numeric(), color=character())
  for (cl in c("car", "truck_bus", "pedestrian", "bicycle")) {
    for (LocId in unique(clusters[, locationId])) {
      colors = rainbow(n_distinct(clusters$clusterId))
      for (cId in unique(clusters[class == cl &
                                  locationId == LocId, clusterId])) {
        size = length(unlist(clusters[class == cl &
                                        locationId == LocId & clusterId == cId, 'trackId']))
        color = colors[cId]
        idList = tracksMeta[trackId %in% unlist(clusters[class == cl &
                                                           locationId == LocId &
                                                           clusterId == cId, 'trackId']), .(trackId, distance, class)]
        tId = chooseRepresentativeId(idList, "mean_distance")
        
        clusterMeta <-
          rbind(clusterMeta, list(LocId, cl, cId, size, tId, color), fill = TRUE)
      }
      #if(sum(unique(tracksMeta[trackId %in% clusters$trackId, class])=='pedestrian')) type="ped/h" else type="veh/h"
      type = "veh/h"
      clusterMeta[, 'annotation(veh/h)'] <-
        paste(round(clusterMeta$size / (unlist(
          sum(recordingMeta[locationId == LocId, 'duration'])
        ) / 60), 0), type, sep = "")
      
    }
  }
  
  clusterMeta
}
