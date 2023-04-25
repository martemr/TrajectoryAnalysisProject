studiedDataset <- trajectoriesDataset[class=="pedestrian" & 
                                        isOnRoad==TRUE]

drawEmptyPlot("")
points(studiedDataset[,.(xCenter,yCenter)])


points(studiedDataset[,lapply(.SD,first), by=trackId][,.(xCenter,yCenter)])
points(studiedDataset[,lapply(.SD,last), by=trackId][,.(xCenter,yCenter)], col='red')

# Origine
tracksMetaCluster <- tracksMeta[clusterDataset[trackLifetime<5,.(origineHeading=mean(heading)),by = "trackId"]
                                , on='trackId'
                                , nomatch=0]

# Destination
tracksMetaCluster <- tracksMetaCluster[
  clusterDataset[tracksMeta[,.(trackId, lastFrame5=finalFrame-25)], on="trackId"][frame>lastFrame5,.(destinationHeading=mean(heading)),by=trackId], 
  on="trackId",
  nomatch=0]

LocId=4

# Clustering
clusteringResult <- dbscan::dbscan(scale(studiedDataset[locationId==LocId,lapply(.SD,first), by=trackId][,.(xCenter,yCenter)]), eps = 0.20, minPts = 3)
fviz_cluster(clusteringResult, data = scale(studiedDataset[locationId==LocId,lapply(.SD,first), by=trackId][,.(xCenter,yCenter)])) # Affichage des clusters

# Récupération des clusters
test <- data.table(locationId=LocId,
  trackId=unique(studiedDataset[locationId==LocId,trackId]),
                   clusterId=clusteringResult$cluster+1)
testMeta <- data.table(clusterId=unique(test$clusterId),color=rainbow(n_distinct(test$clusterId)))
drawClusters(selectedClass = "pedestrian",LocationId= 4,clusters = test,clusterMeta = testMeta,AllTrajectoriesOnOneGraph = TRUE)
