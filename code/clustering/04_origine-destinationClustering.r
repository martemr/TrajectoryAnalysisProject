##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 21th
# Description : Make trajectories cluster by origine/destination
##---------------------------------------------

#==========================================
# Parametres
#==========================================
DetailledGraphCluster = FALSE
if(!exists("ClusteringClass")) ClusteringClass = 'car' # 'ALL' for all class

######
clusterDataset <- trajectoriesDataset[trajectoriesDataset$class==ClusteringClass,]
range = unique(clusterDataset$trackId)

#==========================================
# Librairies
#==========================================
library(dbscan)
library(factoextra)
#library(tidyverse)

#==========================================
# Clustering par origine et destinations
#==========================================

# Origine
tracksMeta <- tracksMeta[clusterDataset[trackLifetime<5,.(origineHeading=mean(heading)),by = "trackId"]
  , on='trackId'
  , nomatch=0]

# Destination
tracksMeta <- tracksMeta[
  clusterDataset[tracksMeta[,.(trackId, lastFrame5=finalFrame-25)], on="trackId"][frame>lastFrame5,.(destinationHeading=mean(heading)),by=trackId], 
  on="trackId",
  nomatch=0]

# Clustering
clusteringResult <- dbscan::dbscan(scale(tracksMeta[,.(origineHeading,destinationHeading)]), eps = 0.20, minPts =  1)
fviz_cluster(clusteringResult, data = scale(tracksMeta[,.(origineHeading,destinationHeading)])) # Affichage des clusters

# Récupération des clusters
clusters <- data.table('trackId'=tracksMeta$trackId, 'clusterId'=clusteringResult$cluster)

# On ne garde que les clusters avec plus de 2 courbes
clusters <- clusters[clusterId %in% clusters[,.(number=.N),by="clusterId"][number>2,clusterId]]
print(paste("Il y a", n_distinct(clusters$clusterId), "trajectoires différentes"))
clusterDataset <- clusterDataset[clusters, on=.(trackId)]
