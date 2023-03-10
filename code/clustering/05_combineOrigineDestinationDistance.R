##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 24th
# Description : Recherche de clusters par distances entre courbes
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init
# Run 04_origine-destinationClustering


# On ne prend que les trajectoires rectilignes 
# ClusterID = 2,3

trajectoriesDataset <- trajectoriesDataset %>% inner_join(clusters, by='trackId')
subDatasetCluster = filter(trajectoriesDataset, clusterId==2)

#==========================================
# Parametres
#==========================================
range = unique(subDatasetCluster$trackId)
seuil = 20
interpolationRange=seq(0, 200)
distance='euclidian'

#==========================================
# Librairies
#==========================================
library(dplyr)
library(spatstat)
library(cluster)

#==========================================
# Functions
#==========================================

#==========================================
# Interpolate and re-sample
#==========================================
interpolationDf <- data.frame(x=interpolationRange,na=interpolationRange*NA)
for (tId in range)
{
  # Calcul de la fonction d'interpolation  
  fun <-
    smooth.spline(unlist(subDatasetCluster[subDatasetCluster$trackId==tId, "xCenter"]),
                  unlist(subDatasetCluster[subDatasetCluster$trackId==tId, "yCenter"])) %>%
    splinefun(method = "natural")
  
  # Dertermination de la zone de rééchantilllonage
  xMin=subDatasetCluster[subDatasetCluster$trackId==tId, "xCenter"] %>% 
    unlist %>%
    min %>%
    trunc
  xMax=subDatasetCluster[subDatasetCluster$trackId==tId, "xCenter"] %>% 
    unlist %>%
    max %>%
    trunc
  
  # Ré-échantillonage entre xMin et xMax
  subrange <- seq(xMin,xMax)
  paddedInterpolation <- merge(interpolationDf, data.frame(x=subrange,y=fun(subrange)), all.x = TRUE)
  interpolationDf <- cbind(interpolationDf, paddedInterpolation$y)
}
interpolationMatrix  <- data.frame(t(interpolationDf)[-c(1,2),])
rownames(interpolationMatrix)=range
colnames(interpolationMatrix)=interpolationRange




#==========================================
# Calcul de la matrice de distances
#==========================================
distanceMat<- dist(interpolationMatrix, method=distance)

# On récupère les éléments de la matrice de distance inférieurs au seuil
mat <- data.frame(t(combn(range,2)), dist=as.numeric(distanceMat)) %>%
  filter(dist < seuil)

#==========================================
# Répartition dans les clusters
#==========================================
clusters <- data.frame(trackId=range, clusterId=NA, dist=Inf)
clusterCount = 10
for(m in seq(1:nrow(mat))){
  X1 <- mat[m,'X1']
  X2 <- mat[m,'X2']
  dist <- mat[m,'dist']
  
  cluster = clusters[clusters$trackId==X1,'clusterId']
  if(is.na(cluster)) {
    clusterCount = clusterCount+1
    cluster = clusterCount
    clusters[clusters$trackId==X1,'clusterId'] <- cluster
    clusters[clusters$trackId==X1,'dist'] <- 0
  }
  if (is.na(clusters[clusters$trackId==X2,'clusterId']) ||
      (dist < clusters[clusters$trackId==X2,'dist'])){
    clusters[clusters$trackId==X2,'clusterId'] <- cluster # X2 est dans le cluster de X1
    clusters[clusters$trackId==X2,'dist'] <- dist
  }
}
print(paste("Il y a", n_distinct(clusters$clusterId), "clusters."))

