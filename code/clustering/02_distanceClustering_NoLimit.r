##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 22th
# Description : Recherche de clusters par distances entre courbes
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init

#==========================================
# Parametres
#==========================================
range = unique(trajectoriesDataset$trackId)
seuil = 300
interpolationRange=seq(75, 175)
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
interpolationDf <- data.frame()
for (tId in range)
{
  # Calcul de la fonction d'interpolation  
  fun <-
    smooth.spline(unlist(trajectoriesDataset[trajectoriesDataset$trackId==tId, "xCenter"]),
                  unlist(trajectoriesDataset[trajectoriesDataset$trackId==tId, "yCenter"])) %>%
    splinefun(method = "natural")
  
  # Ré-échantillonage
  interpolationDf= rbind(interpolationDf, 
                         fun(interpolationRange))
}
rownames(interpolationDf)=range

#==========================================
# Calcul de la matrice de distances
#==========================================
distanceMat<- dist(interpolationDf, method=distance)

# On récupère les éléments de la matrice de distance inférieurs au seuil
mat <- data.frame(t(combn(range,2)), dist=as.numeric(distanceMat)) %>%
  filter(dist < seuil)

#==========================================
# Répartition dans les clusters
#==========================================
clusters <- data.frame(trackId=range, clusterId=NA, dist=Inf)
clusterCount = 0
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
