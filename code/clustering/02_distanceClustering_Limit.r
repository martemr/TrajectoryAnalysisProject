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

#==========================================
# Parametres
#==========================================
if(!exists("ClusteringClass")) ClusteringClass = 'car' # 'ALL' for all class
clusterDataset <- trajectoriesDataset[trajectoriesDataset$class==ClusteringClass,]

range = unique(clusterDataset$trackId)

if(!exists("seuil")) seuil = 30
interpolationRange=seq(0, 200)
distance='euclidian'

#==========================================
# Librairies
#==========================================
library(dplyr)
library(spatstat)
library(cluster)

#==========================================
# Interpolate and re-sample
#==========================================
# Pre-allocate memory for interpolationMatrix
interpolationMatrix <- matrix(NA, nrow = length(range), ncol = length(interpolationRange))
colnames(interpolationMatrix) <- interpolationRange
rownames(interpolationMatrix) <- range

# Loop through each tId
i=0
for (tId in range) {
  i=i+1
  # Extract x and y data for the current tId
  xData <- clusterDataset[trackId == tId, xCenter]
  yData <- clusterDataset[trackId == tId, yCenter]
  
  # Calculate the interpolation function
  try(test <- smooth.spline(xData, yData),silent=TRUE )
  interpFunc <- splinefun(test, method = "natural")
  
  # Determine the range of x values for interpolation
  xMin <- trunc(min(xData))
  xMax <- trunc(max(xData))
  
  # Resample between xMin and xMax
  subrange <- seq(xMin, xMax)
  interpolatedValues <- data.frame(x = subrange, y = interpFunc(subrange))
  
  # Merge with the interpolation range
  paddedInterpolation <- merge(data.frame(x = interpolationRange), interpolatedValues, all.x = TRUE)
  
  # Add the interpolated values to the interpolationMatrix
  interpolationMatrix[i, ] <- paddedInterpolation$y
}
# 
# interpolationDf <- data.frame(x=interpolationRange,na=interpolationRange*NA)
# for (tId in range)
# {
#   # Calcul de la fonction d'interpolation  
#   fun <-
#     smooth.spline(unlist(clusterDataset[clusterDataset$trackId==tId, "xCenter"]),
#                   unlist(clusterDataset[clusterDataset$trackId==tId, "yCenter"])) %>%
#     splinefun(method = "natural")
#   
#   # Dertermination de la zone de rééchantilllonage
#   xMin=clusterDataset[clusterDataset$trackId==tId, "xCenter"] %>% 
#     unlist %>%
#     min %>%
#     trunc
#   xMax=clusterDataset[clusterDataset$trackId==tId, "xCenter"] %>% 
#     unlist %>%
#     max %>%
#     trunc
#   
#   # Ré-échantillonage entre xMin et xMax
#   subrange <- seq(xMin,xMax)
#   paddedInterpolation <- merge(interpolationDf, data.frame(x=subrange,y=fun(subrange)), all.x = TRUE)
#   interpolationDf <- cbind(interpolationDf, paddedInterpolation$y)
# }
# interpolationMatrix  <- data.frame(t(interpolationDf)[-c(1,2),])
# rownames(interpolationMatrix)=range
# colnames(interpolationMatrix)=interpolationRange




#==========================================
# Calcul de la matrice de distances
#==========================================
# On calcule la matrice de distance inférieurs au seuil
mat <- data.table(t(combn(range, 2)), # combinaisons de paire de ID 
                  dist = as.numeric(dist(interpolationMatrix, method =
                                                               distance)))


p  <- levelplot(
 xlim = xlim,ylim = ylim,
 Speed ~ xCenter * yCenter,
 data = speedArray[, 2:4],
 main = "",
 col.regions = rev(heat.colors(20)),
 at=c(0,50,100),
 cuts = 1)

#plot(p + layer(grid.raster(as.raster(bg_image)), under=TRUE))
mat <- as.numeric(dist(interpolationMatrix, method = distance))
#==========================================
# Répartition dans les clusters
#==========================================
#Convert data.frame to data.table
mat <- as.data.table(mat[dist<seuil,])

# Create a lookup table for trackIds
trackIdLookup <- unique(mat[, .(trackId = c(X1, X2))])

# 


# Self-join mat to get the minimum distance for each track pair
minDist <- mat[mat,on = c(X1 = 'X2', X2 = 'X1'), .(trackId = X1, clusterId = fifelse(dist.x <= dist.y, clusterId.x, clusterId.y), dist = pmin(dist.x, dist.y)), nomatch = 0L]

# Merge the clusterId and dist columns with the trackIdLookup table
clusters <- merge(trackIdLookup, minDist, all.x = TRUE)

# Sort by trackId
clusters <- clusters[order(trackId)]





library(igraph)
library(cluster)

# Create a graph from the pairs
g <- graph_from_edgelist(as.matrix(mat[,1:2]))

# Find the connected components of the graph
cc <- clusters(g)$membership

# Convert the connected components to a list of groups
groups <- split(names(cc), cc)


# 
# clusters <- data.frame(trackId=range, clusterId=NA, dist=Inf)
# clusterCount = 0
# for(m in seq(1:nrow(mat))){
#   X1 <- mat[m,'X1']
#   X2 <- mat[m,'X2']
#   dist <- mat[m,'dist']
#   
#   cluster = clusters[clusters$trackId==X1,'clusterId']
#   if(is.na(cluster)) {
#     clusterCount = clusterCount+1
#     cluster = clusterCount
#     clusters[clusters$trackId==X1,'clusterId'] <- cluster
#     clusters[clusters$trackId==X1,'dist'] <- 0
#   }
#   if (is.na(clusters[clusters$trackId==X2,'clusterId']) ||
#       (dist < clusters[clusters$trackId==X2,'dist'])){
#     clusters[clusters$trackId==X2,'clusterId'] <- cluster # X2 est dans le cluster de X1
#     clusters[clusters$trackId==X2,'dist'] <- dist
#   }
# }
# 
# # On ne garde que les clusters avec plus de 2 courbes
# quantities <- data.table(clusters %>% group_by(clusterId) %>% count)
# clusters <- clusters[clusters$clusterId %in% unlist(quantities[quantities$n>2,'clusterId']),]
# print(paste("Il y a", n_distinct(clusters$clusterId), "clusters."))


#==========================================
# Nettoyage des variables globales
#==========================================
#rm(interpolationDf, paddedInterpolation)
#rm(interpolationDf, paddedInterpolation)
rm(interpolationDf, paddedInterpolation)
rm(interpolationRange, range, subrange)