##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : April 2023, 06th
# Description : Recherche de clusters par distances entre courbes
##---------------------------------------------

#==========================================
# 
#==========================================
createDistanceClusters <- function(LocId, ClusteringClass="car", minSizecluster=3, plot=FALSE, maxDistance=1000, distanceTreshold = 10){
  print("Creating clusters")
  startTime <- Sys.time()
  
  if(!exists("clusters")) clusters <<- data.table(trackId=tracksMeta$trackId, locationId=tracksMeta$locationId, class=tracksMeta$class)
  clusterDataset <- trajectoriesDataset[trackId %in% clusters[locationId==LocId & class==ClusteringClass, trackId],]
  clusterDataset <- subset(clusterDataset, select = -c(class))
  
  if (ClusteringClass=="pedestrian"){
    clusterDataset <- simplifyDataset(clusterDataset, 1)
  }
  
  range <- unique(clusterDataset[, trackId])
  interpolationRange <- seq(1, 200)
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
    interpolationMatrix <<- interpolationMatrix

  interpolationMatrix[is.na(interpolationMatrix)] <- maxDistance
  
  distance="euclidian"
  mat <- data.table(t(combn(range, 2)), # combinaisons de paire de ID 
                    dist = as.numeric(dist(interpolationMatrix, method =
                                             distance)))
  setorder(mat, dist)
  mat <- as.data.table(mat[!(is.na(dist)),])
  mat <<- mat
  
  clustersTemp <- data.table(trackId = unlist(unique(c(mat$V1, mat$V2))), clusterId=numeric(), distance=numeric())
  clusterCnt <- 1
  for (row in seq(1,nrow(mat))){
    Id1 = mat[row,V1]
    Id2 = mat[row,V2]
    if (mat[row,dist]>distanceTreshold) break
    
    if(!is.na(clustersTemp[trackId==Id1,clusterId])){
      if(!is.na(clustersTemp[trackId==Id2,clusterId])){
        # Tous les deux déja dans un cluster
        # On unit les deux clusters
        cl1 = clustersTemp[trackId==Id1,clusterId]
        cl2 = clustersTemp[trackId==Id2,clusterId]
        clustersTemp[clusterId %in% c(cl1,cl2), clusterId := clusterCnt]
        #replace(clustersTemp, clustersTemp$clusterId %in% c(cl1,cl2), 10000)
        clusterCnt <- clusterCnt+1
        #next
      } else {
        # 1 seul dans un cluster 
        clustersTemp[trackId==Id2,c('clusterId', 'distance') := list(clustersTemp[trackId==Id1,clusterId], mat[row,dist])]
        #clustersTemp[trackId==Id2,cluster] <- clustersTemp[trackId==Id1,cluster]
      }
    } else {
      if(!is.na(clustersTemp[trackId==Id1,clusterId])){
        # 1 seul dans un cluster   
        clustersTemp[trackId==Id1,c('clusterId', 'distance') := list(clustersTemp[trackId==Id2,cluster], mat[row,dist])]
        #clustersTemp[trackId==Id1,cluster] <- clustersTemp[trackId==Id2,cluster]
      } else {
        # Aucun dans un cluster   
        clustersTemp[trackId==Id1,c('clusterId', 'distance') := list(as.numeric(clusterCnt), mat[row,dist])]
        clustersTemp[trackId==Id2,c('clusterId', 'distance') := list(as.numeric(clusterCnt), mat[row,dist])]
        clusterCnt <- clusterCnt+1
      }
    }
    #print(mat[row])
  }
  
  names(clustersTemp)<- c('trackId', 'clusterId', 'distance')
  
  clustersTemp$locationId <- 1
  clustersTemp$class <- "pedestrian"
  
  clusterMeta <- data.table(table(clustersTemp$clusterId))
  names(clusterMeta) <- c('clusterId', 'size')
  clusterMeta <- clusterMeta[size>minSizecluster,]
  clusterMeta$color <- rainbow(nrow(clusterMeta))
  
  endTime <- Sys.time()
  print(paste("Clusters created in", round(endTime-startTime, 2), "secondes"))
  
  if (plot)  drawClusters(selectedClass = "pedestrian", 1, clusters = clustersTemp[clusterId %in% clusterMeta$clusterId] ,clusterMeta = clusterMeta, AllTrajectoriesOnOneGraph = TRUE)
  list(clusterMeta = clusterMeta, clusterTemp = clustersTemp)
  }
