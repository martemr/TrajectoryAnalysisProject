##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 27th
# Description : 
##---------------------------------------------

library(cluster)

mesureMetrixMatrix <- subset(trajectoriesDataset[,c("trackId","xCenter","yCenter")]) %>% inner_join(clusters[,1:2], by='trackId')
colnames(mesureMetrixMatrix) <- c("trackId",'x','y','clustering')

mesureMetrixMatrix$x <- as.integer(mesureMetrixMatrix$x)
mesureMetrixMatrix$y <- as.integer(mesureMetrixMatrix$y)
mesureMetrixMatrix$clustering <- as.integer(mesureMetrixMatrix$clustering)

dist_matrix <- dist(mesureMetrixMatrix[,c("x","y")])
silhouette_vals <- silhouette(mesureMetrixMatrix[,c("x","y")]
                              , mesureMetrixMatrix$clustering, dist_matrix)
