##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 01th
# Description : Make trajectories cluster by heading
# Production : Etude de changement de voies 
##---------------------------------------------

#==========================================
# Librairies
#==========================================
library(dbscan)
library(factoextra)
library(png)
library(tidyverse)

#==========================================
# Functions
#==========================================
getTurn <- function(l){
  print(l[1])
  l <- l-l[1]
  if(any(l<(-30))){ # LEFT
    1# 'red'
  } else if(any(l>30)){ # RIGHT
    2# 'blue'
  } else { # STRAIGHT
    3#'green'
  }
}

#==========================================
# Selection des clusters de ligne droite
#==========================================
clusterIds <- c(3)
range = unique(trajectoriesDataset$trackId)
plot(NULL, xlim=c(-50,50), ylim=c(0,100),xlab="Heading", ylab="Time", main="Variations of heading")
for (ind in range){
  id=ind
  #id=9
  if (clusters[clusters$trackId==id,"clusterId"] %in% clusterIds){
      h0 = (unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"heading"])[1]+180 )%% 360
      points((unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"heading"])+180 )%% 360-h0,
         unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"group"]),pch=20)
      text(last((unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"heading"])+180 )%% 360-h0),
           last(unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"group"])),
           labels=id,
           col='red')
      
  }
  
}

#==========================================
# Clustering par virages
#==========================================
clusters <-trajectoriesDataset %>%
  group_by(trackId) %>% 
  summarize("clusterId"=getTurn(heading))

#==========================================
# Nettoyage
#==========================================
rm(getTurn, id, ind, h0)
