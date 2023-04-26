##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Make trajectories cluster by heading
# Production : Un dataframe de clusters (clusters) qui associe à chaque trackId un clusterId
# ___________DEPRECATED___________
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
# Tracé des changements de directions (virages)
#==========================================
range = unique(trajectoriesDataset$trackId)
plot(NULL, xlim=c(-180,180), ylim=c(0,500),xlab="Heading", ylab="Time", main="Variations of heading")
for (ind in range){
  id=ind
  h0 = unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"heading"])[1]
  points(unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"heading"])-h0,
         unlist(trajectoriesDataset[trajectoriesDataset$trackId==id,"group"]))
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
