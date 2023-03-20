##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 08th
# Description : Make trajectories cluster by heading
# Production : Compte du nombre d'usagers
##---------------------------------------------

#==========================================
# Run 00_init.r with StudiedClass = 'ALL'
# Run origine-destinationClustering.r
#==========================================


totalTime = unlist(sum(recordingMeta[locationId==LocationId,'duration']))/60

traficTable <- data.table("Usagers totaux" , round((n_distinct(trajectoriesDataset$trackId)/totalTime),0),'usagers/heure')
traficTable <- 
  rbind(traficTable, list("Véhicules" ,round((n_distinct(trajectoriesDataset[trajectoriesDataset$class %in% c('car','truck_bus'), 'trackId'])/ totalTime), 0), 'usagers/heure'))
traficTable <- 
  rbind(traficTable, list("Piétons" ,round((n_distinct(trajectoriesDataset[trajectoriesDataset$class == 'pedestrian', 'trackId'])/ totalTime), 0), 'usagers/heure'))
traficTable <- 
  rbind(traficTable, list("Vélos" ,round((n_distinct(trajectoriesDataset[trajectoriesDataset$class == 'bicycle', 'trackId'])/ totalTime), 0), 'usagers/heure'))
colnames(traficTable)=c('Type', 'Valeur', 'Unité')
