##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Initialisation des données
##---------------------------------------------

#==========================================
# Librairies
#==========================================
library(dplyr)
library(data.table)
library(concaveman)
library(sp) # Pointin polygon
library(logr) # Logs

#==========================================
# Calcul de distance
#==========================================
euclidean <- function(a, b) sqrt(sum((a - b)^2))
sum_distance <- function(x, y) {
  n <- length(x)
  total_distance <- 0
  for (i in 1:(n-1)) {
    distance <- sqrt((x[i+1] - x[i])^2 + (y[i+1] - y[i])^2)
    total_distance <- total_distance + distance
  }
  return(total_distance)
}

#==========================================
# Chargement des Données
#==========================================
loadData <- function(dosinit="./data/"){
  print(paste("Chargement des données"))
  # Charge toutes les Metadonnées des recordings
  recordingMeta <- data.table()
  for (i in 0:32) {
    recordingMetaName <- sprintf("%s%02d_recordingMeta.csv", dosinit, i)
    recordingMeta <- rbindlist(list(recordingMeta, fread(recordingMetaName, header = TRUE, sep = ",")))
  }
  
  # Calcule la durée de chacune en minute
  recordingMeta[, `duration` := duration / 60]
  
  # Lecture des tracks
  tracks <- data.table()
  tracksMeta <- data.table()
  for (i in unlist(recordingMeta[, recordingId])){
    tracks <- rbindlist(list(tracks, 
                             fread(sprintf("%s%02d_tracks.csv",    dosinit, i), header = TRUE, sep = ',')))
    tracksMeta <- rbindlist(list(tracksMeta, 
                                 fread(sprintf("%s%02d_tracksMeta.csv",dosinit, i), header = TRUE, sep = ',')))
  }
  
  # Décalage des tracksId par recording ID
  tracks[, trackId := trackId + recordingId*100000]
  tracksMeta[, trackId := trackId + recordingId*100000]
  
  # Ajout de la distance parcourue dans les métadonnées
  distance <- tracks[, .(distance = sum_distance(unlist(.SD[, xCenter]), unlist(.SD[, yCenter]))), by = trackId]
  tracksMeta <- merge(tracksMeta, distance, by = "trackId")
  
  # Ajout de la localisation partout
  tracksMeta <- tracksMeta[recordingMeta[,.(recordingId,locationId)], on=('recordingId'), nomatch = NULL]
  tracks <- tracks[tracksMeta[,.(trackId,locationId)], on=('trackId'), nomatch = NULL]

  # Définition des variables globales
  tracks <<- tracks
  tracksMeta <<- tracksMeta
  recordingMeta <<- recordingMeta
  print("Chargement des données terminé")
  
  # Ajout de dosinit aux variables globales
  dosinit <<- dosinit
}


#==========================================
# Détermination de la voie (chaussée)
#==========================================
findRoad <- function(toPlot = FALSE, trajectoriesDataset, LocId){
  x = unlist(trajectoriesDataset[class %in% c('car','truck_bus') & recordingId %in% recordingMeta[locationId==LocId, recordingId],'xCenter'])
  y = unlist(trajectoriesDataset[class %in% c('car','truck_bus') & recordingId %in% recordingMeta[locationId==LocId, recordingId],'yCenter'])
  
  # Compute the convex hull polygon
  roadArea <- concaveman(cbind(x, y))
  if(toPlot){
    drawEmptyPlot("")
    lines(roadArea,col='red', lwd=2)
  }
  roadArea
}

#==========================================
# Fonction de simplification des données
#==========================================
simplifyDatasetByLifeTime <- function(dataset, fps=5){
  setDT(dataset)
  clonedDataset <- dataset[, group:= trunc(frame /(25/fps))]
  #startTime <- Sys.time()
  simplified <- clonedDataset[, lapply(.SD, mean), by = .(trackId, group)]
  simplified[,frame:=as.integer(frame)]
  simplified[,trackLifetime:=as.integer(trackLifetime)]
  #endTime <- Sys.time()
  #runtime = endTime-startTime
  #list(runtime, clonedDataset)
  simplified
}

simplifyDatasetByFrame <- function(dataset, fps){
  simplified <- dataset[(frame %% fps)==0]
}

#==========================================
# Nettoyage du dataset 
#==========================================
cleanDataset <- function(distanceMin=20, fps=5){
  print("Nettoyage des données")
  # Selection par distance minimale parcourue
  trajectoriesDataset <- tracks[trackId %in% unlist(tracksMeta[tracksMeta$distance > distanceMin, 'trackId']),]

  # Selection des trajectoires entières
  # On ne garde pas les trajectoires qui ne sont potentiellement pas entières : éléments présents sur les 5 premières et 5 dernières frame
  maxs <- setDT(aggregate(frame ~ recordingId, data = trajectoriesDataset, max))[,frame := frame-5]

  toRemove <- tracksMeta[maxs, on=.(recordingId,finalFrame>=frame)][,.(recordingId,trackId)]
  toRemove <- rbind(toRemove, tracksMeta[initialFrame<5, .(recordingId, trackId)])
  
  trajectoriesDataset <- trajectoriesDataset[!unlist(trajectoriesDataset[,.(recordingId %in% toRemove$recordingId & trackId %in% toRemove$trackId)])]
  #################### >
  
  #trajectoriesDataset <- trajectoriesDataset[!(trajectoriesDataset$'trackId' %in% (tracksToRemove$trackId)),]]

  # Simplification des données (25fps -> 5fps)
  trajectoriesDataset <- simplifyDatasetByLifeTime(trajectoriesDataset, fps=fps)

  # Ajout de la classe de l'objet
  trajectoriesDataset <- trajectoriesDataset[tracksMeta[,.(trackId,class)], on=('trackId'), nomatch = NULL]

  # Ajout de l'information "sur la route"
  for(LocalisationId in unique(recordingMeta$locationId)){
    roadArea = findRoad(trajectoriesDataset=trajectoriesDataset, LocId = LocalisationId)
    trajectoriesDataset[locationId==LocalisationId, isOnRoad := as.logical(point.in.polygon(xCenter, yCenter, roadArea[, 1], roadArea[, 2]))]
  }
  
  # Ajout de la localisation partout
  #trajectoriesDataset <- trajectoriesDataset[tracksMeta[,.(trackId,class)], on=('trackId'), nomatch = NULL]

  print("Données nettoyés")
  
  # Retourne le dataset nettoyé
  trajectoriesDataset <<- trajectoriesDataset
}
# 
# clusteringResult <- dbscan::dbscan(scale(trajectoriesDataset[class %in% c('car','truck_bus') & locationId==1,.(xCenter, yCenter)]), eps = 0.20, minPts =2)
# fviz_cluster(clusteringResult, data =scale(trajectoriesDataset[class %in% c('car','truck_bus') &locationId==1,.(xCenter, yCenter)])) # Affichage des clusters

#print(paste("Pré-traitement :", n_distinct(tracks[!(trackId %in% unique(trajectoriesDataset$trackId)),trackId]), "trajectoires ont été retirés sur",  n_distinct(tracks$trackId)))