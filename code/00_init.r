##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Initialisation des données
##---------------------------------------------


#==========================================
# Parametres
#==========================================
## Chemins  
if(!exists("LocationId") ) {LocationId <- 1}
if(!exists("StudiedClass")) StudiedClass = 'car' # 'ALL' for all class
if(!exists("distanceMin"))  distanceMin = 15 # Filtre de distance minimum à parcourir
dosinit <- "./data/"
trajectoryIdMax = NA # NA for all trajectories
SimplifyData = TRUE

#==========================================
# Librairies
#==========================================
library(dplyr)
library(png)
library(data.table)

#==========================================
# Functions
#==========================================
# Calcul de la distance parcourue par le véhicule (Exprimée en m)
# calculateDistance <- function(lx, ly) {
#   dx <- diff(lx)
#   dy <- diff(ly)
#   dist <- sqrt(dx ^ 2 + dy ^ 2)
#   return(sum(dist))
# }
euclidean <- function(a, b) sqrt(sum((a - b)^2))


#==========================================
# Chargement des Données
#==========================================
loadData <- function(dosinit, LocationId){
  print(paste("Chargement des données de la localisation :", LocationId))
  # Charge toutes les Metadonnées des recordings
  recordingMeta <- data.table()
  for (i in 0:32) {
    recordingMetaName <- sprintf("%s%02d_recordingMeta.csv", dosinit, i)
    recordingMeta <- rbindlist(list(recordingMeta, 
                                    fread(recordingMetaName, header = TRUE, sep = ",")))
  }
  
  # Calcule la durée de chacune en minute
  recordingMeta[, `duration` := duration / 60]
  
  # Lecture des tracks
  tracks <- data.table()
  tracksMeta <- data.table()
  for (i in unlist(recordingMeta[locationId==LocationId, recordingId])){
    tracks <- rbindlist(list(tracks, 
                             fread(sprintf("%s%02d_tracks.csv",    dosinit, i), header = TRUE, sep = ',')))
    tracksMeta <- rbindlist(list(tracksMeta, 
                                 fread(sprintf("%s%02d_tracksMeta.csv",dosinit, i), header = TRUE, sep = ',')))
  }
  
  # Décalage des tracksId
  tracks[, trackId := trackId + recordingId*100000]
  tracksMeta[, trackId := trackId + recordingId*100000]
  
  # Ajout de la distance parcourue dans les métadonnées
  tracksMeta <- tracks[, .(distanceTraveled = euclidean(xCenter, yCenter)), by = trackId][tracksMeta, on = "trackId"]
  
  tracks <<- tracks
  tracksMeta <<- tracksMeta
  print("Chargement des données terminé")
}

#==========================================
# Nettoyage du dataset 
#==========================================
cleanDataset <- function(distanceMin=20){
  print("Nettoyage des données")
  # Selection par distance minimale parcourue
  trajectoriesDataset <- tracks[trackId %in% unlist(tracksMeta[tracksMeta$distanceTraveled > distanceMin, 'trackId']),]
  # Ajout de la classe de l'objet
  trajectoriesDataset <- tracksMeta[,.(trackId,class), by=trackId][trajectoriesDataset, on='trackId']

  # Selection des trajectoires entières
  # On ne garde pas les trajectoires qui ne sont potentiellement pas entières : éléments présents sur les 5 premières et 5 dernières frame
  maxs <- aggregate(frame ~ recordingId, data = trajectoriesDataset, max)
  #recordingMeta <- cbind(recordingMeta, 'lastFrame'=maxs)
  maxs <- rbind(maxs,
                list(maxs$recordingId, maxs$frame-1),
                list(maxs$recordingId, maxs$frame-2),
                list(maxs$recordingId, maxs$frame-3),
                list(maxs$recordingId, maxs$frame-4),
                data.table('recordingId'=unique(maxs$recordingId), 'frame'=0),
                data.table('recordingId'=unique(maxs$recordingId), 'frame'=1),
                data.table('recordingId'=unique(maxs$recordingId), 'frame'=2),
                data.table('recordingId'=unique(maxs$recordingId), 'frame'=3),
                data.table('recordingId'=unique(maxs$recordingId), 'frame'=4))
  
  tracksToRemove <- subset(trajectoriesDataset, recordingId %in% maxs$recordingId & frame %in% maxs$frame)[,'trackId'] %>% unique
  trajectoriesDataset <- trajectoriesDataset[!(trajectoriesDataset$'trackId' %in% (tracksToRemove$trackId)),]
  rm(maxs, tracksToRemove)
  
  # Simplification des données (25fps -> 5fps)
  trajectoriesDataset <- trajectoriesDataset[, group := trunc(trackLifetime / 5)]
  trajectoriesDataset <- trajectoriesDataset[, group := trunc(frame / 5)]
  trajectoriesDataset <- trajectoriesDataset[, lapply(.SD, mean), by = .(trackId, group), .SDcols = is.numeric]
  trajectoriesDataset <- inner_join(trajectoriesDataset[,3:ncol(trajectoriesDataset)], tracksMeta[,c('trackId','class')], by='trackId')
  
  print("Données nettoyés")
  # Retourne le dataset nettoyé
  trajectoriesDataset
}

#print(paste("Pré-traitement :", n_distinct(tracks[!(trackId %in% unique(trajectoriesDataset$trackId)),trackId]), "trajectoires ont été retirés sur",  n_distinct(tracks$trackId)))