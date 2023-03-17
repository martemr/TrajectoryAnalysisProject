##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Initialisation des données
##---------------------------------------------

#rm(list = ls())
#==========================================
# Parametres
#==========================================
## Chemins  
if(!exists("LocationId") & !exists("RecordNumbers")) {LocationId <- 1}
#LocationId <- 4
RecordNumbers <- 0
if(!exists("StudiedClass")) StudiedClass = 'car' # 'ALL' for all class
if(!exists("distanceMin")) distanceMin = 15 # Filtre de distance minimum à parcourir
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
calculateDistance <- function(lx, ly) {
  dx <- diff(lx)
  dy <- diff(ly)
  dist <- sqrt(dx ^ 2 + dy ^ 2)
  return(sum(dist))
}

#==========================================
# Chargement des Metadonnées
#==========================================
## Récupération des noms de fichiers
#setwd(paste("C:/Users/martin.emery/Documents/Courbes_trajectoires/",dosinit,sep=""))

# Charge toutes les données des recordings
recordingMeta <- data.table()
for (i in 0:32) {
  recordingMetaName <- sprintf("%s%02d_recordingMeta.csv", dosinit, i)
  recordingMeta <- rbindlist(list(recordingMeta, 
                                  fread(recordingMetaName, header = TRUE, sep = ",")))
}


# Calcule la durée de chacune en minute
recordingMeta[, `duration(m)` := duration / 60]
RecordNumbers <- unlist(recordingMeta[recordingMeta$locationId==LocationId,'recordingId'])

#==========================================
# Lecture des données (Fusion de plusieurs tableaux de données)
#==========================================
library(data.table)

tracks <- data.table()
tracksMeta <- data.table()

for (i in unlist(RecordNumbers)){
  tracksName     <- sprintf("%s%02d_tracks.csv",    dosinit, i)
  tracksMetaName <- sprintf("%s%02d_tracksMeta.csv",dosinit, i)
  tracks <- rbindlist(list(tracks, fread(tracksName, header = TRUE, sep = ',')))
  tracksMeta <- rbindlist(list(tracksMeta, fread(tracksMetaName, header = TRUE, sep = ',')))
}

# Modification des tracksId
tracks[, trackId := trackId + recordingId*100000]
tracksMeta[, trackId := trackId + recordingId*100000]


#==========================================
# Ajout de la distance parcourue dans les meta données
#==========================================
tracksMeta <- tracks %>%
  group_by(trackId) %>%
  summarise('distanceTraveled' = calculateDistance(xCenter, yCenter)) %>%
  cbind(tracksMeta, .)

#==========================================
# Nettoyage du dataset : Selection de la classe étudiée
#==========================================
# On ne garde pas les véhicules garés ou objets ne se déplaçant pas de plus de DISTANCE MIN sur l'enregistrement.
trajectoriesDataset <- tracks[trackId %in% unlist(tracksMeta[tracksMeta$distanceTraveled > distanceMin, 'trackId']),]
# Ajout de la classe de l'objet
trajectoriesDataset <- inner_join(trajectoriesDataset, tracksMeta[,c('trackId','class')], by='trackId')


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

# 
if(StudiedClass!='ALL'){
    filter(trajectoriesDataset, trackId %in% tracksMeta[tracksMeta$class %in% StudiedClass ,'trackId'])
}
if(!is.na(trajectoryIdMax)){
  trajectoriesDataset <- filter(trajectoriesDataset, trackId<trajectoryIdMax)
}
# On créé un dataset groupé
#trajectoriesGrouped <- trajectoriesDataset %>% 
#  group_by(trackId) %>% 
#  summarize(x = list(xCenter), y = list(yCenter))

#==========================================
# Simplification des données (25fps -> 5fps)
#==========================================
setDT(trajectoriesDataset)
trajectoriesDataset <- trajectoriesDataset[, group := trunc(trackLifetime / 5)]
trajectoriesDataset <- trajectoriesDataset[, group := trunc(frame / 5)]
trajectoriesDataset <- trajectoriesDataset[, lapply(.SD, mean), by = .(trackId, group), .SDcols = is.numeric]
trajectoriesDataset <- inner_join(trajectoriesDataset[,3:ncol(trajectoriesDataset)], tracksMeta[,c('trackId','class')], by='trackId')

#==========================================
# Préparation de l'image
#==========================================
source("~/TrajectoryAnalysisProject/code/01_plotUtils.R", echo=FALSE)

#==========================================
# Nettoyage des variables utilisés pour l'initialisation
#==========================================
rm(calculateDistance)
rm(bgName, distanceMin, i, recordingMetaName)
rm(SimplifyData, tracksMetaName, tracksName, trajectoryIdMax)
#rm(recordingMeta, recordingMetaName)