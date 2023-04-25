##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 15 th
# Description : ...
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init

#==========================================
# Parametres
#==========================================

#==========================================
# Librairies
#==========================================
library(rgl)
library(concaveman)
library(sp)


#==========================================
# Fonctions de sélection des ID
#==========================================
# TRI PAR TEMPORALITE -----
selectSameTimeId <- function(studiedDatasetRecording, studiedTrackId, idToFilter=unique(studiedDatasetRecording$trackId)){
  ids <- unique(idToFilter[idToFilter %in% unique(studiedDatasetRecording[frame > min(studiedDatasetRecording[trackId == studiedTrackId,frame]) & 
                                              frame < max(studiedDatasetRecording[trackId == studiedTrackId,frame]) 
                                              ,trackId])])
  ids[ids!=studiedTrackId]
}
# -------------------------

# TRI PAR ESPACE ----------
selectSameSpace <- function(studiedDatasetRecording, studiedTrackId, idToFilter=unique(studiedDatasetRecording$trackId), areaSize=5, plotArea=FALSE){ # areaSize = X metres de chaque coté de la trajectoire
  # Détermine la zone englobante de la trajectoire (zone d'interraction)
  countourPoints <- rbind(
    as.matrix(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter+areaSize*cos((heading+90)*pi/180), 
                                                                 y=yCenter+areaSize*sin((heading+90)*pi/180), frame)]),
    as.matrix(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter+areaSize*cos((heading-90)*pi/180), 
                                                                 y=yCenter+areaSize*sin((heading-90)*pi/180), frame)]))
  polygonShape <- concaveman(countourPoints)
  if(plotArea) lines(polygonShape, col='red')

  # Selectionne les trajectoires dans la zone englobante
  unique(studiedDatasetRecording[trackId %in% idToFilter & as.logical(point.in.polygon(
    unlist(studiedDatasetRecording[trackId %in% idToFilter, .(xCenter)]),
    unlist(studiedDatasetRecording[trackId %in% idToFilter, .(yCenter)]),
    polygonShape[, 1],
    polygonShape[, 2]
  )), trackId])
}
# -------------------------

# TRI PAR DISTANCES ENTRE POINTS
selectClosestPoints <- function(studiedDatasetRecording, studiedTrackId, idList, distTreshold = 21){
  # Matrice de distance : lignes=frames, colonnes=Trajectoires
  distMatrixByFrame <- matrix(
    nrow = n_distinct(studiedDatasetRecording[trackId==studiedTrackId, frame]), 
    ncol = n_distinct(idList),
  )
  # Calcul de la matrice
  for (f in unique(studiedDatasetRecording[trackId==studiedTrackId, frame])){
    for(tId in idList){
      distMatrixByFrame[
        which(studiedDatasetRecording[trackId==studiedTrackId, frame]==f),
        which(idList==tId)
        ] <- euclidean(unlist(studiedDatasetRecording[trackId == studiedTrackId & frame == f, .(xCenter, yCenter)]),
                       unlist(studiedDatasetRecording[trackId == tId            & frame == f, .(xCenter, yCenter)]))
    }
  }
  # Sélection des éléments proches (seuil de distance)
  indexList = which(distMatrixByFrame < distTreshold)
  couplesInterractions <- data.table(frame = unique(studiedDatasetRecording[trackId==studiedTrackId, frame])[trunc((indexList-1)%%nrow(distMatrixByFrame))+1], 
                                     id    = idList[trunc((indexList-1)/nrow(distMatrixByFrame))+1])
  couplesInterractionsXY <- studiedDatasetRecording[couplesInterractions, .(frame, trackId,xCenter,yCenter), on=.(frame=frame,trackId=id)]
  couplesInterractionsXY[!is.na(couplesInterractionsXY$xCenter)]
}

#==========================================
# Fonction d'affichage des interractions
#==========================================
printInterractions <- function(studiedDatasetRecording, studiedTrackId, couplesInterractionsXY){
  if (n_distinct(couplesInterractionsXY[,trackId])>0){
    drawEmptyPlot("Interractions")
    # Tracé de la trajectoire étudiée
    lines(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter, y=yCenter)], col='green', lwd=6)
    addArrow(studiedTrackId, color='green')
    # Tracé des interractions
    cnt=1
    for(id in unique(couplesInterractionsXY[,trackId])){
      color=rainbow(15)[cnt]
      lines(studiedDatasetRecording[trackId==id, .(x=xCenter, y=yCenter)], col=color, lwd=4)
      addArrow(id, color=color)
      
      # Affichage des points où il y a interraction et leurs liens
      points(studiedDatasetRecording[trackId==studiedTrackId & frame %in% couplesInterractionsXY[trackId==id & !is.na(xCenter),frame], .(x=xCenter, y=yCenter)], col=color, pch=19)
      points(studiedDatasetRecording[trackId==id             & frame %in% couplesInterractionsXY[trackId==id & !is.na(xCenter),frame], .(x=xCenter, y=yCenter)], col=color, pch=19)
      for (f in couplesInterractionsXY[trackId==id,frame]){
        lines(studiedDatasetRecording[frame==f& trackId %in% c(studiedTrackId,id),.(xCenter, yCenter)], col=color)
      }
      cnt=cnt+1
    }
  }
}


#==========================================
# Affichage des interractions d'une trajectoire
#==========================================
plotTrackInterractions <- function(studiedTrackId, rId){
  studiedTrackId <- as.integer(studiedTrackId)
  rId <- as.integer(rId)
  studiedDatasetRecording <- trajectoriesDataset[recordingId==rId,]
  # On ne va s'intéresser qu'aux trajectoires qui ne sont pas dans le même cluster
  # idList <- unique(studiedDatasetRecording[
  #   #class %in% c("pedestrian","bicycle") & # QUE PIETONS ET VELOS
  #     !(trackId %in% clusters[clusterId==1, trackId]), # PAS DANS LE MEME CLUSTER 
  #   trackId])
  idList <- unique(studiedDatasetRecording[,trackId])
  idList <- selectSameTimeId(studiedDatasetRecording, studiedTrackId, idList) 
  # idList <- idList[idList != studiedTrackId] # On enleve la trajectoire étudié du dataset
  #print(paste("Trajectoires au même moment :",list(idList)))
  
  idList <- selectSameSpace(studiedDatasetRecording, studiedTrackId, idList)
  #print(paste("Trajectoires au même endroit", list(idList)))
  
  couplesInterractionsXY <- selectClosestPoints(studiedDatasetRecording, studiedTrackId, idList)
  #print(paste("La trajectoire", studiedTrackId, "a un interraction avec la trajectoire", list(unique(couplesInterractionsXY$trackId))))
  printInterractions(studiedDatasetRecording, studiedTrackId, couplesInterractionsXY)
  list(unique(couplesInterractionsXY$trackId))
}



#==========================================
# Etude des interraction pour 1 enregistrement
#==========================================
# Selection des données à analyser
# 
# studiedRecordId = 0
# studiedDatasetRecording <- trajectoriesDataset[recordingId==studiedRecordId,]
# for (i in clusters[clusterId==1, trackId]){
#   plotTrackInterractions(700008)
# }

