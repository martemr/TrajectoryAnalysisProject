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


# On ne prend que les données d'un enregistrement
studiedDatasetRecording <- trajectoriesDataset[trackId<100000,]
interractionList <- vector()


drawEmptyPlot("")
for(studiedTrackId in unique(clusters[clusterId == 2 & trackId<100000, trackId])){
#for(studiedTrackId in unique(studiedDatasetRecording[,trackId])){
   #studiedTrackId = 51
      
      
      
  # Selection des trajectoires qui sont au même moment
  idOtherTraj <- unique(studiedDatasetRecording[frame > min(studiedDatasetRecording[trackId==studiedTrackId,frame]) & 
                                       frame < max(studiedDatasetRecording[trackId==studiedTrackId,frame]) 
                                       ,trackId])
  
  # Détermine la zone englobante de la trajectoire
  size=5
  countourPoints <- rbind(
    as.matrix(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter+size*cos((heading+90)*pi/180), 
                                                  y=yCenter+size*sin((heading+90)*pi/180), frame)]),
    as.matrix(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter+size*cos((heading-90)*pi/180), 
                                                  y=yCenter+size*sin((heading-90)*pi/180), frame)]))
  polygonShape <- concaveman(countourPoints)
  
  # drawEmptyPlot("")
  # lines(as.matrix(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter, y=yCenter)]), col='blue')
  # lines(polygonShape,col='red')
  
  # Selectionne les trajectoires dans la zone englobante
  subsetOtherTraj <- studiedDatasetRecording[trackId %in% idOtherTraj,]
  idOtherTrajFiltered <- unique(subsetOtherTraj[as.logical(point.in.polygon(
    unlist(subsetOtherTraj[, .(xCenter)]),
    unlist(subsetOtherTraj[, .(yCenter)]), 
    polygonShape[, 1],
    polygonShape[, 2]
  )), trackId])
  idOtherTrajFiltered <- idOtherTrajFiltered[!idOtherTrajFiltered==studiedTrackId]
  
   #studiedDatasetRecording[]
  
  # for (id in idOtherTrajFiltered){
  #   x = unlist(studiedDatasetRecording[studiedDatasetRecording$trackId==id ,'xCenter'])
  #   y = unlist(studiedDatasetRecording[studiedDatasetRecording$trackId==id ,'yCenter'])
  #   inside <- as.logical(point.in.polygon(x, y, polygonShape[, 1], polygonShape[, 2]))
  #   points(x[inside], 
  #         y[inside], col='orange', pch=19)
  # }
  
  
  # Calcul de la matrice de distance par frame V1
  distMatrixByFrame <- matrix(
    nrow = n_distinct(studiedDatasetRecording[trackId==studiedTrackId, frame]), 
    ncol = n_distinct(idOtherTrajFiltered),
  )
  
  euclidianDistance <- function(a,b){
    if(length(b)==0){
      NA
    } else {
      sqrt((a[1]-b[1])^2+(a[2]-b[2])^2)  
    }
  }
  
  for (f in unique(studiedDatasetRecording[trackId==studiedTrackId, frame])){
    for(tId in idOtherTrajFiltered){
      dist <- euclidianDistance(
        unlist(studiedDatasetRecording[trackId==studiedTrackId & frame==f,.(xCenter, yCenter)]),
        unlist(studiedDatasetRecording[trackId==tId & frame==f,.(xCenter, yCenter)]))
      
        distMatrixByFrame[which(studiedDatasetRecording[trackId==studiedTrackId, frame]==f), 
                          which(idOtherTrajFiltered==tId)] <- dist
    }
  }
  ######################################
  
  indexList = which(distMatrixByFrame < 21)
  couplesInterractions <- data.table(frame=unique(studiedDatasetRecording[trackId==studiedTrackId, frame])
                                       [trunc((indexList-1)%%nrow(distMatrixByFrame))+1], 
                                     id=idOtherTrajFiltered[trunc((indexList-1)/nrow(distMatrixByFrame))+1])
  couplesInterractionsXY <- studiedDatasetRecording[couplesInterractions, .(frame, trackId,xCenter,yCenter), on=.(frame=frame,trackId=id)]
    
  # Affichage des interractions
  print(paste(studiedTrackId, n_distinct(couplesInterractionsXY[,trackId])))
  if (n_distinct(couplesInterractionsXY[,trackId])>0){
    #print(paste("Il y a", n_distinct(couplesInterractionsXY[,trackId]), "objets avec lequels il y a interraction"))
  
    #drawEmptyPlot("")
    lines(studiedDatasetRecording[trackId==studiedTrackId, .(x=xCenter, y=yCenter)], col='blue')
    for(id in idOtherTrajFiltered){
      #drawEmptyPlot("")
      lines(studiedDatasetRecording[trackId==id, .(x=xCenter, y=yCenter)], col='red')
  
      for (f in couplesInterractionsXY[trackId==id,frame]){
        lines(studiedDatasetRecording[frame==f& trackId %in% c(studiedTrackId,id),.(xCenter, yCenter)])
      }
      points(studiedDatasetRecording[trackId==studiedTrackId & frame %in% couplesInterractionsXY[trackId==id,frame], .(x=xCenter, y=yCenter)], col='blue')
      points(studiedDatasetRecording[trackId==id & frame %in% couplesInterractionsXY[trackId==id,frame], .(x=xCenter, y=yCenter)], col='red')
    }
  }
  # number = n_distinct(couplesInterractions$id)
  # names(number) <- studiedTrackId
  # interractionList <- c(interractionList, number)
  #print(paste(studiedTrackId, number))
}

