##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 28th
# Description : Zones d'exces de vitesse et d'arrets
##---------------------------------------------

#==========================================
# Parametres
#==========================================

#==========================================
# Librairies
#==========================================
library("lattice")
library("latticeExtra")
library("cartography")
library("data.table")

#==========================================
# Functions
#==========================================
getSpeed <- function(xSpeed,ySpeed){
  (sqrt(xSpeed^2+ySpeed^2))*3.6
}

#==========================================
# Analysis of the speed
#==========================================
speedArrayDataset <- trajectoriesDataset[trajectoriesDataset$class==StudiedClass,]
speedArray <- data.table('trackId'= speedArrayDataset$trackId,
                         'xCenter'= round(speedArrayDataset$xCenter,0), 
                         'yCenter'= round(speedArrayDataset$yCenter,0), 
                         'Speed'  = as.integer(getSpeed(speedArrayDataset$xVelocity,
                                                        speedArrayDataset$yVelocity)))

for (cId in unique(clusters$clusterId)){
  clusterSpeedArray <- speedArray[trackId %in% unlist(clusters[clusterId==cId, 'trackId']),]
  
  myarray <- aggregate(Speed ~ xCenter + yCenter, data=clusterSpeedArray, FUN=mean)
  matToPlot <-  matrix(0, 
                       nrow=abs(xlim[2]+1-xlim[1]), 
                       ncol=abs(ylim[2]+1-ylim[1]), 
                       dimnames=list(seq(xlim[1], xlim[2]), seq(ylim[1], ylim[2])))
  
  for (row in seq(1,nrow(myarray))){
    matToPlot[rownames(matToPlot)==myarray[row,'xCenter'],colnames(matToPlot)==myarray[row,'yCenter']] <- myarray[row,'Speed']
  }
  
  # N'affiche pas les valeurs nulles
  matToPlot[matToPlot==0]<- NA
  printHeatMapMatrix(mat = matToPlot, name =paste("Vitesses pratiquées cluster",cId ))
}


#==========================================
# Infractions zones
#==========================================
# INFRACTION QUANTITY
print(paste("Il y a",
  as.integer(length(unique(speedArray[speedArray$Speed>50,'trackId']))/length(unique(speedArray$trackId))*100), "% d'infractions sur cet enregistrement"))
# INFRACTIONS ZONES
# drawEmptyPlot("Zones d'infractions")
# points(speedArray[as.integer(speedArray$Speed)>unique(recordingMeta$speedLimit)*3.6,c('xCenter','yCenter')], 
#        col='red', pch=20)

#==========================================
# Speed study
#==========================================
print(paste("La vitesse moyenne pratiquée est de",
            round(mean(speedArray$Speed)), "km/h"))
