##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 28th
# Description : Zones d'arrets
##---------------------------------------------

#==========================================
# Parametres
#==========================================

#==========================================
# Librairies
#==========================================
library("lattice")
library("latticeExtra")

#==========================================
# Functions
#==========================================
getSpeed <- function(xSpeed,ySpeed){
  (sqrt(xSpeed^2+ySpeed^2))*3.6
}

#==========================================
# Analysis of the speed
#==========================================
if(!exists("speedArrayDataset")) speedArrayDataset <- trajectoriesDataset[trajectoriesDataset$class==StudiedClass,]
speedArray <- data.frame('trackId'= speedArrayDataset$trackId,
                         'xCenter'= round(speedArrayDataset$xCenter,1), 
                         'yCenter'= round(speedArrayDataset$yCenter,1), 
                         'Speed'  = as.integer(getSpeed(speedArrayDataset$xVelocity,
                                                        speedArrayDataset$yVelocity)))

# for (cId in unique(clusters$clusterId)){
#   clusterSpeedArray <- speedArray[trackId %in% unlist(clusters[clusterId==cId, 'trackId']),]
  
  myarray <- aggregate(Speed ~ xCenter + yCenter, data=speedArray, FUN=mean)
  matToPlot <-  matrix(0, 
                       nrow=abs(xlim[2]+1-xlim[1]), 
                       ncol=abs(ylim[2]+1-ylim[1]), 
                       dimnames=list(seq(xlim[1], xlim[2]), seq(ylim[1], ylim[2])))
  
  for (row in seq(1,nrow(myarray))){
    matToPlot[rownames(matToPlot)==myarray[row,'xCenter'],colnames(matToPlot)==myarray[row,'yCenter']] <- myarray[row,'Speed']
  }
  
  # N'affiche pas les valeurs nulles
  matToPlot[matToPlot==0]<- NA
  printHeatMapMatrix(mat = matToPlot, name =paste("Vitesses pratiquées cluster",cId ), subTitle = paste(0, "%des véhicules commettent un excès de vitesse"))
#}

#==========================================
# Draw stop zones
#==========================================
# WAITING QUANTITY
waitingTimes <-  data.frame(table(speedArray[as.integer(speedArray$Speed)==0,'trackId']))
print(paste("Le temps d'attente moyen est de",
            round(mean(waitingTimes$Freq)/25), "seconde(s)"))

# WAITING ZONES
drawEmptyPlot("Zones d'attente")
points(speedArray[as.integer(speedArray$Speed)==0,c('xCenter','yCenter')], 
       col='red', pch=20)

