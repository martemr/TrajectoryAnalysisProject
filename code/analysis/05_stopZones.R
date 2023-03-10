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

