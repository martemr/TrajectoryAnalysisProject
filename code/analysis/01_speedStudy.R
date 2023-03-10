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
speedArray <- data.frame('trackId'= speedArrayDataset$trackId,
                         'xCenter'= round(speedArrayDataset$xCenter,1), 
                         'yCenter'= round(speedArrayDataset$yCenter,1), 
                         'Speed'  = as.integer(getSpeed(speedArrayDataset$xVelocity,
                                                        speedArrayDataset$yVelocity)))

#==========================================
# Infractions zones
#==========================================
#setBackground()
# p  <- levelplot(
#   xlim = xlim,ylim = ylim,
#   Speed ~ xCenter * yCenter,
#   data = speedArray[, 2:4],
#   main = "",
#   col.regions = rev(heat.colors(20)),
#   at=c(0,50,100),
#   cuts = 1)

#plot(p + layer(grid.raster(as.raster(bg_image)), under=TRUE))

# INFRACTION QUANTITY
print(paste("Il y a",
  as.integer(length(unique(speedArray[speedArray$Speed>50,'trackId']))/length(unique(speedArray$trackId))*100), "% d'infractions sur cet enregistrement"))
# INFRACTIONS ZONES
drawEmptyPlot("Zones d'infractions")
points(speedArray[as.integer(speedArray$Speed)>unique(recordingMeta$speedLimit)*3.6,c('xCenter','yCenter')], 
       col='red', pch=20)

#==========================================
# Speed study
#==========================================
print(paste("La vitesse moyenne pratiquée est de",
            round(mean(speedArray$Speed)), "km/h"))
