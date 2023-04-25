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

hasDoneInfraction <- function(tId){
  speedArrayTrack <- getSpeed(trajectoriesDataset[trackId==tId,.(xVelocity, yVelocity)][,1],
                              trajectoriesDataset[trackId==tId,.(xVelocity, yVelocity)][,2])
  for(speed in speedArrayTrack){
    # Compute if the speed is above limit of 5km/h for 3s.
    # Return True if yes
  }
}

getInfractionRate <- function(cId){
  # Applique la fonction hasdoneinfractions a tout le cluster
}

#==========================================
# Analysis of the speed
#==========================================
library(plotly)
s <- matrix(c(1, -.75, -.75, 1), ncol = 2)

obs <- mvtnorm::rmvnorm(500, sigma = s)

m <- ggplot(trajectoriesDataset[,.(xCenter,yCenter)]) +
  geom_point() +
  xlim(0.5, 6) +
  ylim(40, 110)
m + geom_density_2d()

# library
library(latticeExtra) 

# create data
set.seed(1) 
data.frame(x = rnorm(100), y = rnorm(100)) 
data <- trajectoriesDataset[1:100, .(xCenter, yCenter, xVelocity)]

# showing data points on the same color scale 
levelplot(xVelocity ~ xCenter * yCenter, data, 
          panel = panel.levelplot.points, cex = 1.2
) + 
  layer_(panel.2dsmoother(..., n = 200))


geom_density_2d(data = trajectoriesDataset[,.(xCenter,yCenter)])
fig <- plot_ly(trajectoriesDataset[,.(xCenter,yCenter)], xlim=c(0,100)) #x = obs[,1], y = obs[,2]) 
fig <- fig %>% add_trace(type='histogram2dcontour')


fig


createSpeedHeatMap <- function(input,output){
  myarray <- aggregate(Speed ~ xCenter + yCenter, data=trajectoriesDataset[, Speed:=getSpeed(xCenter,yCenter)], FUN=mean)
  
  matToPlot <-  matrix(0, 
                       nrow=abs(xlim[2]+1-xlim[1]), 
                       ncol=abs(ylim[2]+1-ylim[1]), 
                       dimnames=list(seq(xlim[1], xlim[2]), seq(ylim[1], ylim[2])))
  
  matToPlot <-  matrix()
  for (row in seq(1,nrow(myarray))){
    matToPlot[rownames(matToPlot)==myarray[row,'xCenter'],colnames(matToPlot)==myarray[row,'yCenter']] <- myarray[row,'Speed']
  }
  
  # N'affiche pas les valeurs nulles
  matToPlot[matToPlot==0]<- NA
  printHeatMapMatrix(mat = matToPlot, name =paste("Vitesses pratiquées cluster"))
  
  library(latticeExtra)
  
  
  
  speedArray <- trajectoriesDataset[]
  
  
  
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
    printHeatMapMatrix(mat = matToPlot, name =paste("Vitesses pratiquées cluster",cId ), subTitle = paste(getInfractionRate(cId), "%des véhicules commettent un excès de vitesse"))
  }
  
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
