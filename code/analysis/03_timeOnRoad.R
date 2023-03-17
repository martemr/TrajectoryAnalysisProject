##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 01th
# Description : Make trajectories cluster by heading
# Production : Etude du temps passé sur la voie par les VRUs
##---------------------------------------------

#==========================================
# Run 00_init.r with StudiedClass = 'ALL'
# Run origine-destinationClustering.r
#==========================================

#==========================================
# Librairies
#==========================================
library(concaveman)
library(sp)

#==========================================
# Functions
#==========================================

#==========================================
# Détermination de la voie (chaussée)
#==========================================
x = unlist(trajectoriesDataset[class %in% c('car','truck_bus') ,'xCenter'])
y = unlist(trajectoriesDataset[class %in% c('car','truck_bus') ,'yCenter'])

# Compute the convex hull polygon
roadArea <- concaveman(cbind(x, y), concavity =10)
# Plot the points and the convex hull polygon
if(!exists("PlotName")) PlotName="Traversés de pietons"
drawEmptyPlot(PlotName)
lines(roadArea,col='red')

#==========================================
# Détermination de la position des pietons à l'intérieur
#==========================================
x = unlist(trajectoriesDataset[class==StudiedClass ,'xCenter'])
y = unlist(trajectoriesDataset[class==StudiedClass ,'yCenter'])
trackIds = unlist(trajectoriesDataset[class==StudiedClass ,'trackId'])

# Points à l'intérieur du polygone
inside <- as.logical(point.in.polygon(x, y, roadArea[, 1], roadArea[, 2]))
for (tId in unique(trackIds[inside])){
  x = unlist(trajectoriesDataset[trajectoriesDataset$trackId==tId ,'xCenter'])
  y = unlist(trajectoriesDataset[trajectoriesDataset$trackId==tId ,'yCenter'])
  inside <- as.logical(point.in.polygon(x, y, roadArea[, 1], roadArea[, 2]))
  points(x[inside] , y[inside] , pch = 20, col = "green" )
  points(x[!inside], y[!inside], pch = 20, col = "orange")
}

legend(5,-5, 
       "legend"=c("Limites de la chaussée",paste(StudiedClass, "sur la chaussée"), paste(StudiedClass, "sur le trotoir")), 
       col=c('red','green','orange'), lty=1, lwd=c(2,4,4))

print(paste(
  round(n_distinct(trackIds[inside]) / n_distinct(trackIds) *100,2), 
  '% de pietons ont traversé a chaussée'
))


