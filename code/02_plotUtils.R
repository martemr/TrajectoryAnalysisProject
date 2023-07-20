##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 07th
# Description : Fonctions pour le tracé de courbes
##---------------------------------------------

#==========================================
# Librairies
#==========================================
library(png)

#==========================================
# Initialisation des paramêtres
#==========================================
initPlotImage <- function(LocationId){
  fact <- switch(LocationId, 
                 '1' = 10.2,
                 '2' = 10.2, 
                 '3' = 10.2, 
                 '4' = 6.5)
  bgName <- switch(LocationId, 
                    '1' = sprintf("%s%02d_background.png", dosinit, 7), 
                    '2' = sprintf("%s%02d_background.png", dosinit, 18), 
                    '3' = sprintf("%s%02d_background.png", dosinit, 30), 
                    '4' = sprintf("%s%02d_background.png", dosinit, 0))
  
  bg_image <<- readPNG(bgName)
  Xlim <<- c(0, dim(bg_image)[2]/fact)
  Ylim <<- c(-dim(bg_image)[1]/fact, 0)
}

#==========================================
# Tracé d'un graphe vide
#==========================================
drawEmptyPlot <- function(locId, PlotName="",Background=TRUE, dosinit="./data/"){
  initPlotImage(locId)
  plot(NULL,xlim=Xlim,ylim=Ylim,axes=T,xlab="X",ylab="Y",main=PlotName)
  if(Background) {
    lim <- par()
    rasterImage(bg_image, 
                xleft=Xlim[1], xright=Xlim[2], 
                ybottom=Ylim[1], ytop=Ylim[2])
  }
}

#==========================================
# Tracé d'un vecteur
#==========================================
drawVector <- function(x,y,angle,size=10, col='blue'){
  angle = angle * (pi / 180)
  arrows(x0=x,
         x1=(x + size * cos(angle)),
         y0=y, 
         y1=(y + size * sin(angle)),
         lwd = 2, col = col)
}

#==========================================
# Attribuer une couleur par classe
#==========================================
getCol <- function(class){
  switch(class,
         'car'='red', 
         'truck_bus'='yellow',
         'pedestrian'='blue',
         'bicycle'='green')
}

#==========================================
# Tracé d'UNE trajectoire
#==========================================
drawTrajectory <- function(LocationId, tId, type="l", col='black', add=FALSE, lwd=1){
  if(!add){
    #initPlotImage(LocationId, dosinit)
    drawEmptyPlot(locId=LocationId, PlotName = paste("Trajectoire", tId))
  }
  points(tracks[trackId==tId, list(xCenter, yCenter)], type=type, col=col, lwd=lwd)
}

#==========================================
# Tracé des trajectoires
#==========================================
drawTrajectories <- function(LocationId, AllTrajectoriesOnOneGraph = TRUE, StudiedClass='ALL', legend=FALSE){
  subDataset <- trajectoriesDataset[recordingId %in% (recordingMeta[locationId==LocationId, recordingId]),]
  if(AllTrajectoriesOnOneGraph){
    drawEmptyPlot(locId=LocationId,PlotName="Trajectoires")
  }
  if (StudiedClass=='ALL') classList <- unique(subDataset$class) else classList <- StudiedClass
  if (!AllTrajectoriesOnOneGraph) par(mfrow=c(2,2))# Grille
  for (cl in classList)
  {
    if (!AllTrajectoriesOnOneGraph) drawEmptyPlot(locId=LocationId,PlotName=paste("Trajectories of", cl))
    color=getCol(cl)
    lapply(unique(subDataset[class == cl, trackId]), function(x) drawTrajectory(LocationId, x,col=color,add=T))
  }
  if(legend) legend(1,1, legend=c('Voitures','Bus','Piétons','Cyclistes'),
                    col=c('red','yellow','blue', 'green'), lty=1, cex=0.8, lwd=2)
}

#==========================================
# Tracé des clusters
#==========================================
drawClusters <- function(LocationId, clusters, clusterMeta,selectedClass='car', clusterId='ALL', AllTrajectoriesOnOneGraph = TRUE, annotation=FALSE){
  if(annotation) stop("Not implemented") # TODO

  # Selection du cluster à afficher
  clusterIdList <- unique(clusters$clusterId)
  #if (clusterId=='ALL') clusterIdList <- unique(clusters$clusterId)
  #else clusterIdList <- clusterId
  
  if(AllTrajectoriesOnOneGraph) drawEmptyPlot(LocationId, "Clusters")
  
  for (cId in clusterIdList){
    idList = unlist(clusters[locationId==LocationId & class==selectedClass & clusterId==cId, trackId])
    if(!AllTrajectoriesOnOneGraph) drawEmptyPlot(LocationId, paste("Cluster", cId))
    color=clusterMeta[clusterId==cId, color]
    for (id in unlist(idList)) {
      lines(unlist(trajectoriesDataset[trajectoriesDataset$trackId == id, "xCenter"]),
            unlist(trajectoriesDataset[trajectoriesDataset$trackId == id, "yCenter"]),
            col = color)
    }
  }
}

#==========================================
# Graphe en camembert
#==========================================
drawPieChart <- function(LocId){
  recordingList = recordingMeta[locationId==LocId, recordingId]
  totalTime = unlist(sum(recordingMeta[locationId==LocId,'duration']))/60
  car        = paste(round((n_distinct(trajectoriesDataset[recordingId %in% recordingList & class %in% c('car','truck_bus'), 'trackId'])/ totalTime), 0), 'usagers/heure')
  pedestrian = paste(round((n_distinct(trajectoriesDataset[recordingId %in% recordingList & class == 'pedestrian'          , 'trackId'])/ totalTime), 0), 'usagers/heure')
  bicycle    = paste(round((n_distinct(trajectoriesDataset[recordingId %in% recordingList & class == 'bicycle'             , 'trackId'])/ totalTime), 0), 'usagers/heure')
  
  pie(c(n_distinct(trajectoriesDataset[recordingId %in% recordingList & class %in% c('car','truck_bus'), 'trackId']),
        n_distinct(trajectoriesDataset[recordingId %in% recordingList & class == 'pedestrian'          , 'trackId']),
        n_distinct(trajectoriesDataset[recordingId %in% recordingList & class == 'bicycle'             , 'trackId'])), 
      labels = c(paste("Véhicules :", car), 
                 paste("Piétons :"  , pedestrian),
                 paste("Cyclistes :", bicycle)), 
      col=c("#CA0020", "#4DAC26", "#0571B0"))
}


#==========================================
# Tracé de la circulation sur la chaussée
#==========================================
convertColor <- function(c, class){
  if (class=='pedestrian'){ # A pedestrian should not be on road
    if(c) 'red'
    else 'green'
  } else {
    if(c) 'green'
    else 'red'
  } 
}

drawOnRoad <- function(trajectoriesDataset, studiedClass, LocId){
  drawEmptyPlot(LocId, "Traversées de chausée")
  for (tId in unique(trajectoriesDataset[locationId==LocId & class==studiedClass, trackId])){
    #print(tId)
    col <- sapply(trajectoriesDataset[trackId==tId,isOnRoad], function(x) convertColor(x, studiedClass))
    points(trajectoriesDataset[trackId == tId,.(xCenter,yCenter)], col=col, cex=0.5, pch=19)
  }
  legend(1,1, legend=c('Sur la chaussée','Sur le troitoir'), 
         col=c(convertColor(TRUE),convertColor(FALSE)), lty=1, cex=0.8, lwd=2)
}

#==========================================
# Calculer l'angle de direction d'un objet en fonction de sa vitesse x,y
#==========================================
getHeading <- function(xVelocity,yVelocity){
  if(xVelocity<0 & yVelocity<0){
    ((atan(yVelocity/xVelocity) * (180/pi))+180)%%360
  } else {
    (atan(yVelocity/xVelocity) * (180/pi))%%360
  }
}


