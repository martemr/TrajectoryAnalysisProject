##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 07th
# Description : Fonctions pour le tracé de courbes
##---------------------------------------------

#==========================================
# Initialisation des paramêtres
#==========================================
initPlotImage <- function(LocationId, dosinit){
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
drawEmptyPlot <- function(PlotName,Background=TRUE){
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
# Tracé des trajectoires
#==========================================
drawTrajectories <- function(AllTrajectoriesOnOneGraph = TRUE, StudiedClass='ALL'){
  if(AllTrajectoriesOnOneGraph){
    drawEmptyPlot("All trajectories")
  }
  if (StudiedClass=='ALL') classList <- unique(trajectoriesDataset$class)
  else classList <- StudiedClass
  for (cl in classList)
  {
    if (!AllTrajectoriesOnOneGraph) drawEmptyPlot(paste("Trajectories of", cl))
    for (tId in unique(trajectoriesDataset[class == cl, trackId])) {
      lines(unlist(trajectoriesDataset[trackId == tId, xCenter]),
            unlist(trajectoriesDataset[trackId == tId, yCenter]),
            col = (switch(cl, 
                          'car'='red', 
                          'truck_bus'='yellow',
                          'pedestrian'='blue',
                          'bicycle'='green')))
    }
  }
}

#==========================================
# Tracé des clusters
#==========================================
drawClusters <- function(clusters, clusterMeta, clusterId='ALL', AllTrajectoriesOnOneGraph = TRUE, annotation=FALSE){
  if(annotation) stop("Not implemented")
  
  # Selection du cluster à afficher
  if (clusterId=='ALL') clusterIdList <- unique(clusters$clusterId)
  else clusterIdList <- clusterId
  
  if(AllTrajectoriesOnOneGraph) drawEmptyPlot("Clusters")
  
  for (cId in clusterIdList){
    idList = unlist(clusters[clusterId==cId,'trackId'])
    if(!AllTrajectoriesOnOneGraph) drawEmptyPlot(paste("Cluster", cId))
    color=clusterMeta[clusterId==cId, color]
    for (id in unlist(idList)) {
      lines(unlist(trajectoriesDataset[trajectoriesDataset$trackId == id, "xCenter"]),
            unlist(trajectoriesDataset[trajectoriesDataset$trackId == id, "yCenter"]),
            col = color)
    }
  }
}
