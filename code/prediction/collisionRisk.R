##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : Etablir un risque de colision entre 2 trajectoires
# Dépendances : kalman.R
# ___________DEPRECATED___________
##---------------------------------------------


library(sf)

#==========================================
#  Calcule le cône de prédiction de la position
#==========================================
getPolygonPrediction <- function(studiedTrackId, time=3, trainingSize=10, startFrameTraining=0, plot=FALSE, legend=FALSE){
  #for(time in seq(1,5)){
    #drawEmptyPlot("")
    # Prédiction de ses positions future avec incertitudes
    predictions <- predictKalmanPosition(studiedTrackId,
                                         timeToPredict=time,
                                         startFrameTraining=startFrameTraining,
                                         trainingSize = trainingSize)
    
    # Creation du cône de prédiction
    conesPoints <- data.table(x=unlist(predictions[,2])+unlist(predictions[,4])*cos((unlist(predictions[,6])+90)*pi/180),
               y=unlist(predictions[,3])+unlist(predictions[,5])*sin((unlist(predictions[,6])+90)*pi/180))
    conesPoints <- rbind(conesPoints, apply(data.table(
      (predictions[,2])+(predictions[,4])*cos(((predictions[,6])-90)*pi/180),
      (predictions[,3])+(predictions[,5])*sin(((predictions[,6])-90)*pi/180)),2,rev))
    countourPoints <- as.matrix(conesPoints)
    
    if(plot){
      points(predictions[,2:3], col='green', pch=19)
      lines(countourPoints,col='orange',lwd=2)
      if(legend) stop('not implemeted') # TODO
    }
  
    st_polygon(list(countourPoints))
  #}
  
}
  
#==========================================
# Calcule le chevauchement entre les deux cônes
#==========================================
getCollisionRisk <- function(trackId1, trackId2, timeToPredict=3, startFrameTraining=0, trainingSize=10, toPlot=FALSE){
  if(toPlot) {
    drawTrajectory(4,trackId1,dosinit,col='blue',newPlot=TRUE)
    drawTrajectory(4,trackId2,dosinit,col='blue',newPlot=FALSE)
  }
  # Get the area of predictions
  poly1 <- getPolygonPrediction(trackId1,time=timeToPredict, startFrameTraining=startFrameTraining, trainingSize=trainingSize, plot=toPlot)
  poly2 <- getPolygonPrediction(trackId2,time=timeToPredict, startFrameTraining=startFrameTraining, trainingSize=trainingSize, plot=toPlot)
  
  # Get the common area
  common_area <- st_intersection(poly1, poly2)
  if(toPlot & length(common_area)>0)lines((st_coordinates(common_area))[,1:2], col='red', lwd=2)
  
  st_area(common_area)/(st_area(poly1)+st_area(poly2))*100
}

drawEmptyPlot("")
par(mfrow=c(1,1))
for(i in seq(1,8)*10){
  print(getCollisionRisk(24,25,timeToPredict=75,startFrameTraining=100000,trainingSize=i, toPlot=TRUE))
}


# points(trajectoriesDataset[trackId %in% c(24,25) & frame==1802,.(xCenter,yCenter)], pch=19, col='purple')
# 
# #getCollisionRisk(24,25,plot=TRUE)
getFirstCommonPoint <- function(trackId1, trackId2){
  count=table(trajectoriesDataset[trackId %in% c(trackId1, trackId2), frame])
  min(as.numeric(names(count[count==2])))
}
