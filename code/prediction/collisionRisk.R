library(sf)

#==========================================
#  
#==========================================
getPolygonPrediction <- function(studiedTrackId, time=5, trainingSize=10, plot=FALSE, legend=FALSE){
  # Prédiction de ses positions future avec incertitudes
  predictions <- predictKalmanPosition(studiedTrackId,timeToPredict=time,trainingSize = trainingSize)
  
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
}
  
#==========================================
# 
#==========================================
getCollisionRisk <- function(trackId1, trackId2, timeToPredict=3, trainingSize=10, toPlot=FALSE){
  if(toPlot) {
    drawTrajectory(4,trackId1,dosinit,col='blue',newPlot=TRUE)
    drawTrajectory(4,trackId2,dosinit,col='blue',newPlot=FALSE)
  }
  # Get the area of predictions
  poly1 <- getPolygonPrediction(trackId1,time=timeToPredict,trainingSize=trainingSize, plot=toPlot)
  poly2 <- getPolygonPrediction(trackId2,time=timeToPredict,trainingSize=trainingSize, plot=toPlot)
  
  # Get the common area
  common_area <- st_intersection(poly1, poly2)
  if(toPlot & length(common_area)>0)lines((st_coordinates(common_area))[,1:2], col='red', lwd=2)
  
  st_area(common_area)/(st_area(poly1)+st_area(poly2))*100
}

drawEmptyPlot("")
par(mfrow=c(2,2))
for(i in seq(1,8)*10){
  print(getCollisionRisk(24,25,timeToPredict=5,trainingSize=i, toPlot=TRUE))

}

#getCollisionRisk(24,25,plot=TRUE)

