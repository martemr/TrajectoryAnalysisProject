#par(mfrow=c(2,3))


calculateGaps <- function(cl='car'){
  gaps <- data.table()
  # On collecte 10 échantillon par classe
  for (cnt in seq(1,100)){
    # Select random trackId
    tId= sample(unique(trajectoriesDataset[class==cl & locationId==1,trackId]), size=1)
    
    # Compute kalman filter at 3 seconds
    trainingSize=20
    timeToPredict=3
    prediction <- predictKalman(timeToPredict,tId, trainingSize=trainingSize)
    #predictKalmanPlot(timeToPredict,tId, trainingSize=trainingSize)
    
    # Determine the distance between truth and prediction
    res <- unlist(tracks[trackId==tId & trackLifetime==(trainingSize+timeToPredict*25), .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]) - unlist(prediction[1])[1:6]
    res <-res/unlist(tracks[trackId==tId & trackLifetime==(trainingSize+timeToPredict*25), .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
    
    gaps <- cbind(gaps, c(tId,res))
  }
  rownames(gaps) <- c('trackId', 'xCenter','yCenter','xVelocity','yVelocity','xAcceleration','yAcceleration')
  colnames(gaps) <- paste(seq(1,100))
  round(abs(gaps),2)
  gaps
}

plotGapPrediction <- function(timeToPredict=3,tId, trainingSize=20){
  # Plot prediction
  predictKalmanPlot(timeToPredict,tId, trainingSize=trainingSize)
  # Plot good points
  for (i in seq(0,3)) points(tracks[trackId==tId & trackLifetime==trainingSize+(i*25), .(xCenter,yCenter)], col='orange', pch=19)
}
  
gCar <- calculateGaps("car")
gPed <- calculateGaps("pedestrian")
gBus <- calculateGaps("truck_bus")
gBicy <- calculateGaps("bicycle")
    
gCar <- round(abs(gCar),2)
gPed <- round(abs(gPed),2)
gBus <- round(abs(gBus),2)
gBicy <- round(abs(gBicy),2)



par(mfrow=c(3,2))
for(i in seq(2,7)){
  plot(table(unlist(gCar[i,])), 
       main=i)
}
