#par(mfrow=c(2,3))

gaps <- data.table()


# On collecte 10 échantillon par classe
for (cl in unique(trajectoriesDataset$class)){
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
    # Plot good points
    #for (i in seq(0,3)) points(tracks[trackId==tId & trackLifetime==trainingSize+(i*25), .(xCenter,yCenter)], col='orange', pch=19)
    gaps <- cbind(gaps, c(tId,res))
  }
  rownames(gaps) <- c('trackId', 'xCenter','yCenter','xVelocity','yVelocity','xAcceleration','yAcceleration')
  
  break
}


round(abs(gaps),2)
par(mfrow=c(4,2))
for(i in seq(1,8)){
  hist(seq(1,6), unlist(
    round(abs(gaps[2:7,..i]),2)), 
    xlab="", main =paste(i))
}

     