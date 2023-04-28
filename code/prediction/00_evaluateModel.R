##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : April 2023, 27th
# Description : Evaluation du modèle dans le cas réel pour déterminer le bruit de modèle
##---------------------------------------------

#==========================================
# Calcule les écarts entre le positions réelles et les valeurs calculés par le modèle
#==========================================
calculateGaps <- function(cl='car', LocationId){
  gaps <- matrix(ncol=2)
  # On collecte 10 échantillon par classe
  dt = 1/unique(recordingMeta[locationId==LocationId,frameRate])
  for (cnt in seq(1,1000)){
    # Select random trackId
    tId= sample(unique(trajectoriesDataset[class==cl & locationId==LocationId,trackId]), size=1)
    
    # On prend une frame random dans cette track
    f = sample(rev(trajectoriesDataset[locationId==LocationId & trackId==tId,frame])[-1], size=1)
    data1 = unlist(trajectoriesDataset[locationId==LocationId & trackId==tId & frame==f  ,.(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
    data2 = unlist(trajectoriesDataset[locationId==LocationId & trackId==tId & frame==f+5,.(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
    if(length(data2)==0) next
    gaps <- rbind(gaps,data2[1:2] - 
                    c(data1[1]+data1[3]*dt+data1[5]*(dt^2)/2,
                         data1[2]+data1[4]*dt+data1[6]*(dt^2)/2))
  }
  gaps
}

#==========================================
# Stocke les max de la densité de fonction des écarts
#==========================================
computeErrorsPosition <- function(toPlot=FALSE){
  # Distribution de l'erreur par localisation
  par(mfrow=c(2,2))
  errors <- list()
  for (i in seq(1,4)){
    for (c in unique(trajectoriesDataset$class)){
      print(paste("Location",i,"classe",c))
      gaps <- calculateGaps(c,i)
      if(toPlot){
      plot(density(unlist(abs(gaps[,1])),na.rm=T),lwd=5, col = (switch(c, 'car'='red',
                                                                   'truck_bus'='yellow',
                                                                   'pedestrian'='blue',
                                                                   'bicycle'='green')), main=paste("Error density on",c))
      }
      dens <- density(unlist(abs(gaps[,1])),na.rm=T)
      print(dens$y[which.max(dens$y)])
      errors <- append(errors,dens$y[which.max(dens$y)])
      }
  }
  
  # On défini une base de données de l'erreur 
  errorPosition <- matrix(unlist(errors),ncol=4,nrow=4)
  colnames(errorPosition) <- c(unique(trajectoriesDataset$class))
  rownames(errorPosition) <- c(1,2,3,4)
  
  # On prend le max de densité de chacun des couples localisation/class
  fwrite(data.table(errorPosition),file=paste(dosinit, "errorsPositionKalman.csv", sep=""))

}
