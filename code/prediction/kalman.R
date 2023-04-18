library(data.table)
library(matlib)

defineKalman <- function(trackIdToPredict=8, sampleSize=10){
  # Récupération des données
  data = tracks[trackId==trackIdToPredict, .(frame,xCenter,xVelocity,yCenter,yVelocity, xAcceleration, yAcceleration)]
  trackClass = unique(tracksMeta[trackId==trackIdToPredict,class])

  # DEFINITION DES PARAMETRES
  # Taille de l'état
  n=6 
  # Taille de la mesure
  m=n
  
  # Matrice identité
  I=diag(n)
  # Intervalle de temps
  dt=0.04
  
  # Matrice de transition
  f= rbind(c(1, 0,dt, 0,1/2*dt^2,       0),
           c(0, 1, 0,dt,       0,1/2*dt^2), 
           c(0, 0, 1, 0,      dt,       0),
           c(0, 0, 0, 1,       0,      dt),
           c(0, 0, 0, 0,       1,       0),
           c(0, 0, 0, 0,       0,       1)
  )
  
  # DEFINITIONS MATRICES
  # Récupération des paramètres et définition de la covariance initiale
  P_k_k=rbind(c(getVariancePercentile(trackClass,'xPosition',0.75,1),   0,    0,    0,    0,    0),
              c(   0,getVariancePercentile(trackClass,'yPosition',0.75,1),    0,    0,    0,    0),
              c(   0,   0,getVariancePercentile(trackClass,'xSpeed',0.75,2),    0,    0,    0),
              c(   0,   0,    0,getVariancePercentile(trackClass,'ySpeed',0.75,2),    0,    0), 
              c(   0,   0,    0,    0,getVariancePercentile(trackClass,'xAcc',0.75,3),    0),
              c(   0,   0,    0,    0,    0,getVariancePercentile(trackClass,'yAcc',0.75,3))
  )
  # Bruit de modèle -> Faible car modèle fiable
  Q = I*5*10^-3
  # Bruit de mesure -> Faible car mesure fiable
  R = I*10^-6
  
  
  # Modélisation du processus de mesure (ici il ne modifie rien)
  H=diag(n)
  
  # Vecteur initial
  x_k_k = unlist(data[1, .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
  
  pred <- data.table(x     =x_k_k[1], 
                     y     =x_k_k[2], 
                     xSpeed=x_k_k[3], 
                     ySpeed=x_k_k[4], 
                     xAcc  =x_k_k[5],
                     yAcc  =x_k_k[6])
  
  # CACUL DU FILTRE
  for (i in seq(2,sampleSize)){
    # Prediction
    x_k1_k <- f %*% x_k_k
    P_k1_k <- f %*% P_k_k %*% t(f) + Q 
    pred <- rbind(pred, data.table(x     =x_k1_k[1], 
                                   y     =x_k1_k[2], 
                                   xSpeed=x_k1_k[3], 
                                   ySpeed=x_k1_k[4], 
                                   xAcc  =x_k1_k[5],
                                   yAcc  =x_k1_k[6]))
    
    # Correction
    y_k1 <- unlist(data[i,  .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]) # mesure
    K_k1 <- P_k1_k %*% t(H) %*% inv(H %*% P_k1_k %*% t(H))
    x_k1_k1 <- x_k1_k + K_k1 %*% (y_k1 - H %*% x_k1_k)
    P_k1_k1 <- (I - K_k1 %*% H) %*% P_k1_k
    
    
    # Boucle
    P_k_k <- P_k1_k1
    x_k_k <- x_k1_k1
  }
  
  
  list("LastValue"=as.matrix(x_k_k), "LastCovariance"=as.matrix(P_k_k), "TransitionMatrix"=f, "ModelNoise"=Q)
}

predictKalman <- function(timeToPredict=3, tId, trainingSize=10, uncertainty=TRUE){
  resultKalman <- defineKalman(tId,trainingSize)
  
  predictedValue <- resultKalman$LastValue
  predictedValueCovariance <- resultKalman$LastCovariance
  
  for (ind in seq(1,timeToPredict)){
    for (r in seq(1,25)){
      predictedValue <- resultKalman$TransitionMatrix %*% predictedValue
      predictedValueCovariance <- resultKalman$TransitionMatrix %*% predictedValueCovariance %*% t(resultKalman$TransitionMatrix) + resultKalman$ModelNoise
    }
  }
  
  rownames(predictedValue) <- c('xCenter','yCenter','xVelocity','yVelocity','xAcceleration','yAcceleration')
  colnames(predictedValue) <- paste('Time = ', timeToPredict,'s')
  list(predictedValue, predictedValueCovariance)
}
  

# timeToPredict is in s
predictKalmanPlot <- function(timeToPredict=3, tId, trainingSize=10, uncertainty=TRUE, newPlot=TRUE){
  resultKalman <- defineKalman(tId,trainingSize)
  
  predictedValue <- resultKalman$LastValue
  predictedValueCovariance <- resultKalman$LastCovariance

  if(newPlot)  drawEmptyPlot("Predicted trajectory") 
  points(tracks[trackId==tId, .(xCenter,yCenter)], col='blue')
  points(tracks[trackId==tId & trackLifetime<trainingSize, .(xCenter,yCenter)], col='red')  
  for (ind in seq(1,timeToPredict)){
    lastPrediction <- predictedValue
    for (r in seq(1,25)){
      predictedValue <- resultKalman$TransitionMatrix %*% predictedValue
      predictedValueCovariance <- resultKalman$TransitionMatrix %*% predictedValueCovariance %*% t(resultKalman$TransitionMatrix) + resultKalman$ModelNoise
    }

    if(uncertainty){
      stop("Not implemented") # TODO
      #print(paste(c(unlist(predictedValue[1]-predictedValueCovariance[1,1]), unlist(predictedValue[2])), c(unlist(predictedValue[1]+predictedValueCovariance[1,1]), unlist(predictedValue[2])) ))
      # rect(xleft = unlist(predictedValue[1]-predictedValueCovariance[1,1]), 
      #      xright  = unlist(predictedValue[1]+predictedValueCovariance[1,1]), 
      #      ytop  = unlist(predictedValue[2]+predictedValueCovariance[2,2]), 
      #      ybottom = unlist(predictedValue[2]-predictedValueCovariance[2,2]),
      #      col=green)
    }
    
  }
  legend(1,1,legend=c("Original trajectory", "Trained part", "Predicted"), 
         col=c('blue', 'red', 'green'), lty=1, lwd=3)
}

predictKalmanPosition <- function(tId, timeToPredict=3, trainingSize=10, uncertainty=TRUE){
  resultKalman <- defineKalman(tId,trainingSize)
  
  predictedValue <- resultKalman$LastValue
  predictedValueCovariance <- resultKalman$LastCovariance
  
  result = data.table(time=0,x=predictedValue[1], y=predictedValue[2],
                      dx=0,dy=0, heading=getHeading(predictedValue[3], predictedValue[4]))
  for (ind in seq(1,timeToPredict)){
    for (r in seq(1,25)){
      predictedValue <- resultKalman$TransitionMatrix %*% predictedValue
      predictedValueCovariance <- resultKalman$TransitionMatrix %*% predictedValueCovariance %*% t(resultKalman$TransitionMatrix) + resultKalman$ModelNoise
    }
    result <- rbind(result,list(ind,predictedValue[1],predictedValue[2],
                                predictedValueCovariance[1,1], predictedValueCovariance[2,2],
                    getHeading(predictedValue[3], predictedValue[4])))
  }
  
  result
}
