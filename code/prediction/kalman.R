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
  Q = I*10^-6
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

# time is in s
predictKalmanPlot <- function(time, tId, trainingSize){
  resultKalman <- defineKalman(tId,trainingSize)
  
  predictedValue <- resultKalman$LastValue
  predictedValueCovariance <- resultKalman$LastCovariance

  drawEmptyPlot("Predicted trajectory") 
  points(tracks[trackId==tId, .(xCenter,yCenter)], col='blue')
  points(tracks[trackId==tId & trackLifetime<trainingSize, .(xCenter,yCenter)], col='red')  
  for (ind in seq(1,time)){
    for (r in seq(1,25)){
      predictedValue <- resultKalman$TransitionMatrix %*% predictedValue
      predictedValueCovariance <- resultKalman$TransitionMatrix %*% predictedValueCovariance %*% t(resultKalman$TransitionMatrix) + resultKalman$ModelNoise
    }

    points(unlist(predictedValue[1]), unlist(predictedValue[2]),pch=19, col='green')
    # rect(xleft = unlist(test[1]-t2[1,1]), xright = unlist(test[1]+t2[1,1]), 
    #      ytop = unlist(test[2]+t2[2,2]),ybottom = unlist(test[2]-t2[2,2]),col=NA)
    # 
  }
  legend(1,1,legend=c("Original trajectory", "Trained part", "Predicted"), 
         col=c('black', 'red', 'green'), lty=1)
}