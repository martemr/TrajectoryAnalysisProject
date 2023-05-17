##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : April 2023, 27th
# Description : Implementation d'un filtre de kalman basique
##---------------------------------------------

library(matlib)

#==========================================
# Filtre de kalman
# y est une liste de vecteurs
#==========================================
kalmanFilter <- function(x0,P0matrix,y,Fmatrix,Qmatrix, Rmatrix, Hmatrix){

  # nombre de mesures
  n = length(y)
  m=nrow(y)

  # variables initiales
  x_k_k <- as.vector(x0)
  P_k_k <- P0matrix 
  I <- diag(n)
  
  P_values <- list(P0matrix)
  #x_values <- list(x0)
  x_values   <- data.table(x=x0[1], 
                           y=x0[2],
                           xVelocity=0,
                           yVelocity=0,
                           xAcceleration=0,
                           yAcceleration=0)
  #xkk_values   <- data.table(x=x0[1], y=x0[2])
  #xk1k_values  <- data.table(x=x0[1], y=x0[2])
  #xk1k1_values <- data.table(x=x0[1], y=x0[2])
  
  # CACUL DU FILTRE
  for (i in seq(1,m)){
    # Prediction
    x_k1_k <- as.vector(Fmatrix %*% x_k_k)
    P_k1_k <- Fmatrix %*% P_k_k %*% t(Fmatrix) + Qmatrix 

    # Correction
    y_k1 <- unlist(y[i])
    K_k1 <- (P_k1_k %*% Hmatrix) %*% inv(Hmatrix %*% P_k1_k %*% Hmatrix + Rmatrix) # Ici H n'est pas transposé car R comprend seul dans quel sens le lire (vecteur colonne ou ligne)
    print(K_k1)
    x_k1_k1 <- as.vector(x_k1_k + K_k1 %*% (y_k1 - as.vector(Hmatrix %*% x_k1_k)))
    P_k1_k1 <- (I - as.numeric(K_k1 %*% Hmatrix)) %*% P_k1_k

    # Stockage des résultats
    P_values <- append(P_values, list(P_k1_k1))
    # x_values <- append(x_values, list(x_k1_k1))
    x_values <- rbind(x_values, list(x=unlist(x_k1_k1)[1], 
                                     y=unlist(x_k1_k1)[2], 
                                     xVelocity=unlist(x_k1_k1)[3], 
                                     yVelocity=unlist(x_k1_k1)[4],
                                     xAcceleration=unlist(x_k1_k1)[5], 
                                     yAcceleration=unlist(x_k1_k1)[6]))
    
    #xkk_values   <- rbind(xkk_values  , list(x=unlist(x_k_k  )[1], y=unlist(x_k_k  )[2]))
    #xk1k_values  <- rbind(xk1k_values , list(x=unlist(x_k1_k )[1], y=unlist(x_k1_k )[2]))
    #xk1k1_values <- rbind(xk1k1_values, list(x=unlist(x_k1_k1)[1], y=unlist(x_k1_k1)[2]))

    # Boucle
    P_k_k <- P_k1_k1
    x_k_k <- x_k1_k1
  }

  #xkk_values   <<- xkk_values    
  #xk1k_values  <<- xk1k_values   
  #xk1k1_values <<- xk1k1_values  
  
  list(P_values, x_values)
}


drawSmoothedTrajectory <- function(resultKalmanValue, uncertaincy=T){
  P_values <- unlist(resultKalmanValue[1],recursive = F)
  x_values <- data.frame(resultKalmanValue[2])
  
  drawEmptyPlot(paste("Smoothed trajectory"))
  points(x_values[,1:2], col='blue', pch=19)
  
}