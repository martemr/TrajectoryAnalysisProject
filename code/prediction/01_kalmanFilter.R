##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : April 2023, 27th
# Description : Implementation d'un filtre de kalman basique
##---------------------------------------------

#==========================================
# Filtre de kalman
# y est une liste de vecteurs
#==========================================
kalmanFilter <- function(x0,P0matrix,y,Fmatrix,Qmatrix, Rmatrix, Hmatrix){
  # nombre de mesures
  n = length(y)

  # variables initiales
  x_k_k <- x0
  P_k_k <- P0matrix 
  I <- diag(n)
  
  P_values <- list(P0matrix)
  x_values <- list(x0)
  
  # CACUL DU FILTRE
  for (i in seq(1,n)){
    # Prediction
    x_k1_k <- Fmatrix %*% x_k_k
    P_k1_k <- Fmatrix %*% P_k_k %*% t(Fmatrix) + Qmatrix 

    # Correction
    y_k1 <- matrix(unlist(y[i]), nrow=1)
      #unlist(data[i,  .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]) # mesure
    K_k1 <- P_k1_k %*% t(Hmatrix) %*% inv(Hmatrix %*% P_k1_k %*% t(Hmatrix) + Rmatrix)
    x_k1_k1 <- x_k1_k + K_k1 %*% (y_k1 - Hmatrix %*% x_k1_k)
    P_k1_k1 <- (I - K_k1 %*% Hmatrix) %*% P_k1_k

    # Récupération des valeurs
    P_values <- append(P_values, P_k1_k1)
    x_values <- append(x_values, x_k1_k1)
    
    # Boucle
    P_k_k <- P_k1_k1
    x_k_k <- x_k1_k1
  }
  
  list(P_values, x_values)
}