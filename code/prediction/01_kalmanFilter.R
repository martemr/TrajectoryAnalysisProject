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
# Avec : 
#  - Ny the number of variables 
#  - Nb the number of unobserved variables in the state equation
#  - T  the number of time periods
# x0 TAILLE:[Nb*1] for the initial guess of the unobserved components 
# P0 TAILLE:[Nb*Nb] for the initial guess of the unobserved components covariance matrix, 
# yt TAILLE:[Ny*T]  est une liste de vecteurs 
# Dm TAILLE:[Nb*1]  for the constant in the state equation, 
# Am TAILLE:[Ny*1]  for the constant in the observation equation, 
# Fm TAILLE:[Nb*Nb] for the state equation transition matrix, 
# Hm TAILLE:[Ny*Nb] for the observation matrix in the observation equation, 
# Qm TAILLE:[Nb*Nb] for the covariance matrix of the errors in the state equation, 
# Rm TAILLE:[Ny*Ny]  for the covariance matrix of the errors in the observation equation,
#==========================================

x0 = x0
P0 = P0
yt = yt
Fm = Fmatrix
Hm = Hmatrix
Qm = Qmatrix
Rm = Rmatrix

kalmanFilter <- function(x0,P0,yt,Fm,Qm,Rm,Hm){

  # Nombre de mesures
  Tmps = ncol(yt)
  Ny = nrow(yt)
  Nb = nrow(x0)
  
  # Verification des dimensions
  stopifnot('Dimensions de P0 incorrectes' = dim(P0)==c(Nb,Nb))
  stopifnot('Dimensions de Fm incorrectes' = dim(Fm)==c(Nb,Nb))
  stopifnot('Dimensions de Qm incorrectes' = dim(Qm)==c(Nb,Nb))
  stopifnot('Dimensions de Rm incorrectes' = dim(Rm)==c(Ny,Ny))
  stopifnot('Dimensions de Hm incorrectes' = dim(Hm)==c(Ny,Nb))
  
  # variables initiales
  x_k_k <- x0
  P_k_k <- P0
  I <- diag(Nb) 
  
  # Préparation des variables de memoire
  P_t1 <- list(P0)
  x_t1 <- list(x0)
  P_tt <- list()
  x_tt <- list()
  
  # CACUL DU FILTRE
  for (i in seq(1,Tmps)){
    # Prediction
    x_k1_k <- Fm %*% x_k_k
    P_k1_k <- Fm %*% P_k_k %*% t(Fm) + Qm
    # Mise en mémoire Prediction
    x_t1 <- append(x_t1, list(x_k1_k))
    P_t1 <- append(P_t1, list(P_k1_k))
    
    # Correction
    y_k1 <- yt[,i]
    K <- (P_k1_k %*% t(Hm)) %*% inv(Hm %*% P_k1_k %*% t(Hm) + Rmatrix) 
    x_k1_k1 <- as.vector(x_k1_k + K %*% (y_k1 - as.vector(Hmatrix %*% x_k1_k)))
    P_k1_k1 <- (I - as.numeric(K %*% Hmatrix)) %*% P_k1_k
    # Mise en mémoire Correction
    x_tt <- append(x_tt, list(x_k1_k1))
    P_tt <- append(P_tt, list(P_k1_k1))

    # Boucle
    P_k_k <- P_k1_k1
    x_k_k <- x_k1_k1
  }

  # Renvoie une liste avec les valeurs de positions et variance/covariance, avant et apres correction
  list(x_t1, P_t1, x_tt, P_tt, K)
}


drawSmoothedTrajectory <- function(resultKalmanValue, uncertaincy=T){
  P_values <- unlist(resultKalmanValue[1],recursive = F)
  x_values <- data.frame(resultKalmanValue[2])
  
  drawEmptyPlot(paste("Smoothed trajectory"))
  points(x_values[,1:2], col='blue', pch=19)
  
}