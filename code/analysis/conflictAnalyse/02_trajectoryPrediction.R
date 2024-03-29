##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : April 2023, 27th
# Description : ...
# Dépendances : 01_kalmanFilter.R
##---------------------------------------------

#==========================================
# Filtre de kalman
#==========================================
predictKalmanPosition <- function(studiedTrackId, startFrame,yt){
  # On définit les variables du filtre de kalamn
  dt=5/recordingMeta[recordingId==unique(trajectoriesDataset[trackId==studiedTrackId, recordingId]), frameRate] # Intervalle de temps
  
  # x [6*1] : Vecteur d'état
  data <- trajectoriesDataset[trackId==studiedTrackId & frame>startFrame, 
                              .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]
  x0 = matrix(unlist(data[1]), ncol=1)
  
  # P [6*6] : Matrice de covariance
  # P est l'estimation de l'erreur de prédiction du modèle. On s'inspire de l'erreur réelle dans nos mesures pour le calculer.
  errors <- fread(sprintf("%serrorsPositionKalman.csv",dosinit), header = TRUE, sep = ',')
  err=unlist(errors[unique(trajectoriesDataset[trackId==studiedTrackId,locationId]),])[switch(unique(trajectoriesDataset[trackId==studiedTrackId,class]),                                                                                             'car'=1,'truck_bus'=2,'pedestrian'=3,'bicycle'=4)]
  err=10
  print(err)
  P0matrix = as.matrix(rbind(c(err,  0,      0,      0,             0,              0),
                             c(0,  err,      0,      0,             0,              0), 
                             c(0,    0, err*dt,      0,             0,              0),
                             c(0,    0,      0, err*dt,             0,              0),
                             c(0,    0,      0,      0,((err*dt)^2)/2,              0),
                             c(0,    0,      0,      0,             0,((err*dt)^2)/2)))
  
  
  # F [6*6] : Matrice de transition
  Fmatrix= as.matrix(rbind(c(1, 0,dt, 0,1/2*dt^2,       0),
                           c(0, 1, 0,dt,       0,1/2*dt^2), 
                           c(0, 0, 1, 0,      dt,       0),
                           c(0, 0, 0, 1,       0,      dt),
                           c(0, 0, 0, 0,       1,       0),
                           c(0, 0, 0, 0,       0,       1)))
  
  # Q [6*6] : Bruit de modèle
  # On considère que notre modèle est fiable donc faible bruit 
  errQ = 1
  Qmatrix= as.matrix(rbind(c(errQ,    0,       0,       0,              0,                0),
                           c(0,    errQ,       0,       0,              0,                0), 
                           c(0,       0, errQ*dt,       0,              0,                0),
                           c(0,       0,       0, errQ*dt,              0,                0),
                           c(0,       0,       0,       0,((errQ*dt)^2)/2,                0),
                           c(0,       0,       0,       0,              0,((errQ*dt)^2)/2)))
  
  # R [2*2] : Bruit de mesure
  errR = 10
  Rmatrix = as.matrix(rbind(c(errR,       0),
                            c(0      , errR)))
  
  # H [2*6] : Processus de mesure
  Hmatrix = as.matrix(rbind(c(1, 0, 1, 0, 1, 0),
                            c(0, 1, 0, 1, 0, 1)))

  
  # yt [2*T] : Observations 
  yt = (trajectoriesDataset[trackId==7,.(xCenter,yCenter)])
  print(yt)
  
  # Appel au filtre de kalman  
  kalmanFilter(x0=x0,
               P0 = P0matrix,
               yt = yt,
               Fm = Fmatrix,
               Qm = Qmatrix, 
               Rm = Rmatrix, 
               Hm = Hmatrix)
}


getResultPoints <- function(res){
  resultsPoints <- t(as.data.table(unlist(res[2], recursive=F)))
  colnames(resultsPoints) <- c("xCenter","yCenter","xVelocity","yVelocity","xAcceleration","yAcceleration")
  resultsPoints
}

