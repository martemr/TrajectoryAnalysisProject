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
predictKalmanPosition <- function(studiedTrackId, startFrame,y){
  # On définit les variables du filtre de kalamn
  dt=5/recordingMeta[recordingId==unique(trajectoriesDataset[trackId==studiedTrackId, recordingId]), frameRate] # Intervalle de temps
  
  # x : Vecteur d'état
  data <- trajectoriesDataset[trackId==studiedTrackId & frame>startFrame, 
                              .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]
  x0 = matrix(unlist(data[1]), nrow=1)
  
  # P : Matrice de covariance
  # P est l'estimation de l'erreur de prédiction du modèle. On s'inspire de l'erreur réelle dans nos mesures pour le calculer.
  errors <- fread(sprintf("%serrorsPositionKalman.csv",dosinit), header = TRUE, sep = ',')
  err=unlist(errors[unique(trajectoriesDataset[trackId==studiedTrackId,locationId]),])[switch(unique(trajectoriesDataset[trackId==studiedTrackId,class]),                                                                                             'car'=1,'truck_bus'=2,'pedestrian'=3,'bicycle'=4)]
  err=1
  P0matrix = as.matrix(rbind(c(err,  0,      0,      0,             0,              0),
                             c(0,  err,      0,      0,             0,              0), 
                             c(0,    0, err*dt,      0,             0,              0),
                             c(0,    0,      0, err*dt,             0,              0),
                             c(0,    0,      0,      0,((err*dt)^2)/2,              0),
                             c(0,    0,      0,      0,             0,((err*dt)^2)/2)))
  
  
  # F : Matrice de transition
  Fmatrix= as.matrix(rbind(c(1, 0,dt, 0,1/2*dt^2,       0),
                           c(0, 1, 0,dt,       0,1/2*dt^2), 
                           c(0, 0, 1, 0,      dt,       0),
                           c(0, 0, 0, 1,       0,      dt),
                           c(0, 0, 0, 0,       1,       0),
                           c(0, 0, 0, 0,       0,       1)))
  
  # Q : Bruit de modèle
  # On considère que notre modèle est fiable donc faible bruit 
  Qmatrix= as.matrix(rbind(c(10^(-3),  0,          0,          0,                 0,                 0),
                           c(0,  10^(-3),          0,          0,                 0,                 0), 
                           c(0,        0, 10^(-3)*dt,          0,                 0,                 0),
                           c(0,        0,          0, 10^(-3)*dt,                 0,                 0),
                           c(0,        0,          0,          0,((10^(-3)*dt)^2)/2,                 0),
                           c(0,        0,          0,          0,                 0,((10^(-3)*dt)^2)/2)))
  
  # R : Bruit de mesure
  # On le considère égal à Q
  Rmatrix = Qmatrix
  
  # H : Processus de mesure
  Hmatrix = as.matrix(rbind(c(1, 0, 0, 0, 0, 0),
                            c(0, 1, 0, 0, 0, 0), 
                            c(0, 0, 0, 0, 0, 0),
                            c(0, 0, 0, 0, 0, 0),
                            c(0, 0, 0, 0, 0, 0),
                            c(0, 0, 0, 0, 0, 0)))

  
  # Appel au filtre de kalman  
  kalmanFilter(x0,P0matrix,y,Fmatrix,Qmatrix, Rmatrix, Hmatrix)
}
