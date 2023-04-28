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
  data <- trajectoriesDataset[trackId==studiedTrackId & frame==startFrame, 
                              .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]
  x0 = matrix(unlist(data), nrow=1)
  
  # P : Matrice de covariance
  P0matrix=as.matrix(rbind(c(getVariancePercentile(trackClass,'xPosition',0.75,1),   0,0,    0,  0,   0),
                        c(   0,getVariancePercentile(trackClass,'yPosition',0.75,1),0,    0,  0,   0),
                        c(   0,   0,getVariancePercentile(trackClass,'xSpeed',0.75,2),    0,  0,   0),
                        c(   0,   0,    0,getVariancePercentile(trackClass,'ySpeed',0.75,2),  0,   0), 
                        c(   0,   0,    0,    0,getVariancePercentile(trackClass,'xAcc',0.75,3),   0),
                        c(   0,   0,    0,    0,    0,getVariancePercentile(trackClass,'yAcc',0.75,3)))
  )
  
  # F : Matrice de transition
  Fmatrix= as.matrix(rbind(c(1, 0,dt, 0,1/2*dt^2,       0),
                           c(0, 1, 0,dt,       0,1/2*dt^2), 
                           c(0, 0, 1, 0,      dt,       0),
                           c(0, 0, 0, 1,       0,      dt),
                           c(0, 0, 0, 0,       1,       0),
                           c(0, 0, 0, 0,       0,       1)))
  
  # Q : Bruit de modèle
  # On s'inspire du vrai bruit : Calculé précédemment
  errors <- fread(sprintf("%serrorsPositionKalman.csv",dosinit), header = TRUE, sep = ',')
  err=unlist(errors[unique(trajectoriesDataset[trackId==studiedTrackId,locationId]),])[switch(unique(trajectoriesDataset[trackId==studiedTrackId,class]), 
                            'car'=1,'truck_bus'=2,'pedestrian'=3,'bicycle'=4)]
  Qmatrix= as.matrix(rbind(c(err,  0,      0,      0,             0,              0),
                           c(0,  err,      0,      0,             0,              0), 
                           c(0,    0, err*dt,      0,             0,              0),
                           c(0,    0,      0, err*dt,             0,              0),
                           c(0,    0,      0,      0,((err*dt)^2)/2,              0),
                           c(0,    0,      0,      0,             0,((err*dt)^2)/2)))
  
  # R : Bruit de mesure
  # On le considère égal à Q
  Rmatrix = Qmatrix
  
  # H : Processus de mesure
  Hmatrix = matrix(c(1,1,0,0,0,0), nrow=1)

  
  # Appel au filtre de kalman  
  kalmanFilter(x0,P0matrix,y,Fmatrix,Qmatrix, Rmatrix, Hmatrix)
}
