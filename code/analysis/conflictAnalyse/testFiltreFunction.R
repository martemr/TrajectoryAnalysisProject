library(kalmanfilter)

studiedTrackId = 7
startFrame = 0

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



# Données d'observation
observations <- matrix(c(1, 2, 2, 4, 3, 6), ncol = 2)

# Initialisation des paramètres du filtre de Kalman
initial_state      <- x0       # État initial
initial_covariance <- P0matrix # Matrice de covariance initiale
transition_matrix  <- Fmatrix  # Matrice de transition
observation_matrix <- Hmatrix  # Matrice d'observation
process_noise      <- Qmatrix  # Bruit du processus
observation_noise  <- Rmatrix  # Bruit d'observation



# Data observations
yt <- t(as.matrix(trajectoriesDataset[trackId==7,.(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)][1:10,], ncol=6))

# Define the state space model
ssm <- list(
  B0 = matrix(0,6,1), # t(initial_state),
  P0 = initial_covariance,
  Dm = matrix(0,6,1),
  Am = matrix(0,2,1),
  Fm = transition_matrix,
  Hm = matrix(c(1,1,1,1,0,0,0,0,0,0,0,0),2,6),
  Qm = process_noise,
  Rm = observation_noise[1:2,1:2]
)

# 

# yt <- matrix(c(1, 2, 2, 4, 3, 6), ncol = 2)
# yt <- diag(6)
# Run the Kalman filter
kf_result <- kalman_filter(ssm, yt)


