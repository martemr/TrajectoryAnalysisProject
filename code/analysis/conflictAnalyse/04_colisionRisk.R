#---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : July 2023
# Description : 
##---------------------------------------------
library(MASS)
library(cgwtools)
library(spatialEco)

drawSituation <- function(Loc, trajectoireA, trajectoireB, f){
  #initPlotImage(Loc)
  drawEmptyPlot(Loc)
  lines(dataset[trackId==trajectoireA, .(xCenter, yCenter)], lwd=2, col='blue')
  lines(dataset[trackId==trajectoireB, .(xCenter, yCenter)], lwd=2, col='blue')
  
  points(dataset[trackId==trajectoireB & frame==f, .(xCenter, yCenter)], col='red', pch=19)
  points(dataset[trackId==trajectoireA & frame==f, .(xCenter, yCenter)], col='red', pch=19)
}

predictPosition <- function(vectorXY, probXY=diag(6), dt=0.04){
  # transitionMatrix = as.matrix(rbind(c(1, 0,dt, 0,1/2*dt^2,       0),
  #                                    c(0, 1, 0,dt,       0,1/2*dt^2), 
  #                                    c(0, 0, 1, 0,      dt,       0),
  #                                    c(0, 0, 0, 1,       0,      dt),
  #                                    c(0, 0, 0, 0,       1,       0),
  #                                    c(0, 0, 0, 0,       0,       1)))
  
  # On ne prends pas l'acceleration en compte
  transitionMatrix = matrix(c(c(1, 0,dt, 0, 0, 0),
                              c(0, 1, 0,dt, 0, 0), 
                              c(0, 0, 1, 0,dt, 0),
                              c(0, 0, 0, 1, 0,dt),
                              c(0, 0, 0, 0, 1, 0),
                              c(0, 0, 0, 0, 0, 1)), nrow=6)
  
  #print(as.vector(vectorXY))
  #print(transitionMatrix)
  vectorXY_t1 <- transitionMatrix %*% as.vector(vectorXY)
  #print(unlist(vectorXY_t1))
  #print(probXY)
  probXY_t1   <- transitionMatrix %*% probXY %*% inv(transitionMatrix)
  
  result <- list(vectorXY_t1, probXY_t1)
}

# Function to rotate a point (x, y) by an angle (in degrees) around a center point (cx, cy)
rotate_point <- function(x, y, cx, cy, angle_degrees) {
  angle_radians <- angle_degrees * (pi / 180)
  new_x <- cos(angle_radians) * (x - cx) - sin(angle_radians) * (y - cy) + cx
  new_y <- sin(angle_radians) * (x - cx) + cos(angle_radians) * (y - cy) + cy
  return(c(new_x, new_y))
}


# create new dataset
dataset=tracks[recordingId==08]
# unshift ids
dataset=dataset[,trackId:=(trackId-recordingId*100000)]


# Choix d'un binôme de trajectoires
rId=8
trajA = 20
trajB = 21
# Choix d'un temps (frame)
f = 1285
# Visualisation de la situation
drawSituation(1,trajA,trajB,f)

# Calcul de la prédiction des deux positions
# Trajectoire A
vectorXY_A = unlist(dataset[trackId==trajA & frame==f,.(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
vectorXY_B = unlist(dataset[trackId==trajB & frame==f,.(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
probXY   = matrix(c(c(0.1,  0,   0,   0,    0,    0), 
                  c(0  ,0.1,   0,   0,    0,    0),
                  c(0  ,  0,0.01,   0,    0,    0),
                  c(0  ,  0,   0,0.01,    0,    0),
                  c(0  ,  0,   0,   0,0.001,    0),
                  c(0  ,  0,   0,   0,    0,0.001)), nrow=6)


predictedA <- predictPosition(vectorXY_A,probXY, dt=1)
predictedB <- predictPosition(vectorXY_B,probXY, dt=1)
# Placement des points prédits
points(unlist(predictedA[1],recursive=F)[1],unlist(predictedA[1],recursive=F)[2], col='green', pch=19)
points(unlist(predictedB[1],recursive=F)[1],unlist(predictedB[1],recursive=F)[2], col='green', pch=19)


# Tirages de monte carlo
tiragesA <- mvrnorm(n = 100, 
        unlist(predictedA[1],recursive=F)[1:2], 
        matrix(c(unlist(predictedA[2],recursive=F)[1:2], unlist(predictedA[2],recursive=F)[7:8]), nrow=2)
                                                         , tol = 1e-06, empirical = FALSE)
points(tiragesA, col='orange', pch=19)

tiragesB <- mvrnorm(n = 100, 
                    unlist(predictedB[1],recursive=F)[1:2], 
                    matrix(c(unlist(predictedB[2],recursive=F)[1:2], unlist(predictedB[2],recursive=F)[7:8]), nrow=2)
                    , tol = 1e-06, empirical = FALSE)
points(tiragesB, col='orange', pch=19)

# Ajout de la boite englobante
xA=tiragesA[1,1]
yA=tiragesA[1,2]
lengthBoxA <- unlist(tracksMeta[,.(meanWidth=mean(width),meanLength=mean(length)), by=.(recordingId,class)][class==tracksMeta[recordingId==rId & trackId==100000*rId+trajA,class] & recordingId==rId,.(meanWidth,meanLength)])
boxA <- matrix(c(
  xA+lengthBoxA[2],yA+lengthBoxA[1],
  xA+lengthBoxA[2],yA-lengthBoxA[1],
  xA-lengthBoxA[2],yA-lengthBoxA[1],
  xA-lengthBoxA[2],yA+lengthBoxA[1], 
  xA+lengthBoxA[2],yA+lengthBoxA[1]),ncol=2,byrow = TRUE
)
lines(boxA, col='pink')

xB=tiragesB[1,1]
yB=tiragesB[1,2]
lengthBoxB <- unlist(tracksMeta[,.(meanWidth=mean(width),meanLength=mean(length)), by=.(recordingId,class)][class==tracksMeta[recordingId==rId & trackId==100000*rId+trajB,class] & recordingId==rId,.(meanWidth,meanLength)])
boxB <- matrix(c(
  xB+lengthBoxB[2],yB+lengthBoxB[1],
  xB+lengthBoxB[2],yB-lengthBoxB[1],
  xB-lengthBoxB[2],yB-lengthBoxB[1],
  xB-lengthBoxB[2],yB+lengthBoxB[1], 
  xB+lengthBoxB[2],yB+lengthBoxB[1]),ncol=2,byrow = TRUE
)
lines(boxB, col='pink')

# Rotation de la boite englobante
# Define the center of rotation
angle_degrees <- unlist(dataset[trackId==trajA & frame==f,heading])
rotatedBoxA <- t(apply(boxA, 1, function(p) rotate_point(p[1], p[2], xA, yA, angle_degrees)))
lines(rotatedBoxA, col = "red")

angle_degrees <- unlist(dataset[trackId==trajB & frame==f,heading])
rotatedBoxB <- t(apply(boxB, 1, function(p) rotate_point(p[1], p[2], xB, yB, angle_degrees)))
lines(rotatedBoxB, col = "red")


# Mesure si il y a chevauchement ou pas
foo <- polyInt(boxA,boxB, stopAtFirst = FALSE)


