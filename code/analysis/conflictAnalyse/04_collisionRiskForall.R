library(data.table)
library(matlib)
library(MASS)
library(cgwtools)

# Dataset of size of boxes 
lengthBoxDataset <- tracksMeta[,.(meanWidth=mean(width),meanLength=mean(length)), by=.(recordingId,class)]

# Function to rotate a point (x, y) by an angle (in degrees) around a center point (cx, cy)
rotate_point <- function(x, y, cx, cy, angle_degrees) {
  angle_radians <- angle_degrees * (pi / 180)
  new_x <- cos(angle_radians) * (x - cx) - sin(angle_radians) * (y - cy) + cx
  new_y <- sin(angle_radians) * (x - cx) + cos(angle_radians) * (y - cy) + cy
  return(c(new_x, new_y))
}

# Function that takes a position, a direction and a size of box and return the box rotated
addBox <- function(pointsXY, heading_degrees, lengthBox){
  # Define box
  box <- matrix(c(
    pointsXY[1]+lengthBox[2],pointsXY[2]+lengthBox[1],
    pointsXY[1]+lengthBox[2],pointsXY[2]-lengthBox[1],
    pointsXY[1]-lengthBox[2],pointsXY[2]-lengthBox[1],
    pointsXY[1]-lengthBox[2],pointsXY[2]+lengthBox[1], 
    pointsXY[1]+lengthBox[2],pointsXY[2]+lengthBox[1]),ncol=2,byrow = TRUE)
  
  # Rotate box  
  rotatedBox <- t(apply(box, 1, function(p) rotate_point(p[1], p[2], pointsXY[1], pointsXY[2], heading_degrees)))
  return(data.table(rotatedBox))
}

predictPosition <- function(vectorXY, dt=0.04){
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
  
  # Probabilités
  probXY   = matrix(c(c(0.1,  0,   0,   0,    0,    0), 
                      c(0  ,0.1,   0,   0,    0,    0),
                      c(0  ,  0,0.01,   0,    0,    0),
                      c(0  ,  0,   0,0.01,    0,    0),
                      c(0  ,  0,   0,   0,0.001,    0),
                      c(0  ,  0,   0,   0,    0,0.001)), nrow=6)
  
  vectorXY_t1 <- transitionMatrix %*% as.vector(vectorXY)
  probXY_t1   <- transitionMatrix %*% probXY %*% inv(transitionMatrix)
  return(list(vectorXY_t1, probXY_t1))
}

# Retourne le risque de collision entre deux trajectoires
# tA et tB les deux id de trajectoires
# f la frame
# dt intervalle de temps en seconde
# dataset le jeu de données à utiliser
#
# RETURN : Si pas d'interaction possible retourne 0, sinon le risque entre 0 et 1
getCollisionCount <- function(tA, tB, f, dt, dataset, nTirages){
  if(nrow(dataset[trackId %in% c(tA,tB) & frame==f,])<2) return(0) # Pas d'interaction possible
  
  # Prédiction des positions futures
  predictedA <- predictPosition(unlist(dataset[trackId==tA & frame==f,  .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]), dt=dt)
  predictedB <- predictPosition(unlist(dataset[trackId==tB & frame==f,  .(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)]), dt=dt)
  
  # Tirages de Monte Carlo
  tiragesA <- mvrnorm(n = nTirages, unlist(predictedA[1],recursive=F)[1:2], matrix(c(unlist(predictedA[2],recursive=F)[1:2], unlist(predictedA[2],recursive=F)[7:8]), nrow=2), tol = 1e-06, empirical = FALSE)
  tiragesB <- mvrnorm(n = nTirages, unlist(predictedB[1],recursive=F)[1:2], matrix(c(unlist(predictedB[2],recursive=F)[1:2], unlist(predictedB[2],recursive=F)[7:8]), nrow=2), tol = 1e-06, empirical = FALSE)
  
  #Calcul de la boite englobante 
  headingA <- dataset[trackId==tA & frame==f,heading]
  headingB <- dataset[trackId==tB & frame==f,heading]
  boxA <- apply(tiragesA, 1, f <- function(x) {addBox(pointsXY = x, 
                                                      heading_degrees = headingA,
                                                      lengthBox = unlist(lengthBoxDataset[recordingId==7 & class=='car',.(meanWidth,meanLength)]))})
  boxB <- apply(tiragesB, 1, f <- function(x) {addBox(pointsXY = x, 
                                                      heading_degrees = headingB,
                                                      lengthBox = unlist(lengthBoxDataset[recordingId==7 & class=='car',.(meanWidth,meanLength)]))})
  
  # Compte du nombre de collisions
  collisions <- apply(data.table(A=boxA, B=boxB), 1, function(box){!is.null(polyInt(as.data.table(box[1]),as.data.table(box[2]), stopAtFirst = FALSE))})
  return(sum(collisions))
}


###################           
  

# 
# 
# library(MASS)
# library(cgwtools)
# library(spatialEco)
# 
# drawSituation <- function(Loc, trajectoireA, trajectoireB, f){
#   initPlotImage(Loc)
#   drawEmptyPlot()
#   lines(dataset[trackId==trajectoireA, .(xCenter, yCenter)], lwd=2, col='blue')
#   lines(dataset[trackId==trajectoireB, .(xCenter, yCenter)], lwd=2, col='blue')
#   
#   points(dataset[trackId==trajectoireB & frame==f, .(xCenter, yCenter)], col='red', pch=19)
#   points(dataset[trackId==trajectoireA & frame==f, .(xCenter, yCenter)], col='red', pch=19)
# }
# 
# 
# 
# 
# 
# getCollisionDataset <- function(nbFrames, rId, trajectoriesDataset, dt=1, nTirages=100){
#   dataset=trajectoriesDataset[recordingId==rId]
#   toPlot=F
# 
#   # Create temp file location
#   tmp <- file.path(format(Sys.time(), "collisionRisk_%d-%m-%Y_%H-%M-%S.log"))
#   # Open log
#   lf <- log_open(tmp)
#   
#   collisionFullDataset <- data.table()
#   count=0
#   start_time <- Sys.time()
#   for (f in unique(dataset$frame)[1:nbFrames]){
#     count = count + 1
#     # On récupère les trajectoires de la frame
#     trajectories <- dataset[frame==f,.(trackId, xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration, heading)]
#     log_print(paste("Frame :", f, " : ", nrow(trajectories)," trajectoires"))
#     
#     if(nrow(trajectories)<2){
#       next
#     }
#     
#     collisionDataset <- data.table(t(combn(trajectories$trackId, 2)))
#     colnames(collisionDataset) <- c('trackA', 'trackB')
#     collisionDataset <- cbind(frame=f, collisionDataset)
#     collisionDataset <- cbind(collisionDataset, collisionRate=0)
#     
#     getCollision <-function(frame, trackA, trackB, dt, nTirages)
#     
#     for (row in seq(1,nrow(collisionDataset))){
#       # Recupération d'un paire de trajectoire
#       tA <- collisionDataset[row, trackA]
#       tB <- collisionDataset[row, trackB]
#       
#       
# 
#       
#       sprintf("tA=%d, tB=%d, collision rate=%d",tA, tB, collisionCount)
#       collisionDataset[row,collisionRate := collisionCount/100]
#       
#       if(toPlot){
#         points(unlist(predictedA[1],recursive=F)[1],unlist(predictedA[1],recursive=F)[2], col='green', pch=19)
#         points(unlist(predictedB[1],recursive=F)[1],unlist(predictedB[1],recursive=F)[2], col='green', pch=19)
#         points(tiragesB, col='orange')
#         points(tiragesB, col='orange')
#       }
#       
#       if (count>100){
#         # Stocker les données
#         fwrite(collisionFullDataset, format(Sys.time(), "collisionDataset_%d-%m-%Y_%H-%M-%S.csv"))
#         
#         # Vider le buffer
#         collisionFullDataset <- data.table()
#         
#         # Redemarrer le compte
#         count=0
#       }
#     }
#     
#     collisionFullDataset <- rbind(collisionFullDataset, collisionDataset)
#   }
#   #end_time <- Sys.time()
#   #log_print(paste("Execution time", end_time-start_time))
#   collisionFullDataset
# }
# 
# 
# #getCollisionDataset(rId=8, trajectoriesDataset = trajectoriesDataset)
# 
