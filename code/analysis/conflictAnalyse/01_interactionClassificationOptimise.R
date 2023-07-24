##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : July 2023
# Description : Interactions
##---------------------------------------------

translate <- function(x,y,angle,distanceToTranslate=10){
  angle = angle * (pi / 180)
  data.table(xCenter=(x + distanceToTranslate * cos(angle)),
             yCenter=(y + distanceToTranslate * sin(angle)))
}

getSens <- function(angle1, angle2){
  angle=abs(angle1-angle2)
  if(angle>180) angle=angle-180
  if (angle>10 & angle<170){
    "perp"
  } else {
    "id"
  }
}
#==========================================
# Calcul de la distance de sécurité
#==========================================
getStopDistance <- function(speed){
  unlist(speed+(speed/10)^2)
}

#==========================================
# Calcul de la vitesse d'un point
#==========================================
getSpeed <- function(xSpeed,ySpeed){
  unlist(sqrt(xSpeed^2+ySpeed^2))
}


# Détermine le champ de vision
getViewFieldPoints <- function(tId,f,numberOfPoints=10){
  viewField <- rbindlist(lapply(seq(numberOfPoints,1)*10, function(d) translate(x    =trajectoriesDataset[trackId==tId & frame==f, xCenter],
                                                                                y    =trajectoriesDataset[trackId==tId & frame==f, yCenter],
                                                                                angle=trajectoriesDataset[trackId==tId & frame==f, heading+60], 
                                                                                distanceToTranslate = d )))
  viewField <- rbind(viewField, trajectoriesDataset[trackId==tId & frame==f,.(xCenter,yCenter)])
  viewField <- rbind(viewField, rbindlist(lapply(seq(numberOfPoints,1)*10, function(d) translate(x    =trajectoriesDataset[trackId==tId & frame==f, xCenter],
                                                                                                 y    =trajectoriesDataset[trackId==tId & frame==f, yCenter],
                                                                                                 angle=trajectoriesDataset[trackId==tId & frame==f, heading-60], 
                                                                                                 distanceToTranslate = d ))))
  viewField<- rbind(viewField, viewField[1])
  viewField
}


# Qualifie l'intéraction
getTypeOfInteraction <-function(Id1,Id2,frame,stopDistance,realDistance,sens){
  # Définition de l'interaction
  interaction<-"Pas d'interaction"
  if(realDistance>stopDistance){
    # Pas d'interactions
  } else if(realDistance>stopDistance/2){
    if(sens %in% c("perp")) {
      interaction <- "Inconfort"
    } else {
      # Pas d'interactions
    }
  } else {
    if(sens %in% c("id")) {
      interaction <-"Inconfort"
    } else {
      interaction <- "Conflit"
    }
  }
  data.table('trackId1'=Id1,'trackId2'=Id2,'frame'=frame,'interaction'=interaction, 'stopDistance'=stopDistance, 'realDistance'=realDistance,'sens'=sens)
}

# Récupère  les intéractions d'un point à un instant
getInteractionsPointFrame <- function(tId,f,dataset){
  # On détermine le champ de vision
  shapeViewField <- as.matrix(getViewFieldPoints(tId,f))
  #if(plotArea) lines(shapeViewField, col='blue')
  
  # Selection des tracks dans le champ de vision et présent sur la frame
  ids <- 
    unique(  dataset[frame==f & trackId!=tId, trackId])[as.logical(point.in.polygon(
      unlist(dataset[frame==f & trackId!=tId, .(xCenter)]),
      unlist(dataset[frame==f & trackId!=tId, .(yCenter)]),
      shapeViewField[, 1],
      shapeViewField[, 2]
    ))]
  
  if(length(ids)!=0){ # Skip si vide
    # Recherche de la distance d'arrêt
    d = getStopDistance(getSpeed(trajectoriesDataset[trackId==tId & frame==f,.(xVelocity)],trajectoriesDataset[trackId==tId & frame==f,.(yVelocity)]))
    if (d<2) d=2
    
    thisInteraction <- lapply(ids, function(thisId) getTypeOfInteraction(tId,thisId,f,
                         d, 
                         euclidean(trajectoriesDataset[trackId==tId & frame==f,.(xCenter,yCenter)],trajectoriesDataset[trackId==thisId & frame==f,.(xCenter,yCenter)]),
                         getSens(trajectoriesDataset[trackId==tId & frame==f, .(heading)],trajectoriesDataset[trackId==thisId & frame==f, .(heading)])))
    thisInteraction
  }
}

# Récupère toutes les interactions sur la frame
getAllInteractionOfFrame <-function(f,dataset){
  lapply(dataset[frame==f,trackId],function(x) getInteractionsPointFrame(x, f, dataset))
}

# Récupère toute les intéractions d'un enregistrement
getAllInteractionsOfRecording <- function(rId){
  datastet=trajectoriesDataset[recordingId==rId,]
  
  interactions <- lapply(unique(dataset$frame)[table(dataset$frame)>1], # On applique uniquement aux frames où il a plus d'une trajectoire 
                         function(x) getAllInteractionOfFrame(x,dataset))
  interactions
}
