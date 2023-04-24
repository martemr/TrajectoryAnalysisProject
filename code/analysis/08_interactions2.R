

selectSameTimeId <- function(studiedDatasetRecording, studiedTrackId, idToFilter=unique(studiedDatasetRecording$trackId)){
  ids <- unique(idToFilter[idToFilter %in% unique(studiedDatasetRecording[frame > min(studiedDatasetRecording[trackId == studiedTrackId,frame]) & 
                                                                            frame < max(studiedDatasetRecording[trackId == studiedTrackId,frame]) 
                                                                          ,trackId])])
  ids[ids!=studiedTrackId]
}

# Champ de vision
getViewFieldPoints <- function(studiedTrackId,studiedFrame, numberOfPoints=10){
  viewField <- data.table()
  for (d in seq(numberOfPoints,1)*10){
    viewField<- rbind(viewField, 
          translate(x    =trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, xCenter],
                    y    =trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, yCenter],
                    angle=trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, heading+60], 
                    distanceToTranslate = d )
    )
  }
  viewField<- rbind(viewField, trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, .(xCenter,yCenter)])
  for (d in seq(1,numberOfPoints)*10){
    viewField<- rbind(viewField, 
                      translate(x    =trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, xCenter],
                                y    =trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, yCenter],
                                angle=trajectoriesDataset[trackId==studiedTrackId & frame==studiedFrame, heading-60], 
                                distanceToTranslate = d )
    )
  }
  
  viewField<- rbind(viewField, viewField[1])
  viewField
}

# Translation de distanceToTranslate
translate <- function(x,y,angle,distanceToTranslate=10){
  angle = angle * (pi / 180)
  data.table(xCenter=(x + distanceToTranslate * cos(angle)),
             yCenter=(y + distanceToTranslate * sin(angle)))
}



# Définition du sens 
getSens <- function(angle1, angle2){
  angle=abs(angle1-angle2)
  if(angle>180){
    angle=angle-180
  }
  
  if(angle<10) {
    "identique"
  } else if (angle<90){
    "perp_suivi"
  } else if (angle<170){
    "perp_oppose"
  } else if (angle<180){
    "oppose"
  } else {
    stop("Angle négatif")
  }
}

for (s in seq(1,10))


getStopDistance <- function(speed){
  unlist(speed+(speed/10)^2)
}


getSpeed <- function(xSpeed,ySpeed){
  unlist(sqrt(xSpeed^2+ySpeed^2))
}




# Itération pour chaque frame
getInteractionsOfTrack <- function(studiedTrackId, studiedRecordingId=0, plotArea=FALSE, verbose=FALSE){
  if(plotArea) drawEmptyPlot(paste('Interactions of',studiedTrackId))
  
  interactionsTotal <- data.table()
  
  start_time <- Sys.time()
  
  # Récupération de toutes les track au même moment
  listOtherTracksId <- selectSameTimeId(studiedDatasetRecording = trajectoriesDataset[recordingId==studiedRecordingId,], 
                                        studiedTrackId = studiedTrackId,
                                        idToFilter=trajectoriesDataset[recordingId==studiedRecordingId,trackId])
  
  for (f in trajectoriesDataset[trackId==studiedTrackId,frame]){
    # On détermine le champ de vision
    shapeViewField <- as.matrix(getViewFieldPoints(studiedTrackId,f))
    #if(plotArea) lines(shapeViewField, col='blue')
    
    # Selection des tracks dans le champ de vision et présent sur la frame
    ids <- 
      unique(  trajectoriesDataset[trackId %in% listOtherTracksId & frame==f, trackId])[as.logical(point.in.polygon(
        unlist(trajectoriesDataset[trackId %in% listOtherTracksId & frame==f, .(xCenter)]),
        unlist(trajectoriesDataset[trackId %in% listOtherTracksId & frame==f, .(yCenter)]),
        shapeViewField[, 1],
        shapeViewField[, 2]
      ))]
    
    # print(paste("Speed", getSpeed(trajectoriesDataset[trackId==studiedTrackId & frame==f,.(xVelocity)],
    #                               trajectoriesDataset[trackId==studiedTrackId & frame==f,.(yVelocity)])))
    
    if(plotArea){
      points(trajectoriesDataset[trackId==studiedTrackId & frame==f, .(xCenter,yCenter)], col='red')
      points(trajectoriesDataset[trackId %in% ids & frame==f, .(xCenter,yCenter)], col='green')
    }
    
    # Definition de l'interraction
    
    #  Recherche de la distance d'arret
    d = getStopDistance(getSpeed(trajectoriesDataset[trackId==studiedTrackId & frame==f,.(xVelocity)],
                                 trajectoriesDataset[trackId==studiedTrackId & frame==f,.(yVelocity)]))
    if (d<4) d=4
    
    # Classification de l'interaction
    for(id in ids){
      #  Recherche du sens relatif
      sens = getSens(trajectoriesDataset[trackId==studiedTrackId & frame==f, .(heading)],
                     trajectoriesDataset[trackId==id & frame==f, .(heading)])
      
      # Calcul de l'écart
      realDist <- euclidean(trajectoriesDataset[trackId==studiedTrackId & frame==f,.(xCenter,yCenter)],
                            trajectoriesDataset[trackId==id & frame==f,.(xCenter,yCenter)])
      
      if(realDist>d){
        interaction <- "Pas interaction"
      } else if(realDist<d/2){
        interaction <- "Conflit"
      } else {
        if(sens %in% c("suivi","perp_suivi")) {
          interaction <-"Inconfort"
        } else {
          interaction <- "Conflit"
        }
      }
      #print(paste("d"=d))  
      #print(interactionsTotal)
      interactionsTotal <- rbind(interactionsTotal,
                                 list('trackId1'=studiedTrackId,
                                      'trackId2'=id,
                                      'frame'=f,
                                      'interaction'=interaction,
                                      'seuil'=d,
                                      'realDistance'=realDist,
                                      'sens'=sens 
                                      ),fill=TRUE)
      
      
      # Affichages
      if(verbose){
        print(paste("Interaction ",studiedTrackId,'-',id, sep=''))
        print(paste(" > Seuil:",d,sep=''))
        print(paste(" > Reel:" ,realDist,sep=''))
        print(paste(" >",interaction))
      }
      if(plotArea){
        if(interaction=="Conflit"){
          lines(x=unlist(trajectoriesDataset[trackId %in% c(studiedTrackId,id) & frame==f, .(xCenter)]),
                y=unlist(trajectoriesDataset[trackId %in% c(studiedTrackId,id) & frame==f, .(yCenter)]),
                col="red")
        } else if(interaction=="Inconfort"){
          lines(x=unlist(trajectoriesDataset[trackId %in% c(studiedTrackId,id) & frame==f, .(xCenter)]),
                y=unlist(trajectoriesDataset[trackId %in% c(studiedTrackId,id) & frame==f, .(yCenter)]),
                col="orange")
        }
        
      }
    }
  }
  end_time <- Sys.time()
  #interactionsTotal <- cbind(interactionsTotal, 'executionTime'=end_time-start_time)
  print(paste("Execution time", end_time-start_time))
  interactionsTotal
}








interactionsDataset <- data.table()
count=0
for (t in (tracksMeta[recordingId==0,trackId])){
  print(paste('Track',t,":",paste(count,"/",n_distinct(tracksMeta[recordingId==0,trackId]),sep="")))
  interactionsDataset <- rbind(interactionsDataset,getInteractionsOfTrack(t,0))
  count=count+1
}



