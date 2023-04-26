##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Interprétation des résultats du dataset interactions
##---------------------------------------------



interactionsDataset <- data.table(read.csv("~/TrajectoryAnalysisProject/run interactions 24.04.23/interactionsDataset.csv"))

addClassInteractionsDataset <- function(interactionsDataset){
  interactionsDataset <- interactionsDataset[tracksMeta[,.(class1=class,trackId)], on=.(trackId1==trackId), nomatch=NULL]
  interactionsDataset <- interactionsDataset[tracksMeta[,.(class2=class,trackId)], on=.(trackId2==trackId), nomatch=NULL]
}


#==========================================
# Chord classes
#==========================================
# Load the circlize library
library(circlize)
# Create agency matrix
conflitAgency <- table(interactionsDataset[interaction=='Conflit',.(class1,class2)])
InconfortAgency <- table(interactionsDataset[interaction=='Inconfort',.(class1,class2)])
NoInteractionAgency <- table(interactionsDataset[interaction=='Pas interaction',.(class1,class2)])

# Make the circular plot
par(mfrow=c(2,3))
chordDiagram(conflitAgency      , transparency = 0.5)
chordDiagram(InconfortAgency    , transparency = 0.5)
chordDiagram(NoInteractionAgency, transparency = 0.5)


#==========================================
# Affichage du nombre d'interactions par frame
#==========================================
plot(table(interactionsDataset[interaction %in% c('Conflit', 'Inconfort'),frame]),200)



#==========================================
# Affichage des interactions
#==========================================
getColor <- function(interaction){
  if (interaction=="Conflit"){
    'red'
  } else if(interaction=="Inconfort"){
    'orange'
  } else if (interaction=="Pas interaction"){
    'green'
  }
}

drawInterractions <- function(studiedTrackId1, studiedTrackId2){
  drawEmptyPlot(paste("Interraction of", studiedTrackId1,'-', studiedTrackId2))

  # Tracé des trajectoires étudiées
  drawTrajectory(4,studiedTrackId1,dosinit,col='blue',newPlot=FALSE)
  drawTrajectory(4,studiedTrackId2,dosinit,col='blue',newPlot=FALSE)
  #addArrow(studiedTrackId1, color='blue')
  #addArrow(studiedTrackId2, color='blue')
  #print("ok")
  for (f in interactionsDataset[trackId1==studiedTrackId1,][trackId2==studiedTrackId2,frame]){
    lines(x=c(trajectoriesDataset[trackId==studiedTrackId1 & frame==f,.(xCenter)],
          trajectoriesDataset[trackId==studiedTrackId2 & frame==f,.(xCenter)]), 
          y=c(trajectoriesDataset[trackId==studiedTrackId1 & frame==f,.(yCenter)], 
          trajectoriesDataset[trackId==studiedTrackId2 & frame==f,.(yCenter)]), 
          col=getColor(unlist(interactionsDataset[trackId1==studiedTrackId1,][trackId2==studiedTrackId2,][frame==f,interaction])))
    #print(getColor(unlist(interactionsDataset[trackId1==studiedTrackId1,][trackId1==studiedTrackId1,][frame==f,interaction])))
    }
}

drawAllInteractionsTrack <- function(studiedTrack){
  for (t2 in unique(interactionsDataset[trackId1==studiedTrack,trackId2])){
    drawInterractions(studiedTrack,t2)
  }
}


trackIds <- unique(interactionsDataset[class1=='car' & class2=='bicycle' & interaction=='Conflit',.(trackId1,trackId2)])
for (t in seq(1,nrow(trackIds))){
  print(paste("Tracks",trackIds[t]))
  drawInterractions(unlist(trackIds[t])[1],
                    unlist(trackIds[t])[2])
  readline()
  }




