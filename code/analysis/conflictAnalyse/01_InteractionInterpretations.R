##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Interprétation des résultats du dataset interactions
##---------------------------------------------
library(circlize)


#==========================================
# Lecture des fichiers interactions
#==========================================
loadAllInteractions <- function(dosinit="./data/"){
  interactions <- data.table()
  for (i in 0:15) {
    interactionsName <- sprintf("%s%02d_interactions.csv", dosinit, i)
    interactions <- rbindlist(list(interactions, fread(interactionsName, header = TRUE, sep = ",")))
  }
  interactions
}

#==========================================
# Ajout des classes des objets
#==========================================
addClassInteractionsDataset <- function(interactionsDataset){
  interactionsDataset <- interactionsDataset[tracksMeta[,.(class1=class,trackId)], on=.(trackId1==trackId), nomatch=NULL]
  interactionsDataset <- interactionsDataset[tracksMeta[,.(class2=class,trackId)], on=.(trackId2==trackId), nomatch=NULL]
  interactionsDataset
}

#==========================================
# Chord classes
#==========================================
drawChordDiagramInteractions <- function(interactionsDataset){
  # Create agency matrix
  conflitAgency <- table(interactionsDataset[interaction=='Conflit',.(class1,class2)])
  InconfortAgency <- table(interactionsDataset[interaction=='Inconfort',.(class1,class2)])
  #NoInteractionAgency <- table(interactionsDataset[interaction=='Pas interaction',.(class1,class2)])
  
  # Make the circular plot
  par(mfrow=c(2,2))
  chordDiagram(conflitAgency      , annotationTrack = c("name",'grid'))
  title(main="Conflits")
  chordDiagram(InconfortAgency    , annotationTrack = c("name",'grid'))
  title(main="Inconforts")
}

#==========================================
# Affichage du nombre d'interactions par frame
#==========================================
plotDensityPerFrame <- function(interactionsDataset) plot(density(interactionsDataset[interaction %in% c('Conflit', 'Inconfort'),frame]), main="Densité d'interactions conflictuelles par frame", xlab='Frame',ylab="Densité")

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

drawInterractions <- function(interactionsDataset, studiedTrackId1, studiedTrackId2){
  drawEmptyPlot(1, paste("Interraction of", studiedTrackId1,'-', studiedTrackId2))

  # Tracé des trajectoires étudiées
  drawTrajectory(LocationId = 1,tId = studiedTrackId1,col='blue',add = T)
  drawTrajectory(LocationId = 1,tId = studiedTrackId2,col='blue',add = T)
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

interactions <- loadAllInteractions()
interactions <- addClassInteractionsDataset(interactions)

pie(table(interactions$interaction))

drawEmptyPlot("")
trackIds <- unique(interactions[class1=='car' & class2=='car' & interaction=='Conflit',.(trackId1,trackId2)])
for (t in seq(1,nrow(trackIds))){
  print(paste("Tracks",trackIds[t]))
  drawInterractions(interactions, 
                    unlist(trackIds[t])[1],
                    unlist(trackIds[t])[2])
  readline()
  }





