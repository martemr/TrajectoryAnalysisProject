##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 23th
# Description : Draw of a chord diagram of each directions origine/destination
#  -----------> Résultat illogique par rapport au carrefour (en fonction du nord,sud,est,ouest) 
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init
# Run 04_origine-destinationClustering

#==========================================
# Parametres
#==========================================
RoadsNumberFrom = 3
RoadsNumberTo = 4

#==========================================
# Librairies
#==========================================
library(circlize)
library(dplyr)

#==========================================
# Functions
#==========================================
getCardPoint <- function(val){
  count = 1
  heading = intervalles[count]
  print(val)
  while (val > heading) {
    count = count + 1
    heading = intervalles[count]
  }
  noms[count-1]
}

#==========================================
# Création de la matrice Origine-Destination-NbClusters
#==========================================
# Origine
origine <- data.frame(0,origineHeading$heading, row.names = origineHeading$trackId) %>%
  dist %>%
  hclust %>%
  cutree(k=RoadsNumberFrom) %>% 
  data.frame('origineId'=., 'trackId'=origineHeading$trackId)

origineX <- filter(trajectoriesDataset, trackLifetime < 5) %>% 
  group_by(trackId) %>% 
  summarize_at(c('xCenter', 'yCenter'), function(l) mean(l)) %>%
  left_join(origine, by='trackId') %>% 
  group_by(origineId) %>%
  summarise_at(c('xCenter', 'yCenter'), mean)

# Destination
destination <- data.frame(0,destinationHeading$heading, row.names = destinationHeading$trackId) %>%
  dist %>%
  hclust %>%
  cutree(k=RoadsNumberTo) %>% 
  data.frame('destinationId'=., 'trackId'=destinationHeading$trackId)

lastTracksTime <- trajectoriesDataset %>% 
  group_by(trackId) %>% 
  summarise_at('group',function(l) getLastTrackTimes(l,5))
lastTracks <- left_join(lastTracksTime, trajectoriesDataset, by=c('trackId', 'group'))

destinationX <- lastTracks %>% 
  group_by(trackId) %>%
  summarize_at(c('xCenter', 'yCenter'), function(l) mean(l)) %>%
  left_join(destination, by='trackId') %>% 
  group_by(destinationId) %>%
  summarise_at(c('xCenter', 'yCenter'), mean)

# Cumul
origDest <- full_join(origineX, destinationX)
groupes <- origDest[, 2:3] %>% 
  dist %>%
  hclust %>%
  cutree(k =max(RoadsNumberFrom,RoadsNumberTo)) %>%
  data.frame('diagrammeId'=., 
             'origineId'=origDest$origineId, 
             'destinationId'=origDest$destinationId, 
             'xCenter'=origDest$xCenter, 
             'yCenter'=origDest$yCenter)
groupesCoords <- groupes %>%
  group_by(diagrammeId) %>%
  summarise_at(c('xCenter','yCenter'), mean)


diagramData <- full_join(origine,destination, by='trackId') %>% 
  inner_join(.,groupes[,c('diagrammeId','origineId')], by='origineId') %>% 
  inner_join(.,groupes[,c('diagrammeId','destinationId')], by='destinationId') %>%
  full_join(., clusters, by='trackId')

chordData <- diagramData %>% 
  group_by(clusterId, diagrammeId.x, diagrammeId.y) %>% 
  count


plot (0,xlim=xlim,ylim=ylim,axes=T,ylab="Y",xlab="X",main=paste("Clusters"))
setBackground()
text(groupesCoords[,2:3],lab=groupesCoords$diagrammeId, col='orange')

chordDiagram(chordData[,2:4], directional =1,direction.type = c("diffHeight", "arrows"))
