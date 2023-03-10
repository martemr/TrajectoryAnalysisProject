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
#intervalles <- c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
#noms <- c('Nord','Nord-Est','Est','Sud-Est','Sud','Sud-Ouest','Ouest','Nord-Ouest','Nord')       
intervalles <- c(0, 67.5, 157.5,247.5, 337.5, 360)
noms <- c('Nord','Est','Sud','Ouest','Nord')

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
mat <- inner_join(origineHeading, destinationHeading, by="trackId") %>% 
  left_join(clusters, by='trackId') %>%
  left_join(count(clusters, clusterId), by="clusterId")
colnames(mat) <- c('trackId', 'origineHeading', 'destinationHeading','clusterId','clusterNb') 

originCard <- data.frame(
  'origineCard'=unlist(lapply(mat$origineHeading, getCardPoint)), 
  'destinationCard'=unlist(lapply(mat$destinationHeading, getCardPoint)), 
                           'clusterId'=mat$clusterNb)
chordDiagram(unique(originCard))
