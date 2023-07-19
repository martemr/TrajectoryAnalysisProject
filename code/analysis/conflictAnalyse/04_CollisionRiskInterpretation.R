##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : July 2023
# Description : Analyse des résultats de collisionRiskParallel
##---------------------------------------------

# Chargement des données
collisionRiskDataset <- fread('./resultAllCollisions.csv', header = TRUE, sep = ",")



plot(collisionRiskDataset[trackId1<100000, mean(collRisk),by=.(trackId1)], col=getCol(tracksMeta[trackId = collisionRiskDataset$trackId1,])
collisionRiskDataset[, mean(collRisk),by=.(trackId1)]

collisionRiskDataset <- cbind(collisionRiskDataset[trackId1<100000, mean(collRisk),by=.(trackId1)],unlist(lapply(unlist(tracksMeta[trackId %in% unique(collisionRiskDataset$trackId1), class]), getCol)))


apply(collisionRiskDataset,1, function(x){points(x[1:2], col=str(x[3]))})



collisionRiskDataset <- cbind(collisionRiskDataset, 'class1'=unlist(lapply(unique(collisionRiskDataset[,trackId1]), function(x){tracksMeta[trackId==x,class]})))
collisionRiskDataset <- cbind(collisionRiskDataset, 'class2'=unlist(lapply(unique(collisionRiskDataset[,trackId2]), function(x){tracksMeta[trackId==x,class]})))


test <- collisionRiskDataset[,mean(collRisk),by=.(trackId1,trackId2)]
getRecordingId <- function(id){
  tracksMeta[trackId==id,recordingId]
}


collisionRiskDataset <- cbind(collisionRiskDataset,recordingId=lapply(collisionRiskDataset$trackId1, getRecordingId))
collisionRiskDataset[, sum(trackId),by=recordingId]

