library("parallel")
# Récupération des infos des dataset déjà triés :
#  tris par image, sens, distance
# library(readr)
# interactions07 <- read_csv("data/07_interactions.csv")
# interactions07 = data.table(interactions07)
# 
# for(row in seq(1,nrow(interactions07))){
#   thisInteraction = interactions07[row]
#   result = getCollisionCount(thisInteraction$trackId1, thisInteraction$trackId2, f=thisInteraction$frame,dt = 3,dataset = trajectoriesDataset[recordingId==7,], nTirages=100)
#   rbind(thisInteraction, result)
# }
# 
# getConflitRisk <- function(row){
#   thisInteraction = interactions07[row]
#   result = getCollisionCount(thisInteraction$trackId1, thisInteraction$trackId2, f=thisInteraction$frame,dt = 3,dataset = trajectoriesDataset[recordingId==7,], nTirages=100)
#   print(result)
#   rbind(thisInteraction, result)
# }



# Création des clusters
cl <- makeCluster(detectCores())

# Export environnement
clusterEvalQ(cl, {
  source("code/startDay.R")
  source("code/analysis/conflictAnalyse/04_collisionRiskForall.R")
  }) 

clusterEvalQ(cl, {
  library(readr)
  library(data.table)
  inters <- read_csv("data/00_interactions.csv")
  allInteractions <- as.data.table(inters)
  for (x in seq(1,18,1)){
    thisInteractions <- data.table(read_csv(sprintf("data/%02d_interactions.csv", x)))
    allInteractions <-  rbind(allInteractions,thisInteractions)
  }
})


getConflitRisk <- function(row){
  thisInteraction = allInteractions[row]
  collRisk = getCollisionCount(thisInteraction$trackId1, thisInteraction$trackId2, f=thisInteraction$frame,dt = 3,dataset = trajectoriesDataset, nTirages=100)
  cbind(thisInteraction, collRisk)
}

getAllCollisions <- function(){
  saveAllRecord <- parLapply(cl, 1:19986, getConflitRisk)
  allCollision <<- rbindlist(saveAllRecord)
  fwrite(allCollision,file = 'resultAllCollisions.csv')
}

getAllCollisions()







# 
# clusterEvalQ(cl, {
#   getConflitRisk <- function(row){
#     allInteractions <- data.table(read_csv("data/00_interactions.csv"))
# 
#     getAllInteractions <- function(x){
#       allInteractions <- data.table(read_csv(sprintf("data/%02d_recordingMeta.csv", x)))
#     }
#     #18
#     thisInteraction = interactions07[row]
#     collRisk = getCollisionCount(thisInteraction$trackId1, thisInteraction$trackId2, f=thisInteraction$frame,dt = 3,dataset = trajectoriesDataset[recordingId==7,], nTirages=100)
#     cbind(thisInteraction, collRisk)
#   }
# })
# 
# # recording 18
# clusterEvalQ(cl, {
#   library(readr)
#   interactions18 <- read_csv("data/18_interactions.csv")
#   interactions18 = data.table(interactions18)
#   getConflitRisk <- function(row,rId){
#     thisInteraction = interactions18[row]
#     collRisk = getCollisionCount(thisInteraction$trackId1, thisInteraction$trackId2, f=thisInteraction$frame,dt = 3,dataset = trajectoriesDataset[recordingId==18,], nTirages=100)
#     cbind(thisInteraction, collRisk)
#   }
# })
# 
# 
# getConflitRisk <- function(row){
#   thisInteraction = interactions18[row]
#   collRisk = getCollisionCount(thisInteraction$trackId1, thisInteraction$trackId2, f=thisInteraction$frame,dt = 3,dataset = trajectoriesDataset, nTirages=100)
#   cbind(thisInteraction, collRisk)
# }
# saveAllRecord18 <- parLapply(cl, 1:1185, getConflitRisk)
# 
# allCollision18<- rbindlist(saveAllRecord18)
# allCollision07 <- rbindlist(saveAllRecord7)
# 
# 
# plotCollRisk <- function(tId){
#   coll=allCollision18[trackId1==1800071, .(frame,collRisk)]
#   coll <- coll[trajectoriesDataset[trackId==1800071,.(frame)], on=.(frame=frame)]
#   coll <- replace(coll, is.na(coll), 0)
#   plot(coll, type='l')
# }


drawTrajectoryRiskLevel <- function(tId,rId){
  coll=allCollision18[trackId1==tId, .(frame,collRisk)]
  coll <- coll[trajectoriesDataset[trackId==tId,.(frame)], on=.(frame=frame)]
  coll <- replace(coll, is.na(coll), 0)
  
  drawEmptyPlot()
  points(trajectoriesDataset[trackId==tId,.(xCenter,yCenter)])
  
  }



# 
# clusterExport(cl, data.table)
# clusterExport(cl, matlib)
# clusterExport(cl, MASS)
# clusterExport(cl, cgwtools)
# 
# 
# clusterEvalQ(cl, source("code/analysis/conflictAnalyse/04_collisionRiskForall.R"))
# 
# save3 <- parLapply(cl, 1:10, getConflitRisk)
# stopCluster(cl)
# 
# 
# colnames(collisionDataset) <- c('trackA', 'trackB')
# collisionDataset <- cbind(frame=f, collisionDataset)
# collisionDataset <- cbind(collisionDataset, collisionRate=0)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# createCollisionDataset <- function(trackList){
#   if(length(unlist(trackList[3]))<2)return(NA)
#   data.table('recordingId'=trackList[1], 'frame'=trackList[2],t(combn(unlist(trackList[3]), 2)))
# }
# 
# 
# 
# trackList <- as.list(unlist(trajectoriesDataset[, .(tracks=list(trackId)), by=.(recordingId,frame)][,3], recursive=F))
# test <- lapply(trackList[50:60], function(x){t(combn(x,2))})
# 
# 
# rId=7
# colDataset <-rbind(trackList[recordingId==rId,frame],lapply(trackList[recordingId==rId,.(tracks)], f<-function(x){createCollisionDataset(x)}))
# 
# createCollisionDataset <- function(trackList){
#   if(length(unlist(trackList[3]))<2)return(NA)
#   data.table('recordingId'=trackList[1], 'frame'=trackList[2],t(combn(unlist(trackList[3]), 2)))
# }
# colDataset <- rbindlist(apply(trackList, 1, createCollisionDataset))
# 
# test <- lapply(trackList, function(x){length(x)})
# lapply(trackList[50:60,3], function(x){combn(x,2)})
# 
# createCollisionDataset <- function(trackList) {
#   if (length(trackList[[3]]) < 2) {
#     return(NA)
#   }
#   
#   recordingId <- trackList[[1]]
#   frame <- trackList[[2]]
#   combinations <- combn(trackList[[3]], 2)
#   data.table('recordingId' = recordingId, 'frame' = frame, t(combinations))
# }
# 
# colDataset <- rbindlist(lapply(trackList, createCollisionDataset))
# 
# 
# # Création des clusters
# cl <- makeCluster(detectCores())
# clusterExport(cl,getCollisionDataset)
# 
# clusterEvalQ(cl, source("code/analysis/conflictAnalyse/04_collisionRiskForall.R"))
# save3 <- parLapply(cl, 1:10, f <- function(x){getCollisionDataset(nbFrame=x, rId=7,trajectoriesDataset = trajectoriesDataset)})
# stopCluster(cl)
# 
# 
# colnames(collisionDataset) <- c('trackA', 'trackB')
# collisionDataset <- cbind(frame=f, collisionDataset)
# collisionDataset <- cbind(collisionDataset, collisionRate=0)
# 
# 
# 
# # Sample trackList with three columns: recordingId, frame, trackId
# trackList <- data.table(
#   recordingId = c(1, 1, 1, 2, 2),
#   frame = c(1, 1, 2, 2, 2),
#   trackId = c(101, 102, 201, 202, 203)
# )
# 
# # Function to combine tracks pairwise by frame
# combineTracks <- function(trackList) {
#   combinations <- combn(unique(trackList$trackId), 2)
#   combinedTracks <- data.table(
#     recordingId = trackList$recordingId[1],
#     frame = trackList$frame[1],
#     trackId1 = combinations[1, ],
#     trackId2 = combinations[2, ]
#   )
#   return(combinedTracks)
# }
# 
# # Apply combineTracks function to each unique frame
# colDataset <- rbindlist(lapply(unique(trackList$frame), function(frame) {
#   combineTracks(trackList[frame == trackList$frame])
# }))
# 
# colDataset <- trackList[, .(trackId1 = V1, trackId2 = V2), by = frame][,.(recordingId = recordingId[1], frame, trackId1, trackId2)]
#  
