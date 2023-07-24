library("parallel")



#==========================================
# Parallelisation
#==========================================
# Création des clusters
cl <- makeCluster(detectCores())

# Export environnement
clusterEvalQ(cl, {
  source("./code/00_startDay.R")
  source("./code/analysis/conflictAnalyse/01_interactionClassificationOptimise.R")
  dataset=trajectoriesDataset[recordingId==2,]
}) 

source("./code/00_startDay.R")
source("./code/analysis/conflictAnalyse/01_interactionClassificationOptimise.R")
dataset=trajectoriesDataset[recordingId==2,]

#i=0
#dataset=trajectoriesDataset[recordingId==i,]
#save <- parLapply(cl, unique(dataset$frame)[table(dataset$frame)>1],function(x) getAllInteractionOfFrame(x,dataset))
#allInteractions <- rbindlist(unlist(unlist(save,recursive = F),recursive = F))
#fwrite(allInteractions,file = paste('interactionDatasetRecording_',i,'.csv',sep=""))

# for (i in seq(1,32,1)){
#   dataset=trajectoriesDataset[recordingId==i,]
#   save <- parLapply(cl, unique(dataset$frame)[table(dataset$frame)>1],function(x) getAllInteractionOfFrame(x,dataset))
#   allInteractions <- rbindlist(unlist(unlist(save,recursive = F),recursive = F))
#   fwrite(allInteractions,file = paste('interactionDatasetRecording_',i,'.csv'))
# }

getAllInterractionsRecording <-function(i){
  dataset=trajectoriesDataset[recordingId==i,]
  save <- parLapply(cl, unique(dataset$frame)[table(dataset$frame)>1],function(x) getAllInteractionOfFrame(x,dataset))
  allInteractions <- rbindlist(unlist(unlist(save,recursive = F),recursive = F))
  fwrite(allInteractions,file = paste('interactionDatasetRecording_',i,'.csv',sep=""))
}

getAllInterractionsRecording(6)

#lapply(seq(6,32,1), getAllInterractionsRecording)
