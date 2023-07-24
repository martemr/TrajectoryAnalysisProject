# Init
source("~/TrajectoryAnalysisProject/code/00_startDay.R", echo=TRUE)

# 4 zones d'études
name='4_zones_vides'
jpeg(paste("./documentation/Illustrations/", name, ".jpg", sep=""), width = 1080, height = 720)
par(mfrow=c(2,2))
lapply(seq(1,4,1),drawEmptyPlot)
dev.off()

# 4 zones dans fichiers différents
par(mfrow=c(1,1))
createEmptyMap <- function(x){
  name=paste('Localisation_',x, sep='')
  jpeg(paste("./documentation/Illustrations/", name, ".jpg", sep=""), width = 1080, height = 720)
  drawEmptyPlot(x)
  dev.off()
}
lapply(seq(1,4,1),createEmptyMap)

# Idem trajectoires
name='4_zones_toutes_trajectoires'
jpeg(paste("./documentation/Illustrations/", name, ".jpg", sep=""), width = 1080, height = 720)
par(mfrow=c(2,2))
lapply(seq(1,4,1),drawTrajectories)
dev.off()

# Diagramme flux
createFlowMap <- function(x){
  name=paste('Flow_localisation_',x, sep='')
  jpeg(paste("./documentation/Illustrations/", name, ".jpg", sep=""), width = 1080, height = 720)
  flowDiagram(selectedClass='car',LocationId=x,clusterMeta, WithAnnotations=FALSE, allFlowOnOneGraph=TRUE)
  dev.off()
}
lapply(seq(1,4,1),createFlowMap)

# Toutes trajectoires
par(mfrow=c(1,1))
createAllClassTraj <-function(x){
  for (cl in c('car','truck'))
  name=paste('Loc',x,'-Trajectoires',sep="")
  jpeg(paste("./documentation/Illustrations/", name, ".jpg", sep=""), width = 1080, height = 720)
  drawTrajectories(x,legend=T)
  dev.off()
}
lapply(seq(1,4,1),createAllClassTraj)

# Trajectoires par classe



name='4_zones_toutes_trajectoires'
jpeg(paste("./documentation/Illustrations/", name, ".jpg", sep=""), width = 1080, height = 720)
par(mfrow=c(2,2))
lapply(seq(1,4,1),drawTrajectories)
dev.off()
