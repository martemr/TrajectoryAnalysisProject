#cleanDataset(fps=1, simplifyMethod = "Frame")

colors=rainbow(1000)

getPolygonPrediction <- function(timeToPredict=3, toDraw=F, tId, f){
  predictionsPoints <- data.table()
  print(tId)
  pred <- predictKalmanPosition(tId,f-75, trajectoriesDataset[trackId==tId & frame<f-75,.(xCenter,yCenter,xVelocity,yVelocity,xAcceleration,yAcceleration)])
  pred <<- pred
  x=(as.data.table(pred[2]))$x
  y=(as.data.table(pred[2]))$y
  predictionsPoints <- data.table(x=x,y=y)
  
  for(row in seq(1,nrow(as.data.table(pred[2])))){
    xUncertaincy=unlist(as.data.table((unlist(pred[1], recursive=F))[row])[1,1])
    yUncertaincy=unlist(as.data.table((unlist(pred[1], recursive=F))[row])[2,2])
    
    predictionsPoints <- rbind(predictionsPoints,
                               data.table(x=x+xUncertaincy,y=y+yUncertaincy))
    predictionsPoints <- rbind(predictionsPoints,
                               data.table(x=x-xUncertaincy,y=y+yUncertaincy))
    predictionsPoints <- rbind(predictionsPoints,
                               data.table(x=x+xUncertaincy,y=y-yUncertaincy))
    predictionsPoints <- rbind(predictionsPoints,
                               data.table(x=x-xUncertaincy,y=y-yUncertaincy))
    
  }
  polygon <- concaveman(as.matrix(predictionsPoints, ncol=2), concavity = 10)
  
  if (toDraw){
    drawEmptyPlot("")
    points(predictionsPoints, col=colors[tId], pch=19)
    lines(polygon, col="blue")  
  }
  polygon
}


getCollisionRiskDataset <- function(frameRange=seq(0,25000), rId=0){
  count=0
  for (f in frameRange){
    tracksIdOnFrame = trajectoriesDataset[recordingId==rId & frame==f, trackId]
    if(length(tracksIdOnFrame)>1){ # On ne cherche que les interactions donc 2 frames min
      for (tId in tracksIdOnFrame){
        print(tId)
        if (nrow(trajectoriesDataset[recordingId==rId & trackId==tId & frame<f,])>=3){ # Possibilité de prédire à 3s.
          p <-getPolygonPrediction(tId=tId, f=f, toDraw = TRUE)
          
          # Calculer la prédiction pour chacun des points et leur chevauchement
        }
        print(f)
      }
    }
    #print(f)
    #readline()
  }
}

getCollisionRiskDataset(frameRange = 0:1000)










# 
# 
# 
# 
# createDatasetCollisionRisk <- function(trajectoriesDataset=trajectoriesDataset){
#   collisionRiskDataset <- data.table[]
#   
#   
#   
#   tracksOnCommonFrame <- trajectoriesDataset[, .(tracksId=list(trackId)), by=.(recordingId, frame)]
#   test <- tracksOnCommonFrame[lapply(tracksOnCommonFrame$tracksId,length)>1]
#   data.table(lapply(test$tracksId, unlist))
#   collisionRiskDataset <- data.table(t(combn(unique(trajectoriesDataset[recordingId==10,trackId]),2)))
#   collisionRiskDataset[.(count=1), on="V1"]
#   
#   cbind(collisionRiskDataset, count=countCommonFrames(collisionRiskDataset$V1,collisionRiskDataset$V2))
#   
#   lapply(collisionRiskDataset, countCommonFrames)
#   class(table(table(c(trajectoriesDataset[trackId==7,frame], trajectoriesDataset[trackId==8,frame]))>1)["TRUE"])
#   
#   
#   b=data.table(a=t[,test])
#   
# }
# 
# 
# 
# 
# countCommonFrames <- function(t1,t2){
#   print(
#     table(table(c(trajectoriesDataset[trackId==t1,frame], trajectoriesDataset[trackId==t2,frame]))>1)["TRUE"]
#   )
# }
# 
# for(r in seq(1,nrow(collisionRiskDataset))){
#   countCommonFrames(collisionRiskDataset[r,1], collisionRiskDataset[r,2])
# }
# 
# countCommonFrames(collisionRiskDataset$V1,collisionRiskDataset$V2)
# 
# trajectoriesDataset[,frame]
# 
# dt=data.table()
# for (f in trajectoriesDataset[1:1000,frame]){
#   dt <- rbind(dt,t(combn(trajectoriesDataset[frame==f,trackId],2)))
# }

