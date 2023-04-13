library(onlineforecast)
library(graphics)


# Définition du tableau des incertitudes
incertitudes <- data.table(class=unique(trajectoriesDataset$class), 
                           xPosition=NA, yPosition=NA,
                           xSpeed=NA, ySpeed=NA, 
                           xAcc=NA, yAcc=NA)

computeVarianceArray <- function(trajectoriesDataset = trajectoriesDataset){
  trajectoriesDataset[, .(xPosition=var(xCenter) ,
                          yPosition=var(yCenter) ,
                          xSpeed   =var(xVelocity) , 
                          ySpeed   =var(yVelocity) , 
                          xAcc     =var(xAcceleration) , 
                          yAcc     =var(yAcceleration)), by=c('trackId', 'class')]
}

plotVarianceArray <- function(class='car'){
  variancesArray <- computeVarianceArray(trajectoriesDataset)
  
  arrondis=c(0,0,1,1,2,2)
  count=1
  par(mfrow=c(3,2))
  for (v in names(variancesArray[,3:8])){# c('xCenter', 'yCenter', 'xVelocity', 'yVelocity', 'xAcceleration', 'yAcceleration')){
    arr=arrondis[count]
    # Affichage des variances
    plot(table(round(variancesArray[class==class, ..v],digits=arr)),
         xlab="Variance (m)", ylab="Occurences", 
         main=v)
    # Affichage des percentiles
    percentiles <- data.table(x=quantile(unlist(round(variancesArray[class==class, ..v],digits=arr)),probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1)),
                              y=c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1)*max(table(round(variancesArray[class==class, ..v],digits = arr))))
    stairs(percentiles$x,percentiles$y, type="a", col='red')
    count=count+1
  }
}

getVariancePercentile <- function(class='car', studiedVariable='xPosition', percentile=.90, digits=0){
  computeVarianceArray(trajectoriesDataset)
  quantile(unlist(round(variancesArray[class==class, ..studiedVariable],digits=digits)),probs = c(percentile))
}
