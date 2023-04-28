#---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : Calcul des paramêtres pour le filtre de kalman en fonction du jeu de données.
# ___________DEPRECATED___________
##---------------------------------------------

#==========================================
# Calcule la variance des variables de positions, vitesse et acceleration dans le dataset
#==========================================
computeVarianceArray <- function(trajectoriesDataset = trajectoriesDataset){
  trajectoriesDataset[, .(xPosition=var(xCenter) ,
                          yPosition=var(yCenter) ,
                          xSpeed   =var(xVelocity) , 
                          ySpeed   =var(yVelocity) , 
                          xAcc     =var(xAcceleration) , 
                          yAcc     =var(yAcceleration)), by=c('trackId', 'class')]
}

#==========================================
# Sert à la visualisation des variances
#==========================================
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


#==========================================
# Renvoie le percentile de la variance pour chaque variable de position, vitesse, acceleration de cette classe.
#==========================================
getVariancePercentile <- function(class='car', studiedVariable='xPosition', percentile=.90, digits=0){
  variancesArray <- computeVarianceArray(trajectoriesDataset)
  quantile(unlist(round(variancesArray[class==class, ..studiedVariable],digits=digits)),probs = c(percentile))
}

