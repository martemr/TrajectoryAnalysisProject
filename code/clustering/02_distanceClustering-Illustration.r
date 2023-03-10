##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 22th
# Description : Recherche de clusters par distances entre courbes
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init

#==========================================
# Parametres
#==========================================
range = unique(trajectoriesDataset$trackId)
seuil = 300
distance='euclidian'

#==========================================
# Librairies
#==========================================
library(dplyr)
library(spatstat)
library(cluster)

#==========================================
# Functions
#==========================================

#==========================================
# Interpolate and re-sample
#==========================================
interpolationDf <- data.frame()
# for (tId in range)
# {
  #interpolationRange=seq(75, 175)
  interpolationRange=seq(0, 200)
  

  # Calcul de la fonction d'interpolation  
  x =unlist(trajectoriesDataset[trajectoriesDataset$trackId==7, "xCenter"])
  y =unlist(trajectoriesDataset[trajectoriesDataset$trackId==7, "yCenter"])
  x2=unlist(trajectoriesDataset[trajectoriesDataset$trackId==8, "xCenter"])
  y2=unlist(trajectoriesDataset[trajectoriesDataset$trackId==8, "yCenter"])
  
  fun <-
    smooth.spline(unlist(trajectoriesDataset[trajectoriesDataset$trackId==7, "xCenter"]),
                  unlist(trajectoriesDataset[trajectoriesDataset$trackId==7, "yCenter"])) %>%
    splinefun(method = "natural")
  
  fun2 <-
    smooth.spline(unlist(trajectoriesDataset[trajectoriesDataset$trackId==8, "xCenter"]),
                  unlist(trajectoriesDataset[trajectoriesDataset$trackId==8, "yCenter"])) %>%
    splinefun(method = "natural")
  
  # Tracé
  plot(NULL,xlim=xlim,ylim=ylim,axes=T,xlab="X",ylab="Y",main="Distance entre interpolations de courbes")
  lines(x,y)
  lines(x2,y2)
  xMin=max(min(x),min(x2))
  xMax=min(max(x),max(x2))
  x=interpolationRange
  x2=interpolationRange
  y=fun(interpolationRange)
  y2=fun2(interpolationRange)
  points(interpolationRange,fun(interpolationRange),col='red',pch=19)
  points(interpolationRange,fun2(interpolationRange),col='red',pch=19)
  for (ind in seq(xMin,xMax)){
  #for (ind in seq(1,(max(interpolationRange)))){
    segments(x[ind],y[ind],x2[ind],y2[ind],col='blue')
  }
  lines(x,x2[1:length(x)])
  
  # Ré-échantillonage
  interpolationDf= rbind(interpolationDf, 
                         fun(interpolationRange))
#}
rownames(interpolationDf)=range
colnames(interpolationDf)=interpolationRange

plot(NULL,xlim=xlim,ylim=ylim,axes=T,xlab="X",ylab="Y",main=PlotName)
t(interpolationDf)
points(range, t(interpolationDf))
lines(x,y)
