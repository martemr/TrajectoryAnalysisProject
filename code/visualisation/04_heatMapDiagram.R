##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 14th
# Description : Zones d'exces de vitesse et d'arrets
##---------------------------------------------

#==========================================
# Prerequistes
#==========================================
# Run 00_Init.r
# Run 00_visualisationInit.r

#==========================================
# Librairies
#==========================================
library("lattice")
library("latticeExtra")

#==========================================
# Exemple d'usage
#==========================================
# test <-  trajectoriesDataset[class=='car',list(y=round(yCenter),x=round(xCenter))]
# matToPlot <- t(table(test))

#==========================================
# Ajoute un padding
#==========================================
addPadding <- function(matToPlot){
  # Crée la matrice de padding
  padMatrix <- matrix(0, 
                 nrow=abs(xlim[2]+1-xlim[1]), 
                 ncol=abs(ylim[2]+1-ylim[1]), 
                 dimnames=list(seq(xlim[1], xlim[2]), seq(ylim[1], ylim[2]))
  )
  
  # Fusionne les 2 matrices
  for (l in as.numeric(rownames(padMatrix))){
    for (c in as.numeric(colnames(padMatrix))){
      m <- matToPlot[as.numeric(rownames(matToPlot))==l, as.numeric(colnames(matToPlot))==c]
      if (length(m)>0){
        padMatrix[as.numeric(rownames(padMatrix))==l, as.numeric(colnames(padMatrix))==c] <- m #+ mat0[rownames(mat0)==l, colnames(mat0)==c]
      }
    }
  }
  
  # N'affiche pas les valeurs nulles
  padMatrix[padMatrix==0]<- NA
  padMatrix
}

#==========================================
# Affiche la heatmap
#==========================================
printHeatMapMatrix <- function(mat = padMatrix, alpha=0.5, name="Carte de chaleur",subTitle=""){
  trellis.par.set(regions = list(alpha = 0.5))
  p = levelplot(
    mat,
    col.regions = rev(heat.colors(20)),
    xlim = xlim,
    ylim = c(ylim[2], -ylim[1]),
    pretty = TRUE, 
    main=name, 
    xlab="X",
    ylab="Y", 
    at=seq(0,100,10),
    cuts=10, 
    sub=subTitle
  ) 
  #+ layer_(panel.2dsmoother(..., n = 200))
  plot(p + layer(grid.raster(as.raster(bg_image)), under = TRUE))
}

#==========================================
# Fonction globale
#==========================================
printHeatMap <- function(matToPlot){
  printHeatMapMatrix(addPadding(matToPlot))
}
