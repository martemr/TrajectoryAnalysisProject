##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 07th
# Description : Fonctions pour le tracé de courbes
##---------------------------------------------

#==========================================
# Initialisation des paramêtres
#==========================================
initPlotImage <- function(LocationId){
  fact <- switch(LocationId, 
                 '1' = 10.2,
                 '2' = 10.2, 
                 '3' = 10.2, 
                 '4' = 6.5)
  bgName <<- switch(LocationId, 
                    '1' = sprintf("%s%02d_background.png", dosinit, 0), 
                    '2' = sprintf("%s%02d_background.png", dosinit, 7), 
                    '3' = sprintf("%s%02d_background.png", dosinit, 18), 
                    '4' = sprintf("%s%02d_background.png", dosinit, 30))
  
  bg_image <<- readPNG(bgName)
  xlim <<- c(0, dim(bg_image)[2]/fact)
  ylim <<- c(-dim(bg_image)[1]/fact, 0)
}

#==========================================
# Tracé d'un graphe vide
#==========================================
drawEmptyPlot <- function(PlotName,Background=TRUE){
  plot(NULL,xlim=xlim,ylim=ylim,axes=T,xlab="X",ylab="Y",main=PlotName)
  if(Background) {
    lim <- par()
    rasterImage(bg_image, 
                xleft=xlim[1], xright=xlim[2], 
                ybottom=ylim[1], ytop=ylim[2])
  }
}

#==========================================
# Tracé d'un vecteur
#==========================================
drawVector <- function(x,y,angle,size=10, col='blue'){
  angle = angle * (pi / 180)
  arrows(x0=x,
        x1=(x + size * cos(angle)),
        y0=y, 
        y1=(y + size * sin(angle)),
         lwd = 2, col = col)
}
