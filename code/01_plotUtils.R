##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 07th
# Description : Fonctions pour le tracé de courbes
##---------------------------------------------

#==========================================
# Définition des paramètres
#==========================================
bgName <- sprintf("%s%02d_background.png", dosinit, RecordNumbers[1])

if(LocationId==1){
  fact= 10.2
} else if(LocationId==2){
  fact=10.2
} else if(LocationId==3){
  fact=10.2
} else if(LocationId==4){
  fact=6.5
}

# Lecture de l'image et des dimensions
bg_image <- readPNG(bgName)
xlim = c(0, dim(bg_image)[2]/fact)
ylim = c(-dim(bg_image)[1]/fact, 0)

#==========================================
# Functions
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
# Vectors
#==========================================
drawVector <- function(x,y,angle,size=10, col='blue'){
  angle = angle * (pi / 180)
  arrows(x0=x,
        x1=(x + size * cos(angle)),
        y0=y, 
        y1=(y + size * sin(angle)),
         lwd = 2, col = col)
}
