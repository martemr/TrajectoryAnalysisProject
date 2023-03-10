##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 09 th
# Description : ...
##---------------------------------------------

#==========================================
# Prerequisite
#==========================================
# Run 00_init

#==========================================
# Parametres
#==========================================

#==========================================
# Librairies
#==========================================

#==========================================
# Functions
#==========================================
drawEmptyPlot("Arrets de bus")
points(trajectoriesDataset[class=='car' & as.integer((sqrt(xVelocity^2+yVelocity^2))*3.6)==0,.(xCenter,yCenter)], col='green')
