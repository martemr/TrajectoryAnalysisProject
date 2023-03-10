##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 23th
# Description : Sauvegarde de tous les graphiques de trajectoires
##---------------------------------------------

#==========================================
# 
#==========================================
for (i in seq(1,24)){
  record_number=paste("0",i,sep="")
  
  # Run INIT
  source("~/Courbes_trajectoires/code/00_init.r", echo=FALSE)
  
  #png(paste(i,".png",sep=""))
  # RUN Trajectories
  source("~/Courbes_trajectoires/code/01_drawTrajectories.r", echo=FALSE)
  # Close device
  #dev.off()
}
