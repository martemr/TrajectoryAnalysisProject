##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Draw trajectories
##---------------------------------------------

#==========================================
# Parametres
#==========================================
if(!exists("AllTrajectoriesOnOneGraph")) AllTrajectoriesOnOneGraph = TRUE
EraseOldGraphs = FALSE
ClassColored = TRUE
if(!exists("StudiedClass")) StudiedClass = 'car' # 'ALL' for all class


#==========================================
# Cleaning plot area
#==========================================
while (EraseOldGraphs & !is.null(dev.list()))  dev.off() 

#==========================================
# Drawing plot area
#==========================================
if(AllTrajectoriesOnOneGraph){
  drawEmptyPlot("All trajectories")
}

# Définition des couleurs par classe
if(ClassColored){
  classColors <- data.table(class=list('car','truck_bus','pedestrian','bicycle'),
                            color=list('red','pink','blue','green'))
}

#==========================================
# Draw trajectories
#==========================================
for (cl in unique(trajectoriesDataset$class))
{
  #print(cl)
  if(!AllTrajectoriesOnOneGraph) drawEmptyPlot(paste("Trajectories of",cl))
  for (tId in unique(trajectoriesDataset[class==cl,trackId])){
    lines(unlist(trajectoriesDataset[trackId==tId,xCenter]),
          unlist(trajectoriesDataset[trackId==tId,yCenter]),
          col=unlist(classColors[class==cl,color]))
  }
  #print(trajectoriesDataset[trajectoriesDataset$trackId==tId & trajectoriesDataset$class==StudiedClass,"yCenter"])
}

#==========================================
# Nettoyage dataset
#==========================================
rm(AllTrajectoriesOnOneGraph, cl, ClassColored, EraseOldGraphs)

