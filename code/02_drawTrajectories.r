##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : February 2023, 20th
# Description : Draw trajectories
##---------------------------------------------


drawTrajectories <- function(AllTrajectoriesOnOneGraph = TRUE, StudiedClass='ALL'){
  classColors <- data.table(class=list('car','truck_bus','pedestrian','bicycle'),
                            color=list('red','pink','blue','green'))
  
  if(AllTrajectoriesOnOneGraph){
    drawEmptyPlot("All trajectories")
  }
  for (cl in unique(trajectoriesDataset$class))
  {
    if (!AllTrajectoriesOnOneGraph) drawEmptyPlot(paste("Trajectories of", cl))
    for (tId in unique(trajectoriesDataset[class == cl, trackId])) {
      lines(unlist(trajectoriesDataset[trackId == tId, xCenter]),
            unlist(trajectoriesDataset[trackId == tId, yCenter]),
            col = unlist(classColors[class == cl, color]))
    }
  }
}
