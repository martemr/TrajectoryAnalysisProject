

# On choisi une trajectoire
studiedTrackId=7

# On récupère toutes les trajectoires qui sont au même moment
concurentTracksId <- unique(trajectoriesDataset[frame %in% trajectoriesDataset[trackId==studiedTrackId,frame] &
                             recordingId %in% trajectoriesDataset[trackId==studiedTrackId,recordingId] & 
                             trackId != studiedTrackId
                             , trackId])

drawTrajectory(4, studiedTrackId, dosinit, col='green',newPlot = TRUE)
for (id in concurentTracksId) drawTrajectory(4, id, dosinit, col='red',newPlot = FALSE)

# On choisi un moment dans la trajectoire
# 10e frame
f=trajectoriesDataset[trackId==studiedTrackId,frame]
f=182
for (f in trajectoriesDataset[trackId==studiedTrackId,frame]){
lines(trajectoriesDataset[trackId %in% c(concurentTracksId[1], studiedTrackId) & 
                             frame==f, .(xCenter,yCenter)], col='orange', pch=19)
}

predictKalmanPlot(tId=7, trainingSize = 40)
# On prédit la trajectoire à 3 secondes
