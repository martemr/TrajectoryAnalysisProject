


#==========================================
# Traitement et nettoyage des trajectoires
#==========================================
# Densités de distances
plot(NULL, main="Densité des distances", xlim=c(-5,300), ylim=c(0,0.030), xlab="Distance", ylab="Taux", )
lines(density(tracksMeta[trackId %in% distancesOktracks ,distance]), col='blue', lwd=2)
lines(density(tracksMeta[ ,distance]), col='red', lwd=2)
lines(c(5,5),c(0,0.030), lty=2, col='black', lwd=1.5)
legend(220,0.030, legend=c("Densité pre-traitement", "Densité post-traitement", "Limite 20m"),
       col=c("red", "blue", "black"), lty=c(1,1,2), cex=0.8)
n_distinct(tracks[trackId %in% distancesOktracks ,trackId])


# Densité de présence sur premieres et dernieres frames
drawEmptyPlot("Trajectoires incompletes")
points(tracks[trackId==1, .(xCenter, yCenter)], type="l", col='blue',lwd=2)
points(tracks[trackId==2, .(xCenter, yCenter)], type="l", col='blue',lwd=2)
n_distinct(tracks[trackId %in% notOnFirstLastFrames ,trackId])


tracks[trackId %in% distancesOktracks,]
trajectoriesDataset

test <- trajectoriesDataset
test <- test[tracksMeta[,.(distance,trackId)],on=.(trackId=trackId)]
plot(density(test$distance), main="Densité d'observations", lwd=2,ylim=c(0,0.18))
lines(c(5,5)  ,c(0,0.30), lty=2, col='blue',   lwd=2.5)
lines(c(10,10),c(0,0.30), lty=2, col='red',    lwd=2.5)
lines(c(20,20),c(0,0.30), lty=2, col='orange', lwd=2.5)
legend(170,0.15, legend=c("Seuil de 20m"),
       col=c("orange"), lty=2, cex=1.5, lwd=2)

x <- c(12480,260,860)
labels <- c("Traj. intéressantes", "Traj. incompletes", "Traj. immobiles")
colors=c("antiquewhite", "cadetblue", "chocolate")
pie(x,labels, col=colors, cex=2)

x <- c(1078846,4448153,8648185)
labels <- c("Obs. intéressantes", "Obs. incompletes", "Obs. immobiles")
colors=c("antiquewhite", "cadetblue", "chocolate")
pie(x,labels, col=colors, cex=2)

