plot(tracks$xCenter,tracks$yCenter)
df <- data.frame('x'=round(tracks$xCenter),'y'=round(tracks$yCenter))

df_counts <- table(df$x, df$y)

# create heatmap
heatmap(df_counts, Rowv = NA, Colv = NA, col = heat.colors(256), 
        margins = c(5, 5), main = "Heatmap of (x, y) occurrences")
