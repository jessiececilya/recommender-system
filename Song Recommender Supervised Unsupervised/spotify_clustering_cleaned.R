library(caret)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

spotify_df <- read.csv("./spotify_files/data.csv")


#1 valence - positivity rating
#2 year
#3 acousticness 
#4 artists
#5 danceability
#6 duration_ms
#7 energy
#8 explicit - categorical (0 and 1)
#9 id
#10 instrumentalness
#11 key - categorical
#12 liveness
#13 loudness
#14 mode - categorical (0 and 1)
#15 name
#16 popularity
#17 release_date
#18 speechiness
#19 tempo
spotify_df_copy <- data.frame(spotify_df)
#spotify_df_copy$key <- 1:nrow(spotify_df_copy)

spotify_df <- spotify_df[,c(-2,-4,-8,-9,-11,-14,-15,-17)]


rest_of_cols <- spotify_df_copy[,c(2,4,8,9,11,14,15,17)]

spotify_norm_values <- preProcess(spotify_df, method = c("center", "scale"))
spotify_df_norm <- predict(spotify_norm_values, spotify_df)

spotify_df_norm_copy <- data.frame(spotify_df_norm)
#spotify_df_norm_copy$key <- 1:nrow(spotify_df_norm_copy)

#add back the other rows and save for top_list_generator
#nrow(rest_of_cols)
#nrow(spotify_df_norm)

merged_df <- cbind(rest_of_cols,spotify_df_norm)
saveRDS(merged_df, "spotify_clusters_and_desc.rds")

set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
    kmeans(spotify_df_norm, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 20, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Is the knee bend at 3???????????????????????

km_3 <- kmeans(spotify_df_norm,3, nstart=10)

plot_1 <- fviz_cluster(km_3, geom = "point", data = spotify_df_norm) + ggtitle("k = 3")
plot_1

km_3
saveRDS(km_3, file="kmeans.rds")

#Category characteristics
#1 
#High Acoustic + instrumental,
#lower in positivity + danceability, energy, loudness
#possible category names?

#2
#High Danceability + Lively + lyrical
#Low in energy
#possible category names?

#3
#High in Positivity + danceablility, high energy, loud, popularity, high tempo
#low acoustic, low speechiness
#possible category names?


