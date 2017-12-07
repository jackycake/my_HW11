library(dplyr)
library(ggmap)

source("data.R")
source("Tile.R")
source("ManyTile.R")

d_load_filtered_dataset <- function()
{
  d <- read.csv("crime_in_vancouver.csv", 
                header=T, stringsAsFactors = F)
  d <- dplyr::filter(d, grepl("Break", d$TYPE))
  return (d)
}

d <- d_load_filtered_dataset()

df_2003 <- d[d$YEAR==2003, ]
events_2003 <- dplyr::select(df_2003, Longitude, Latitude)

#setting parameters for Many Tiles Objects
max_lat <- 49.29723
min_lat <- 49.20091
max_long <- -123.0236
min_long <- -123.2199
center <- c((max_long + min_long)/2, (max_lat + min_lat)/2)

#creating ManyTiles objects for each year
mt_2003 <- ManyTiles(min_lat, max_lat, min_long, max_long, 10, 5, events_2003)

number_tiles_2003 <- get_number_tiles.ManyTiles(mt_2003)
num_events_2003 <- rep(NA, number_tiles_2003)
for (i in 1:number_tiles_2003) {
  current_tile <- get_tile.ManyTiles(mt_2003, i)
  current_events <- get_events.Tile(current_tile)
  num_events_2003[i] <- nrow(current_events)
}

tile_ind_1stmost_2003 <- which.max(num_events_2003)
tile_ind_2ndmost_2003 <- which(num_events_2003 == sort(num_events_2003,partial=n-1)[n-1])
tile_ind_3rdmost_2003 <- which(num_events_2003 == sort(num_events_2003,partial=n-2)[n-2])
tile_ind_4thmost_2003 <- which(num_events_2003 == sort(num_events_2003,partial=n-3)[n-3])
tile_ind_5thmost_2003 <- which(num_events_2003 == sort(num_events_2003,partial=n-4)[n-4])

#2003
Tile_to_rectangle_df <- function(tile) {
  lat <- get_latitudes.Tile(tile)
  long <- get_longitudes.Tile(tile)
  return (data.frame(min_long=long[1], max_long=long[2], 
                     min_lat=lat[1], max_lat=lat[2], 
                     stringsAsFactors = F))
}

tile_list_2003 <- list(tile1_2003, tile2_2003,tile3_2003,tile4_2003,tile5_2003)
df_list_2003 <- as.list(rep(NA, 5))
for (i in 1:5) {
  df_list_2003[[i]] <- Tile_to_rectangle_df(tile_list_2003[[i]])
}
finaldf_2003 <- do.call(rbind, df_list_2003)

events1_2003<-get_events.Tile(tile1_2003)
events2_2003<-get_events.Tile(tile2_2003)
events3_2003<-get_events.Tile(tile3_2003)
events4_2003<-get_events.Tile(tile4_2003)
events5_2003<-get_events.Tile(tile5_2003)
events_all_2003 <- dplyr::bind_rows(events1_2003, events2_2003, events3_2003, events4_2003,events5_2003)

num_events_tile1_2003<-nrow(tile1_2003$events)
num_events_tile2_2003<-nrow(tile2_2003$events)
num_events_tile3_2003<-nrow(tile3_2003$events)
num_events_tile4_2003<-nrow(tile4_2003$events)
num_events_tile5_2003<-nrow(tile5_2003$events)

m <- get_googlemap("vancouver ca", zoom = 12)
p <- ggmap(m)
p <- p + geom_rect(mapping=aes(xmin=min_long, xmax=max_long,
                               ymin=min_lat, ymax=max_lat),
                   data=finaldf_2003, fill=NA, size=1, color="red", inherit.aes=FALSE)

p<-p+geom_point(mapping=aes(x=Longitude,y=Latitude),color="blue",
                size=.5, data=events_all_2003)
p<-p + annotate("text", x = -123.1414, y = 49.27797, label = num_events_tile1_2003)
p<-p + annotate("text", x = -123.1414, y = 49.25870, label = num_events_tile2_2003)
p<-p + annotate("text", x = -123.1021, y = 49.27797, label = num_events_tile3_2003)
p<-p + annotate("text", x = -123.1806, y = 49.25870, label = num_events_tile4_2003)
p<-p + annotate("text", x = -123.1021, y = 49.25870, label = num_events_tile5_2003)

return(p)