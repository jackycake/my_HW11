# these are the max/mins over all the breakins, covering all of Vancouver
max_lat <- 49.29723
min_lat <- 49.20091
max_long <- -123.0236
min_long <- -123.2199
center <- c((max_long + min_long)/2, (max_lat + min_lat)/2)

# load the dataset, but do this once, exists() can do this
if(!exists("d")) {
  print("loading dataset filtered for breakins and year 2003")
  d <- load_filtered_dataset()
} else {
  print("dataset already loaded!")
}

# test a tile
# keep only the Longitude and Latitude columns of d
events <- dplyr::select(d, Longitude, Latitude)
tile <- Tile(min_lat=49.21, max_lat=49.23, min_long=-123.2, max_long=-123.1, 
             events)

# CHECK : these lines check that the range of the test tile is correct (it is)
#tile_events <- get_events.Tile(tile)
# check that these prints fall within appropriate range (they do)
# print(min(tile_events$Longitude))
# print(max(tile_events$Longitude))
# print(min(tile_events$Latitude))
# print(max(tile_events$Latitude))

mt <- ManyTiles(min_lat, max_lat, min_long, max_long, 10, 5, events)

# CHECK : check that we have 50 tiles
#print(get_number_tiles.ManyTiles(mt))

# let's find out how many events in each tile
number_tiles <- get_number_tiles.ManyTiles(mt)
num_events <- rep(NA, number_tiles)
for (i in 1:number_tiles) {
  current_tile <- get_tile.ManyTiles(mt, i)
  current_events <- get_events.Tile(current_tile)
  num_events[i] <- nrow(current_events)
}
# and then find the index of the tile with least events and most
tile_ind_most <- which.max(num_events)
tile_ind_least <- which.min(num_events)

# let's use ggplot to plot these two tiles.  To use ggplot I need a data.frame.
# It will have two rows, for each tile, and specify its boundaries
tile1 <- get_tile.ManyTiles(mt, tile_ind_most)
tile2 <- get_tile.ManyTiles(mt, tile_ind_least)

# In ggplot, geom_rect draws rectangles, and its aesthetics are xmin, xmax,
# ymin, ymax. Let's write a function that creates a data.frame with 
# a single row for a tile
Tile_to_rectangle_df <- function(tile) {
  lat <- get_latitudes.Tile(tile)
  long <- get_longitudes.Tile(tile)
  return (data.frame(min_long=long[1], max_long=long[2], 
                     min_lat=lat[1], max_lat=lat[2], 
                     stringsAsFactors = F))
}

# now map our two tiles
tile_list <- list(tile1, tile2)
df_list <- as.list(rep(NA, 2))
for (i in 1:2) {
  df_list[[i]] <- Tile_to_rectangle_df(tile_list[[i]])
}
df <- do.call(rbind, df_list)

# CHECK: check that df looks good
print(df)

events1 <- get_events.Tile(tile1)
# FINALLY, let's see those tiles!
if (!exists("m")) {  # loading the map takes a while, so do it only once
  m <- get_googlemap("vancouver ca", zoom = 11)
}

p <- ggmap(m)
p <- p + geom_rect(mapping=aes(xmin=min_long, xmax=max_long,
                               ymin=min_lat, ymax=max_lat),
                    data=df, fill=NA, size=1, color="red",
                   inherit.aes=FALSE)
print(p)