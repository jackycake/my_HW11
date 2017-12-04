ManyTiles <- function(min_lat, max_lat, min_long, max_long,
                      nrow, ncol, events)
{
  lat_width <- (max_lat - min_lat)/nrow
  long_width <- (max_long - min_long)/ncol
  
  all_tiles <- as.list(rep(NA, nrow*ncol))
  
  counter <- 1
  for (i in 1:nrow) {
    for (j in 1:ncol) {
      tile_min_lat <- min_lat + (i-1)*lat_width
      tile_max_lat <- min_lat + i*lat_width
      tile_min_long <- min_long + (j-1)*long_width
      tile_max_long <- min_long + j*long_width
      current_tile <- Tile(tile_min_lat, tile_max_lat, 
                           tile_min_long, tile_max_long,
                           events)
      all_tiles[[counter]] <- current_tile
      counter <- counter + 1
    }
  }
  
  return (all_tiles)
}


get_number_tiles.ManyTiles <- function(mt)
{
  return (length(mt))
}

get_tile.ManyTiles <- function(mt, number)
{
  return (mt[[number]])
}
