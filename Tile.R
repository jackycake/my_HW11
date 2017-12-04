#' Tile object constructor
#' 
#' @param min_lat minimum latitude for the tile region
#' @param events a data.frame with columns Latitude and
#' Longitude
Tile <- function(min_lat, max_lat, min_long, max_long,
                 events)
{
  events <- dplyr::filter(events, events$Longitude >= min_long,
                          events$Longitude <= max_long,
                          events$Latitude >= min_lat,
                          events$Latitude <= max_lat)
  
  out <- list(min_lat=min_lat, max_lat=max_lat,
              min_long=min_long, max_long=max_long,
              events=events)
  return (out)
}

get_events.Tile <- function(tile)
{
  return (tile$events)
}

get_latitudes.Tile <- function(tile)
{
  x <- c(tile$min_lat, tile$max_lat)
  return (x)
}

get_longitudes.Tile <- function(tile)
{
  x <- c(tile$min_long, tile$max_long)
  return (x)
}