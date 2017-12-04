library(dplyr)

# load the dataset, but filter for breaking/entry and year 2003
load_filtered_dataset <- function()
{
  d <- read.csv("crime_in_vancouver.csv", 
                             header=T, stringsAsFactors = F)
  d <- dplyr::filter(d, grepl("Break", d$TYPE), YEAR==2003)
  return (d)
}

