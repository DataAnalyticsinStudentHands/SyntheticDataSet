# These packages have to installed and loaded for the function to work
library(sf)
library(dplyr)

# This function uses the geometry column from the model to list the coordinates of the building used
add_lat_long <- function(sam){
  sam <- st_as_sf(sam, crs=3674)

  sam <- sam %>%
    ungroup() %>%
    mutate(
      NADcentroids = st_centroid(geometry),
      ptcoords = st_transform(NADcentroids,crs=4326),
    )

  sam$coords = st_coordinates(sam$ptcoords)
  
  return(sam)
}
