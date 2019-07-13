library(dplyr)
library(sf)

# This function numbers the rows in the model by powers of ten
one_of <- function(sam){
  sam <-  mutate(sam,
      one_of = case_when(
          1:n() %% 10000 == 0 ~ 10000,
          1:n() %% 1000 == 0 ~ 1000,
          1:n() %% 100 == 0 ~ 100,
          1:n() %% 10 == 0 ~ 10,
          TRUE ~ 1)
  )
  
  return(sam)
}


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

# This function converts the columns of Sam City to their appropriate class, which is either character or numeric
convertColumnTypes <- function(sam){
  # These columns should be characters
  sam[c(1:3, 5:7, 9:20, 23, 25, 33:34, 36:38, 41, 44, 47, 51:52, 56:57, 63:64, 80:81)] = sapply(sam[c(1:3, 5:7, 9:20, 23, 25, 33:34, 36:38, 41, 44, 47, 51:52, 56:57, 63:64, 80:81)], as.character)
  
  # These columns ahould be numeric
  sam[c(4, 8, 21:22, 24, 26:32, 35, 39:40, 42:43, 45:46, 48:50, 53:55, 58:62, 65:79, 82:87)] = sapply(sam[c(4, 8, 21:22, 24, 26:32, 35, 39:40, 42:43, 45:46, 48:50, 53:55, 58:62, 65:79, 82:87)], as.numeric)
  
  # Return the updated model
  return(sam)
}

