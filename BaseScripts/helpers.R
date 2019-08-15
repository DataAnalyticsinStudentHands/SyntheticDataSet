library(dplyr)
library(sf)

#df should have a variable called number_sams
#runs independently or inside a group_by, if appropriate totals
#input for factors: i.e., unique(sex_by_age_race_data$race); c("B" "G" "C" "D" "A" "F" "E"), which is not ordered...
#probs should equal 1, but is not nec. so. 
assign_factors_to_expand <- function(df, factors, factor_name){
  new_df <- df %>%
    mutate(total= max(number_sams), # df[which.max(number_sams)]
           prob = number_sams/total,
           base = if_else(prob==1,length(factors),as.integer(0))) %>%
    uncount(base,.remove=TRUE,.id = "sams_by_factor") # %>%
   # mutate(final_sams = case_when(.[[factor_name]] %in% factors ~ 44)) %>%  #later straight to number_sams
  #  filter(final_sams > 0) 
    return(new_df)
}

df <- marital_status_data_from_census
test <- df %>% 
  separate(concept,into=c('factor1','factor2','factor3'),sep = " BY ") %>%
  #  rename_at(.,c('factor2'),list( ~str_replace(tolower(min(unique(.['factor2']))),' ','_'))) #not sure why it's fighting me
  mutate(factor1_name=str_replace(tolower(min(unique(.$'factor1'))),' ','_'),
         factor2_name=str_replace(tolower(min(unique(.$'factor2'))),' ','_'),
         factor3_name=str_replace(tolower(str_split(max(unique(.$'factor3'),na.rm = T),' FOR ')[[1]][1]),' ','_'),
         factor1 = str_replace(tolower(str_split(label,'!!')[[1]][3]),' ','_'),
         factor2 = str_replace(tolower(str_split(label,'!!')[[1]][4]),' ','_'),
         factor3 = str_replace(tolower(str_split(label,'!!')[[1]][5]),' ','_'),
         race = case_when(max(nchar(name))==12 ~ substr(name,7,7),
                          TRUE ~ 'none')
         ) %>%
  
  #tell if it has a race, get totals by each and then do the separation with the uncount

#prep outside pipe
test <- separate(df,concept,into=c('factor1','factor2','factor3'),sep = " BY ")
newname <- str_replace(tolower(min(unique(test$factor2))),' ','_')


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

