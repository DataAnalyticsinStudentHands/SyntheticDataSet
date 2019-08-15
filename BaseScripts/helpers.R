library(tidyr)
library(dplyr)
library(sf)
library(stringr)


acs_race_codes <- c("A","B","C","D","E","F","G")
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

df <- marital_status_data_from_census #test throughout... I think the only ones that have 4 factors that aren't race are NA files
test <- df %>% 
  separate(concept,into=c('factor1','factor2','factor3','factor4','factor5'),sep = c(" BY | FOR ")) %>%
  #  rename_at(.,c('factor2'),list( ~str_replace(tolower(min(unique(.['factor2']))),' ','_'))) #not sure why it's fighting me
  rowwise() %>%
  mutate(factor1_name=str_replace_all(tolower(min(unique(.$'factor1'),na.rm = T)),' ','_'),
         factor2_name=str_replace_all(tolower(min(unique(.$'factor2'),na.rm = T)),' ','_'),
         factor3_name=str_replace_all(tolower(min(unique(.$'factor3'),na.rm = T)),' ','_'),
         str_length = length(str_split(label,'!!')[[1]]),
         str_length2 = if_else(str_length>5,as.numeric(str_length),as.numeric(5)),
         factor1 = str_replace_all(tolower(str_split(label,'!!')[[1]][str_length2-2]),' ','_'),
         factor2 = str_replace_all(tolower(str_split(label,'!!')[[1]][str_length2-1]),' ','_'),
         factor3 = str_replace_all(tolower(str_split(label,'!!')[[1]][str_length2]),' ','_'),
         race = case_when(max(nchar(name))==12 ~ substr(name,7,7),
                          TRUE ~ 'none')
         ) # %>%
  
  
test2 <- df %>%
  separate(concept,into=c('factor1','factor2','factor3','factor4','factor5'),sep = c(" BY | FOR ")) %>%
  rowwise() %>%
  mutate(
    factor5=
      if_else(
        is.na(factor5) & !is.na(str_split(label,'!!')[[1]][7]), str_replace_all(tolower(str_split(label,'!!')[[1]][7]),' ','_'),'no_label'
    ),
    factor5_name=
      if_else(
        is.na(factor5), 'none', str_replace_all(tolower(min(unique(.$'factor5'),na.rm = T)),' ','_')
    ),
    factor4=
      if_else(
        is.na(factor4) & !is.na(str_split(label,'!!')[[1]][6]), str_replace_all(tolower(str_split(label,'!!')[[1]][6]),' ','_'),
        if_else( #nested on condition neg
          !is.na(factor3) & !is.na(str_split(label,'!!')[[1]][5]),str_replace_all(tolower(str_split(label,'!!')[[1]][5]),' ','_'),'no_label'
        )
      ),
    factor4_name=
      if_else(
        is.na(factor4), 'none', str_replace_all(tolower(min(unique(.$'factor4'),na.rm = T)),' ','_')
      )
  )
  #tell if it has a race, get totals by each and then do the separation with the uncount



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

