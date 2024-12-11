#FOR LATER - 

source('Sam/get_tools.R')
library(stringr)
library(data.table)
maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusdir = paste0(maindir,"Census/") 
vintage = "2020"
state = "48" #48 Texas; 22 Louisiana
county = "*" 
tract = "*"
#you don't need a censuskey if you're not pulling new files down; you can only use this one if you have correct access to mine on the OneDrive
censuskey <- readLines(paste0(censusdir, "2017", "/key"))

#idea is to get wider detailed ethnicity and distribute
#https://api.census.gov/data/2020/dec/ddhca/variables.html
#https://api.census.gov/data/2020/dec/ddhcb/variables.html

#get all pop group labels for all the states (can do just one state at a time)
#https://api.census.gov/data/2020/dec/ddhca?get=NAME,POPGROUP,POPGROUP_LABEL&for=state:*
#gets total pop for each group for all of Texas
#https://api.census.gov/data/2020/dec/ddhca?get=NAME,T02003_001N,POPGROUP&POPGROUP_LABEL=*&for=state:48

#did preliminary stuff - you can't get tract level through gettract, but you can get state level... 

groupname <- "T02003" # SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS (Race/eth x 2) #total pop - group quarters
geo_type <- "supp"
api_type <- "dec/ddhcb"
path_suff <- "est"
supp_hhAge_data_from_census <- 
  census_supplemental_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)