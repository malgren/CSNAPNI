#NANI_NAPI_NASS_pull.r
# Script to extract and save raw 1987-2017 ag census data from USDA NASS database

if(print_tags == 1){
  print("CreateInputsSubs/NASSpull/NANI_NAPI_NASS_pull.R")
}

# Data to pull: 
# harvest and production data for
#(1) corn grain, (2) corn silage, (3) wheat, (4) oats,
#(5) barley, (6) sorghum grain, (7) sorghum silage, (8) potatoes,
#(9) rye, (10) alfalfa hay, (11) other hay, (12) soybeans,
#(13) cropland, (14) noncropland, (15) rice, (16) peanuts
# inventory data for
#(1) fattened cattle (2) milk cows, (3) hogs for breeding, (4) hogs for slaughter,
#(5) chicken layers, (6) breeding turkeys, (7) chicken pullets, (8) chicken broilers,
#(9) slaughter turkeys, (10) beef breeding herd, (11) beef calves, (12) dairy calves, 
#(13) beef heifers, (14) dairy heifers, (15) beef stockers, (16) dairy stockers, 
#(17) sheep, (18) horses, (17) goats

library(rnassqs)
library(stringr)

NASSQS_TOKEN="E737E9C9-A62A-3D17-9EBD-87FCCD7D5058"
nassqs_auth(key = NASSQS_TOKEN)

#define query variables
year <- c(1997,2002,2007,2012,2017) #removed 1987 and 1992 because only survey data was available in the quickstats database, 
                                    #and only for some commodities
n_years = length(year)

source("CreateInputsSubs/NASSpull/crop_query_data.r")
source("CreateInputsSubs/NASSpull/anim_query_data.r")
source("CreateInputsSubs/NASSpull/gap_filling_data.r")

query_desc = c(crop_desc,anim_desc,var_desc)
default_data_source = "CENSUS"

source("CreateInputsSubs/NASSpull/NASS_national.R") #pulls the national level data for the specified queries
source("CreateInputsSubs/NASSpull/NASS_states.R") #pulls the state level data and uses national level data and state ag land areas to fill anonymity gaps
source("CreateInputsSubs/NASSpull/NASS_counties.R") #pulls the county level data and uses state level data and county ag land areas to fill anonymity gaps


#use NASS data to calculate NANI/NAPI commodity categories
source('CreateInputsSubs/NASSpull/crop_calcs.r')
source('CreateInputsSubs/NASSpull/anim_calcs.r')

#print("Outputs (NASS_crop_area, NASS_crop_prod, NASS_anim) saved to 'InputFiles' folder.")

