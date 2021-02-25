# Script to extract and save county-level 1997-2017 ag census data from USDA NASS database

write(paste("Error Log:",Sys.time()), file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Error_Log.txt",
      append = FALSE, sep = "/n")

write("Names of NASS data files generated that are to be used in commodity calculations:", file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/County_FileNames.txt",
      append = FALSE, sep = "/n")

#pull data from each year
for (i in 1:length(year)){
  #for each commodity
  for (j in 1:length(query_desc)){
    data_source = default_data_source
    data_year = year[i]
    note = ""
    #SPECIAL CASES
    if((any(year[i]==c(2017,2012,2007)))&(query_desc[j]=='POTATOES - PRODUCTION, MEASURED IN CWT')){
      #for potato production in 2012 and 2017
      #county-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('HAY - ACRES HARVESTED',
                                                           'HAY - PRODUCTION, MEASURED IN TONS')))){
      #for hay in 1997
      #county-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CATTLE, (EXCL COWS) - INVENTORY',
                                                           'CATTLE, ON FEED - INVENTORY')))){
      #for cattle excluding cows and cattle on feed in 1997
      #county-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CHICKENS, PULLETS, REPLACEMENT - INVENTORY')))){
      #for chicken pullets (inventory data missing in 1997)
      #use the 2002 CENSUS data
      data_year = 2002
      note = "_using2002"
    }else if((any(year[i]==c(1997)))&(query_desc[j]=='EQUINE, HORSES & PONIES - INVENTORY')){
      #for horses (missing from 1987, 1992, and 1997)
      #use the 2002 CENSUS data
      data_year = 2002
      data_source = "CENSUS"
      note = "_using2002"  
    }else if((any(year[i]==c(2017)))&(query_desc[j]=='HOGS, BREEDING - INVENTORY')){
      #for breeding hogs (missing 2017)
      #use the 2012 CENSUS data
      data_year = 2012
      note = "_using2012"
    }else if((any(year[i]==c(1997)))&(query_desc[j]=='GOATS, ANGORA - INVENTORY')){
      #for angora goats (missing 1997)
      #use the 2002 CENSUS data
      data_year = 2002
      data_source = "CENSUS"
      note = "_using2002"
    }else if((any(year[i]==c(1997)))&(query_desc[j]=='GOATS, MILK - INVENTORY')){
      #for angora goats (missing 1997)
      #use the 2002 CENSUS data
      data_year = 2002
      data_source = "CENSUS"
      note = "_using2002"
    }
    #create a filename for each dataset that is the query name, but with special characters removed
    short_desc = str_replace_all(
      str_replace_all(
        str_replace_all(
          str_replace_all(
            str_replace_all(
              str_replace_all(
                query_desc[j], " ", "_"), ",", ""), "-", ""), "__", "_"), "\\(",""), "\\)","")
    file_name = paste("CreateInputsSubs/NASSpull/RawNASSData/CountyData/",short_desc,'_',year[i],data_source,note,".txt",sep='')
    if (file.exists(file_name)){
      #print(paste("****************** already have:",file_name))
      write(file_name, file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/County_FileNames.txt",
            append = TRUE, sep = "/n")
    }else{
      # Specify the query parameters for crops
      params <- list(
        source_desc=data_source,
        agg_level_desc="COUNTY",
        domain_desc="TOTAL",
        short_desc=query_desc[j],
        year=data_year)
      
      # Get the data
      tryCatch({
        print(paste("****************** pulling data for:",file_name))
        d <- nassqs(params)
        #write data to a file
        write.table(d, file_name, append = FALSE, quote = TRUE, sep = " ",
                    eol = "\r", na = "0", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"),
                    fileEncoding = "")
        print(paste("****************** saved:",file_name))
        write(file_name, file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/County_FileNames.txt",
              append = TRUE, sep = "/n")
      }, warning = function(w) {
        #warning-handler-code
      }, error = function(e) {
        #error-handler-code
        print(e)
        error_desc = paste(year[i],query_desc[j],data_source,e)
        write(error_desc, file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Error_Log.txt",
              append = TRUE, sep = "/n") 
      }, finally = {
        #cleanup-code
      })
    }
  }
}

#list exceptions
survey_not_census = c(list.files(path = "CreateInputsSubs/NASSpull/RawNASSData/CountyData", pattern = "SURVEY", all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE,
                                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
other_year = c(list.files(path = "CreateInputsSubs/NASSpull/RawNASSData/CountyData", pattern = "using", all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
#Write exceptions to file
#print("See 'CountyData/Error_Log.txt' to review any errors that may have occurred.")
#print("See 'CountyData/Exceptions.txt' for a list of substitutions made where census data was missing.")

write("Used survey data instead of census (county-level census was not available):", file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Exceptions.txt",
      append = FALSE, sep = "/n")
write(survey_not_census, file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Exceptions.txt",
      append = TRUE, sep = "/n")
write(" ", file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Exceptions.txt",
      append = TRUE, sep = "/n")
write("Used data from the nearest available year (data from desired year was not available):", file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Exceptions.txt",
      append = TRUE, sep = "/n")
write(other_year, file = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/Exceptions.txt",
      append = TRUE, sep = "/n")

#print("Joining NASS data to NANI_NAPI counties...")


#Get a list of all the data files
FileNames = readLines("CreateInputsSubs/NASSpull/RawNASSData/CountyData/County_FileNames.txt")
county_commodities_list=gsub("CreateInputsSubs/NASSpull/RawNASSData/CountyData/","",gsub(".txt","",FileNames[2:length(FileNames)]))
# Target is to get data for the
# full List of counties tracked in NANI_NAPI
cnty_IDs_NANI_NAPI = read.delim(file = "CreateInputsSubs/NASSpull/cnty_IDs_NANI_NAPI.txt", header = TRUE, sep = "\t")
NANI_NAPI_states = unique(cnty_IDs_NANI_NAPI$STATE)

n_commodities = length(state_commodities_list)/n_years

#create a data frame to store all of the NASS county data joined with the NANI_NAPI counties
NASS_County_raw = data.frame(array(0,c(length(cnty_IDs_NANI_NAPI[,1]),length(county_commodities_list))))
colnames(NASS_County_raw) = county_commodities_list
NASS_County_withheld = array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_commodities,n_years))
sum_of_counties = array(0,c(length(NANI_NAPI_states),n_commodities,n_years)) #sum of county-level productions within each state
counties_gap = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
counties_gap_perc = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))

#for each year
for(j in 1:n_years){
  #Join raw data to the NANI/NAPI counties and add the resultant array to the NASS County data frame
  #and add the resultant commodity array to the NASS State data frame
  #for each commodity in each year
  for(i in 1:n_commodities){  #iterates through the commodities
    correction=(i+(n_commodities)*(j-1))
    #read the file named in the ith position of the "Filenames" list
    temp = read.table(FileNames[correction+1], header = TRUE, sep = " ", quote = "\"'",
                      dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"))
    
    #create a new column with the county-specific FIPS code by combining state FIPS and county ansi codes
    temp$fipscty = as.integer(temp$state_fips_code, base = 0L)*1000+as.integer(temp$county_ansi, base = 0L)
    
    #sort data by county-specific FIPS code
    temp_sorted = temp[order(temp$fipscty),]
    
    #merge the data in this file with the list of NANI/NAPI counties (continental US)
    temp_comp = merge(x = cnty_IDs_NANI_NAPI, y = temp, by.x = "FIPS", by.y = "fipscty", all.x = TRUE)
    
    #code to probe values for a commodity in a particular year
    if(i==3){
      if(j==1){
        test=temp_comp
      }
    }
    
    #fill in the columns with the merged county data
    NASS_County_raw[,correction]=gsub(",","",temp_comp$Value)
    
    #some values are NA
    #replace NAs with 0
    NASS_County_raw[is.na(NASS_County_raw[,correction]),correction] <- 0
    NASS_County_raw[grep("NA", NASS_County_raw[,correction], value = FALSE),correction] <- 0
    
    #(Z) entry means production is less than half the rounding unit
    #replace (Z)s with 0 **this is a good assumption
    NASS_County_raw[NASS_County_raw[,correction]=='                 (Z)',correction] <- 0
    
    #(D) entry means data is withheld to avoid disclosing data for individual operations
    #note which values have been withheld
    NASS_County_withheld[grep("D", NASS_County_raw[,correction], value = FALSE),i,j]= 1
    #then replace (D)s with 0 for gap calculation purposes
    #will be replaced with estimates later
    NASS_County_raw[grep("D", NASS_County_raw[,correction], value = FALSE),correction] <- 0
    
    #for each state
    for(s in 1:length(NANI_NAPI_states)){
      #sum the values for all counties in each state included in the NANI_NAPI model
      #Hawaii and Alaska are not included in the NANI_NAPI model      
      #DC values will be zero since there are none in the NASS data
      #do the calculation for state s
      temp_indices_of_s = which(temp_comp$STATE == NANI_NAPI_states[s])
      #but only do it if the temp_comp array has data from state s
      if(length(NASS_County_raw[temp_indices_of_s,correction])>0){
        #sum production in counties in state s
        sum_of_counties[s,i,j] = sum(as.numeric(NASS_County_raw[temp_indices_of_s,correction]))
        #define the position of state s in the NASS list
        NASS_index_of_s = which(toupper(NANI_NAPI_states[s])==NASS_states$state_name)
        #if s is in the NASS list (in other words, if s is not DC)
        if(length(NASS_index_of_s)>0){
          #get the difference between the sum of state production values and the national prod value
          #for this commodity
          counties_gap[s,i,j] = as.numeric(NASS_State[NASS_index_of_s,correction])-sum_of_counties[s,i,j]
          #calculate the gap perc if the denominator>0
          if(as.numeric(NASS_State[NASS_index_of_s,correction])>0){
            counties_gap_perc[s,i,j] = counties_gap[s,i,j]/as.numeric(NASS_State[NASS_index_of_s,correction])*100
          }
          #It seems that not all of the data gaps are the result of counties reported with "(D)" in place of the
          #production quantity. Some counties are not reported on at all. I think some of these do indeed have no production
          #but, if the state totals are accurate, then there is missing county-level data in some states.
          #In some cases where state-level totals are withheld, no county-level data (even data withholding) is reported.
          #In other cases state-level totals are just much greater than the sum of all county-level production in a state,
          #there are many counties with no data, but no county-level data is marked as withheld.
          #To address this, all gaps greater than 1% between state-level and county-level data will be filled, 
          #but only for gaps that still exist after the first round of gapfilling.

          # #Therefore, if the state's data was withheld
          # if(NASS_State_withheld[NASS_index_of_s,i,j]){
          #   #make sure each of the counties in that state with '0' production are marked as having had their data withheld
          #   for(k in temp_indices_of_s){
          #     if(as.numeric(NASS_County_raw[k,i,j])==0){
          #       NASS_County_withheld[k,i,j] = 1
          #     }
          #   }
          # }
        }
      }
    }
  }
}

#fill gaps in state data left by entries marked as withheld
ag_land_cropland_list = grep("ag_land_cropland_acres", county_commodities_list, value = TRUE)
ag_land_cropland_areas = array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_years))
ag_land_cropland_areas_in_gaps = array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_commodities,n_years))
sum_ag_land_cropland_areas_in_gaps = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
counties_gapfactor = array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_commodities,n_years))
NASS_County = data.frame(array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_commodities*n_years)))
colnames(NASS_County) = county_commodities_list
sum_of_counties_check = array(0,c(length(NANI_NAPI_states),n_commodities,n_years)) #sum of county-level productions within each state
counties_gap_check = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
counties_gap_perc_check = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
gaps_filled = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))

#fill gaps in state data left by entries assumed to be withheld
ag_land_cropland_areas_in_gaps2 = array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_commodities,n_years))
sum_ag_land_cropland_areas_in_gaps2 = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
counties_gapfactor2 = array(0,c(length(cnty_IDs_NANI_NAPI[,1]),n_commodities,n_years))
sum_of_counties_check2 = array(0,c(length(NANI_NAPI_states),n_commodities,n_years)) #sum of county-level productions within each state
counties_gap_check2 = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
counties_gap_perc_check2 = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))
gaps_filled2 = array(0,c(length(NANI_NAPI_states),n_commodities,n_years))

for(j in 1:n_years){
  ag_land_cropland_areas[,j] = as.numeric(NASS_County_raw[,ag_land_cropland_list[j]])
  for(i in 1:n_commodities){
    correction=(i+(n_commodities)*(j-1))
    #for each commodity, calculate the gapfilling factors
    #need the ag land cropland in the county data gaps
    ag_land_cropland_areas_in_gaps[,i,j] = NASS_County_withheld[,i,j]*ag_land_cropland_areas[,j]
    #need the sum of ag land cropland in the county data gaps for each state
    for(s in 1:length(NANI_NAPI_states)){
      #sum the values for all counties in each state by
      #summing indices where cnty_IDs_NANI_NAPI$STATE == NANI_NAPI_states[s]
      NANI_NAPI_indices_of_s = which(cnty_IDs_NANI_NAPI$STATE == NANI_NAPI_states[s])
      sum_ag_land_cropland_areas_in_gaps[s,i,j] = sum(ag_land_cropland_areas_in_gaps[NANI_NAPI_indices_of_s,i,j])
      #calculate the gapsfactors for each commodity in each county in each year
      #where county-level data was withheld (the gapfactor remains 0 if there was no data withheld)
      #this has to be done one state at a time so that the correct state ag land totals can be used
      #do the calculation for state s
      #only calculate a gapfactor if sum_ag_land_cropland_areas_in_gaps is greater than 0
      if(sum_ag_land_cropland_areas_in_gaps[s,i,j]>0){
        counties_gapfactor[NANI_NAPI_indices_of_s,i,j] = 
          ag_land_cropland_areas_in_gaps[NANI_NAPI_indices_of_s,i,j]/sum_ag_land_cropland_areas_in_gaps[s,i,j]
        #this should be either 1 or 0 for each state+commodity combo
        #it indicates which commodities in which states have counties that had data withheld and, therefore, gaps filled
        gaps_filled[s,i,j] = sum(counties_gapfactor[NANI_NAPI_indices_of_s,i,j])
      }
      #Add counties_gapfactor*counties_gap to all NASS_County_raw entries
      NASS_County[NANI_NAPI_indices_of_s,correction] = as.numeric(NASS_County_raw[NANI_NAPI_indices_of_s,correction])+counties_gapfactor[NANI_NAPI_indices_of_s,i,j]*counties_gap[s,i,j]

      #sum the values for all counties in each state included in the NANI_NAPI model and in the NASS data
      #NOTE: DC is not in the NASS data, Hawaii and Alaska are not included in the NANI_NAPI model
      #define the position of state s in the NASS list
      NASS_index_of_s = which(toupper(NANI_NAPI_states[s])==NASS_states$state_name)
      #so, only do the calculation if this state is in the list of NASS states
      if(length(NASS_index_of_s)>0){
        sum_of_counties_check[s,i,j] = sum(as.numeric(NASS_County[NANI_NAPI_indices_of_s,correction]))
        #get the difference between the sum of state production values and the national prod value
        #for this commodity
        if(as.numeric(NASS_State[NASS_index_of_s,correction])>0){
          counties_gap_check[s,i,j] = as.numeric(NASS_State[NASS_index_of_s,correction])-sum_of_counties_check[s,i,j]
          counties_gap_perc_check[s,i,j] = counties_gap_check[s,i,j]/as.numeric(NASS_State[NASS_index_of_s,correction])*100
        }
      }
      
      
      #It seems that not all of the data gaps are the result of counties reported with "(D)" in place of the
      #production quantity. Some counties are not reported on at all. I think some of these do indeed have no production
      #but, if the state totals are accurate, then there is missing county-level data in some states.
      #In some cases where state-level totals are withheld, no county-level data (even data withholding) is reported and the gap is 100%.
      #In other cases state-level totals are just much greater (also up to 100%) than the sum of all county-level production in a state,
      #there are many counties with no data, and no county-level data is marked as withheld.
      #To address this, all remaining gaps greater than 1% between state-level and county-level data will be filled.
      
      #If the gap perc for state s, commodity i, in year j is still greater than 2% 
      if(counties_gap_perc_check[s,i,j]>2){
        #assume each of the counties in state s with '0' production have had data withheld (2=assumed withholding)
        for(k in NANI_NAPI_indices_of_s){
          if(as.numeric(NASS_County_raw[k,i,j])==0){
            NASS_County_withheld[k,i,j] = 2
          }
        }
      }
    }
    #for each commodity, re-calculate the gapfilling factors
    #need the ag land cropland in the county data gaps
    ag_land_cropland_areas_in_gaps2[,i,j] = (NASS_County_withheld[,i,j]==2)*ag_land_cropland_areas[,j]
    #need the sum of ag land cropland in the county data gaps for each state
    for(s in 1:length(NANI_NAPI_states)){
      #sum the values for all counties in each state by
      #summing indices where cnty_IDs_NANI_NAPI$STATE == NANI_NAPI_states[s]
      NANI_NAPI_indices_of_s = which(cnty_IDs_NANI_NAPI$STATE == NANI_NAPI_states[s])
      sum_ag_land_cropland_areas_in_gaps2[s,i,j] = sum(ag_land_cropland_areas_in_gaps2[NANI_NAPI_indices_of_s,i,j])
      #calculate the gapsfactors for each commodity in each county in each year
      #where county-level data was withheld (the gapfactor remains 0 if there was no data withheld)
      #this has to be done one state at a time so that the correct state ag land totals can be used
      #do the calculation for state s
      #only calculate a gapfactor if sum_ag_land_cropland_areas_in_gaps is greater than 0
      if(sum_ag_land_cropland_areas_in_gaps2[s,i,j]>0){
        counties_gapfactor2[NANI_NAPI_indices_of_s,i,j] = 
          ag_land_cropland_areas_in_gaps2[NANI_NAPI_indices_of_s,i,j]/sum_ag_land_cropland_areas_in_gaps2[s,i,j]
        #this should be either 1 or 0 for each state+commodity combo
        #it indicates which commodities in which states have counties that had data withheld and, therefore, gaps filled
        gaps_filled2[s,i,j] = sum(counties_gapfactor2[NANI_NAPI_indices_of_s,i,j])
      }
      #Add counties_gapfactor2*counties_gap2 to all NASS_County_raw entries
      NASS_County[NANI_NAPI_indices_of_s,correction] = as.numeric(NASS_County[NANI_NAPI_indices_of_s,correction])+counties_gapfactor2[NANI_NAPI_indices_of_s,i,j]*counties_gap_check[s,i,j]
      
      #sum the values for all counties in each state included in the NANI_NAPI model and in the NASS data
      #NOTE: DC is not in the NASS data, Hawaii and Alaska are not included in the NANI_NAPI model
      #define the position of state s in the NASS list
      NASS_index_of_s = which(toupper(NANI_NAPI_states[s])==NASS_states$state_name)
      #so, only do the calculation if this state is in the list of NASS states
      if(length(NASS_index_of_s)>0){
        sum_of_counties_check2[s,i,j] = sum(as.numeric(NASS_County[NANI_NAPI_indices_of_s,correction]))
        #get the difference between the sum of county production values and the state prod values
        #for this commodity
        if(as.numeric(NASS_State[NASS_index_of_s,correction])>0){
          counties_gap_check2[s,i,j] = as.numeric(NASS_State[NASS_index_of_s,correction])-sum_of_counties_check2[s,i,j]
          counties_gap_perc_check2[s,i,j] = counties_gap_check2[s,i,j]/as.numeric(NASS_State[NASS_index_of_s,correction])*100
        }
      }
      
      }
  }
}
# #find unfilled gaps
# counties_national_gap_check = array(0,c(n_commodities,n_years))
# counties_national_gap_perc_check = array(0,c(n_commodities,n_years))
# tolerance=2 #allowable +/- % difference between state-level and national-level totals, and the corresponding sums of county-level totals
# #find unfilled gaps in the county data with respect to the state data
# print("REMAINING COUNTY-STATE GAPS:")
# for(i in 1:52){
#   for(j in 1:5){
#     if(length(which(abs(counties_gap_perc_check2[,i,j])>tolerance))>0){
#       print("**********************")
#       print(c("product:",county_commodities_list[i]))
#       print(c("year:",year[j]))
#       print(c("states:", NANI_NAPI_states[which(abs(counties_gap_perc_check2[,i,j])>tolerance)]))
#       print(c("gaps:",counties_gap_perc_check2[which(abs(counties_gap_perc_check2[,i,j])>tolerance),i,j]))
#       print("**********************")
#     }
#   }
# }
# #find unfilled gaps in the county data with respect to the national data
# print("REMAINING COUNTY-NATIONAL GAPS:")
# for(i in 1:52){
#   for(j in 1:5){
#     counties_national_gap_check[i,j] = sum(counties_gap_check2[,i,j])
#     counties_national_gap_perc_check[i,j] = counties_national_gap_check[i,j]/as.numeric(NASS_National[i,j])*100
#     if(abs(counties_national_gap_perc_check[i,j])>tolerance){
#       print("**********************")
#       print(c("product:",county_commodities_list[i]))
#       print(c("year:",year[j]))
#       print(c("gap:",counties_national_gap_perc_check[i,j]))
#       print("**********************")
#     }
#   }
# }

#print("Type 'View(NASS_County)' to see compiled query data joined to the NANI_NAPI counties.")

#write County data to a file
file_name = "CreateInputsSubs/NASSpull/RawNASSData/CountyData/All_County_Data.txt"
write.table(NASS_County, file_name, append = FALSE, quote = TRUE, sep = " ",
        eol = "\r", na = "0", dec = ".", row.names = FALSE,
        col.names = TRUE, qmethod = c("escape", "double"),
        fileEncoding = "")
