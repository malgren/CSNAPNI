# Script to extract and save state-level 1997-2017 ag census data from USDA NASS database

if(print_tags == 1){
  print("CreateInputsSubs/NASSpull/NASS_states.R")
}

write(paste("Error Log:",Sys.time()), file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Error_Log.txt",
      append = FALSE, sep = "/n")

write("Names of NASS data files generated that are to be used in commodity calculations:", file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/State_FileNames.txt",
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
      #State-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('HAY - ACRES HARVESTED',
                                                            'HAY - PRODUCTION, MEASURED IN TONS')))){
      #for hay in 1997
      #State-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CATTLE, ON FEED - INVENTORY')))){
      #for cattle excluding cows and cattle on feed in 1997
      #1997 data does not exist in the CENSUS, only in the SURVEY
      #SURVEY data is comparable to census data in other years
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CATTLE, (EXCL COWS) - INVENTORY')))){
      #for cattle excluding cows (inventory data missing in 1997)
      #1997 data does not exist in the CENSUS, only in the SURVEY
      #however, the 1997 survey data for this commodity is 75% less on average
      #than other census years after that, so 2002 census data will be used
      data_year = 2002
      note = "_using2002"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CHICKENS, PULLETS, REPLACEMENT - INVENTORY')))){
      #for chicken pullets (inventory data missing in 1997)
      #use the 2002 CENSUS data
      data_year = 2002
      note = "_using2002"
    }else if((any(year[i]==c(2017)))&(query_desc[j]=='HOGS, BREEDING - INVENTORY')){
      #for breeding hogs (missing 2017)
      #use the 2012 CENSUS data
      data_year = 2012
      note = "_using2012"
    }
    #create a filename for each dataset that is the query name, but with special characters removed
    short_desc = str_replace_all(
      str_replace_all(
        str_replace_all(
          str_replace_all(
            str_replace_all(
              str_replace_all(
                query_desc[j], " ", "_"), ",", ""), "-", ""), "__", "_"), "\\(",""), "\\)","")
    file_name = paste("CreateInputsSubs/NASSpull/RawNASSData/StateData/",short_desc,'_',year[i],data_source,note,".txt",sep='')
    if (file.exists(file_name)){
      #print(paste("****************** already have:",file_name))
      write(file_name, file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/State_FileNames.txt",
            append = TRUE, sep = "/n")
    }else{
      # Specify the query parameters for crops
      params <- list(
        source_desc=data_source,
        agg_level_desc="STATE",
        domain_desc="TOTAL",
        short_desc=query_desc[j],
        year=data_year)
      
      # Get the data
      tryCatch({
        print(paste("****************** pulling data for:",file_name))
        d <- nassqs(params)
        states_list = unique(d$state_name) #need this to know which states we have data for so that a list can be made and data gaps can be filled
        #write data to a file
        write.table(d, file_name, append = FALSE, quote = TRUE, sep = " ",
                    eol = "\r", na = "0", dec = ".", row.names = FALSE,
                    col.names = TRUE, qmethod = c("escape", "double"),
                    fileEncoding = "")
        print(paste("****************** saved:",file_name))
        write(states_list, file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/StateNames.txt", append = TRUE, sep = "/n")
        write(file_name, file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/State_FileNames.txt",
              append = TRUE, sep = "/n")
      }, warning = function(w) {
        #warning-handler-code
      }, error = function(e) {
        #error-handler-code
        print(e)
        error_desc = paste(year[i],query_desc[j],data_source,e)
        write(error_desc, file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Error_Log.txt",
              append = TRUE, sep = "/n") 
      }, finally = {
        #cleanup-code
      })
    }
  }
}

#list exceptions
survey_not_census = c(list.files(path = "CreateInputsSubs/NASSpull/RawNASSData/StateData", pattern = "SURVEY", all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE,
                                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
other_year = c(list.files(path = "CreateInputsSubs/NASSpull/RawNASSData/StateData", pattern = "using", all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
#Write exceptions to file
#print("See 'StateData/Error_Log.txt' to review any errors that may have occurred.")
#print("See 'StateData/Exceptions.txt' for a list of substitutions made where census data was missing.")

write("Used survey data instead of census (state-level census was not available):", file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Exceptions.txt",
      append = FALSE, sep = "/n")
write(survey_not_census, file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Exceptions.txt",
      append = TRUE, sep = "/n")
write(" ", file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Exceptions.txt",
      append = TRUE, sep = "/n")
write("Used data from the nearest available year (data from desired year was not available):", file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Exceptions.txt",
      append = TRUE, sep = "/n")
write(other_year, file = "CreateInputsSubs/NASSpull/RawNASSData/StateData/Exceptions.txt",
      append = TRUE, sep = "/n")

#Get a list of all the data files
FileNames = readLines("CreateInputsSubs/NASSpull/RawNASSData/StateData/State_FileNames.txt")
state_commodities_list=gsub("CreateInputsSubs/NASSpull/RawNASSData/StateData/","",gsub(".txt","",FileNames[2:length(FileNames)]))
#Get a list of all the states from which we have data
NASS_states = data.frame(unique(readLines("CreateInputsSubs/NASSpull/RawNASSData/StateData/StateNames.txt")))
colnames(NASS_states) = "state_name"

n_commodities = length(state_commodities_list)/n_years
n_NASS_states = length(NASS_states$state_name)

#create a data frame to store all of the NASS county data joined with the NANI_NAPI counties
NASS_State_raw = data.frame(array(0,c(length(NASS_states$state_name),n_commodities*n_years)))
colnames(NASS_State_raw) = state_commodities_list
NASS_State_withheld = array(0,c(length(NASS_states$state_name),n_commodities,n_years))
sum_of_states = array(0,c(n_commodities,n_years))
states_gap = array(0,c(n_commodities,n_years))
states_gap_perc = array(0,c(n_commodities,n_years))

#for each year
for(j in 1:n_years){
  #Join raw commodity data to the state list (make sure the indices match and there are no duplicates) 
  #and add the resultant commodity array to the NASS State data frame
  #for each commodity in each year
  for(i in 1:n_commodities){  #iterates through the commodities
    correction=(i+(n_commodities)*(j-1))
    #read the file named in the ith position of the "Filenames" list
    temp = read.table(FileNames[correction+1], header = TRUE, sep = " ", quote = "\"'",
                      dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"))
    
    #remove duplicate state data (the reference period for the duplicates is not the full year)
    temp_nodups = temp[!duplicated(temp$state_name),]
    
    #merge the data in this file with the list of state names
    #temp_comp = merge(x = NASS_states, y = temp_nodups, by = "state_name", all.x = TRUE, sort = FALSE)
    
    #fill in the columns with the merged state data
    for(state in 1:n_NASS_states){
      #if there is a value for that state
      if(length(temp_nodups$Value[NASS_states$state_name[state] == temp_nodups$state_name])>0){
        NASS_State_raw[state,correction]=gsub(",","",temp_nodups$Value[NASS_states$state_name[state] == temp_nodups$state_name])
      }
    }
    
    #some values are NA
    #replace NAs with 0
    NASS_State_raw[is.na(NASS_State_raw[,correction]),correction] <- 0
    NASS_State_raw[grep("NA", NASS_State_raw[,correction], value = FALSE),correction] <- 0

    #(Z) entry means production is less than half the rounding unit
    #replace (Z)s with 0 **this is a good assumption
    NASS_State_raw[NASS_State_raw[,correction]=='                 (Z)',correction] <- 0


    #(D) entry means data is withheld to avoid disclosing data for individual operations
    #note which values have been withheld
    NASS_State_withheld[NASS_State_raw[,correction]=='                 (D)',i,j]= 1
    #then replace (D)s with 0 for gap calculation purposes
    #will be replaced with estimates later
    NASS_State_raw[NASS_State_raw[,correction]=='                 (D)',correction] <- 0
    
    #sum up all the state values for this commodity
    sum_of_states[i,j] = sum(as.numeric(NASS_State_raw[,correction]))
    
    #get the difference between the sum of state production values and the national prod value
    #for this commodity
    states_gap[i,j] = as.numeric(NASS_National[i,j])-sum_of_states[i,j]
    states_gap_perc[i,j] = states_gap[i,j]/as.numeric(NASS_National[i,j])*100
  }
}

#fill gaps in state data left by withheld entries
gapfill_var = "ag_land_cropland_acres"
ag_land_cropland_list = grep(gapfill_var, state_commodities_list, value = TRUE)
ag_land_cropland_areas = array(0,c(length(NASS_states$state_name),n_years))
ag_land_cropland_areas_in_gaps = array(0,c(length(NASS_states$state_name),n_commodities,n_years))
sum_ag_land_cropland_areas_in_gaps = array(0,c(n_commodities,n_years))
states_gapfactor = array(0,c(length(NASS_states$state_name),n_commodities,n_years))
NASS_State = data.frame(array(0,c(length(NASS_states$state_name),n_commodities*n_years)))
colnames(NASS_State) = state_commodities_list
sum_of_states_check = array(0,c(n_commodities,n_years))
states_gap_check = array(0,c(n_commodities,n_years))
states_gap_perc_check = array(0,c(n_commodities,n_years))

for(j in 1:n_years){
  ag_land_cropland_areas[,j] = as.numeric(NASS_State_raw[,ag_land_cropland_list[j]])
  for(i in 1:n_commodities){
    correction=(i+(n_commodities)*(j-1))
    #for each commodity, calculate the gapfilling factors
    ag_land_cropland_areas_in_gaps[,i,j] = NASS_State_withheld[,i,j]*ag_land_cropland_areas[,j]
    sum_ag_land_cropland_areas_in_gaps[i,j] = sum(ag_land_cropland_areas_in_gaps[,i,j])
    #each state should have a gap factor for each commodity in each year (0 if there was no gap)
    if(sum_ag_land_cropland_areas_in_gaps[i,j]>0){
    states_gapfactor[,i,j] = ag_land_cropland_areas_in_gaps[,i,j]/sum_ag_land_cropland_areas_in_gaps[i,j]
    }
    #Add states_gapfactor*states_gap to all NASS_State_raw entries 
    NASS_State[,correction] = as.numeric(NASS_State_raw[,correction])+states_gapfactor[,i,j]*states_gap[i,j]
    sum_of_states_check[i,j] = sum(NASS_State[,correction])
    states_gap_check[i,j] = as.numeric(NASS_National[i,j])-sum_of_states_check[i,j]
    states_gap_perc_check[i,j] = states_gap_check[i,j]/as.numeric(NASS_National[i,j])*100
  }
}

#print("Type 'View(NASS_State)' to see compiled compiled state commodity data.")

#write State data to a file
file_name = "CreateInputsSubs/NASSpull/RawNASSData/StateData/All_State_Data.txt"
write.table(NASS_State, file_name, append = FALSE, quote = TRUE, sep = " ",
          eol = "\r", na = "0", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")