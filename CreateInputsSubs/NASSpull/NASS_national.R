# Script to extract and save National-level 1997-2017 ag census data from USDA NASS database

if(print_tags == 1){
  print("CreateInputsSubs/NASSpull/NASS_national.R")
}

write(paste("Error Log:",Sys.time()), file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Error_Log.txt",
      append = FALSE, sep = "/n")

write("Names of NASS data files generated that are to be used in commodity calculations:", file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/National_FileNames.txt",
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
      #National-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('HAY - ACRES HARVESTED',
                                                           'HAY - PRODUCTION, MEASURED IN TONS')))){
      #for hay in 1997
      #National-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CATTLE, ON FEED - INVENTORY')))){
      #for cattle excluding cows and cattle on feed in 1997
      #National-level data does not exist in the CENSUS, only in the SURVEY
      data_source = "SURVEY"
    }else if((any(year[i]==c(1997)))&(any(query_desc[j]==c('CATTLE, (EXCL COWS) - INVENTORY')))){
      #for cattle excluding cows (inventory data missing in 1997)
      #National-level data does not exist in the CENSUS, only in the SURVEY
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
    file_name = paste("CreateInputsSubs/NASSpull/RawNASSData/NationalData/",short_desc,'_',year[i],data_source,note,".txt",sep='')
    if (file.exists(file_name)){
      #print(paste("****************** already have:",file_name))
      write(file_name, file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/National_FileNames.txt",
            append = TRUE, sep = "/n")
    }else{
      # Specify the query parameters for crops
      params <- list(
        source_desc=data_source,
        agg_level_desc="NATIONAL",
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
        write(file_name, file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/National_FileNames.txt",
              append = TRUE, sep = "/n")
      }, warning = function(w) {
        #warning-handler-code
      }, error = function(e) {
        #error-handler-code
        print(e)
        error_desc = paste(year[i],query_desc[j],data_source,e)
        write(error_desc, file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Error_Log.txt",
              append = TRUE, sep = "/n") 
      }, finally = {
        #cleanup-code
      })
    }
  }
}

#list exceptions
survey_not_census = c(list.files(path = "CreateInputsSubs/NASSpull/RawNASSData/NationalData", pattern = "SURVEY", all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE,
                                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
other_year = c(list.files(path = "CreateInputsSubs/NASSpull/RawNASSData/NationalData", pattern = "using", all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))
#Write exceptions to file
#print("See 'NationalData/Error_Log.txt' to review any errors that may have occurred.")
#print("See 'NationalData/Exceptions.txt' for a list of substitutions made where census data was missing.")

write("Used survey data instead of census (National-level census was not available):", file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Exceptions.txt",
      append = FALSE, sep = "/n")
write(survey_not_census, file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Exceptions.txt",
      append = TRUE, sep = "/n")
write(" ", file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Exceptions.txt",
      append = TRUE, sep = "/n")
write("Used data from the nearest available year (data from desired year was not available):", file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Exceptions.txt",
      append = TRUE, sep = "/n")
write(other_year, file = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/Exceptions.txt",
      append = TRUE, sep = "/n")

#Get a list of all the data files
FileNames = readLines("CreateInputsSubs/NASSpull/RawNASSData/NationalData/National_FileNames.txt")
nat_commodities_list=gsub("CreateInputsSubs/NASSpull/RawNASSData/NationalData/","",gsub(".txt","",FileNames[2:length(FileNames)]))

n_years = length(year)
n_commodities = length(nat_commodities_list)/n_years

#create an array to store all of the NASS county data joined with the NANI_NAPI counties
NASS_National = array(0,c(n_commodities,n_years))

#Pull national data from each file and add to the NASS National data frame
for(j in 1:n_years){
  for(i in 1:n_commodities){  #iterates through the commodities
    correction=(i+(n_commodities)*(j-1))
    temp = read.table(FileNames[correction+1], header = TRUE, sep = " ", quote = "\"'",
                      dec = ".", numerals = c("allow.loss", "warn.loss", "no.loss"))
    #remove duplicate state data (the reference period for the duplicates is not the full year)
    temp_nodups = temp[!duplicated(temp$country_name),]
    NASS_National[i,j]=gsub(",","",temp_nodups$Value)
  }
}

#print("Type 'View(NASS_National)' to see compiled national commodity data.")

#write National data to a file
file_name = "CreateInputsSubs/NASSpull/RawNASSData/NationalData/All_National_Data.txt"
write.table(NASS_National, file_name, append = FALSE, quote = TRUE, sep = " ",
          eol = "\r", na = "0", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "")