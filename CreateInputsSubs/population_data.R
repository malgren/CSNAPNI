#population_data.r
#This script pulls interpolated county populations (3111 counties from the continental US) for 1987-2012 from the NANI Accounting Tool V3.1.0
read_file = 'RawData/NANI_Accounting_Tool_V3.1.0_People_Areas.xlsx'
import_yrs = c(1997,2002,2007,2012,2017)  #data years to import
read_sheet = 'People'
data = as.matrix(read_excel(read_file, sheet = read_sheet))

#need columns 33 (2012 & 2017), 34 (2007), 35 (2002), 36 (1997)
populationdenscty=data[,33:36]
populationcty=array(0,c(n_cnty,length(import_yrs)))
populationws=array(0,c(n_ws_tbx,length(import_yrs)))
populationdensws=array(0,c(n_ws_tbx,length(import_yrs)))
for(i in 1:length(import_yrs)){
  #if the year is closest to 2012, use 2012 population
  if(abs(import_yrs[i]-2012)<abs(import_yrs[i]-2007) && abs(import_yrs[i]-2012)<abs(import_yrs[i]-2002) && 
     abs(import_yrs[i]-2012)<abs(import_yrs[i]-1997) && abs(import_yrs[i]-2012)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-2012)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[3:3113,1])*areakm2_cnty
  }
  #if the year is closest to 2007, use 2007 population
  if(abs(import_yrs[i]-2007)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-2007)<abs(import_yrs[i]-2002) && 
     abs(import_yrs[i]-2007)<abs(import_yrs[i]-1997) && abs(import_yrs[i]-2007)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-2007)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[3:3113,2])*areakm2_cnty
  }
  #if the year is closest to 2002, use 2002 population
  if(abs(import_yrs[i]-2002)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-2002)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-2002)<abs(import_yrs[i]-1997) && abs(import_yrs[i]-2002)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-2007)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[3:3113,3])*areakm2_cnty
  }
  #if the year is closest to 1997, use 1997 population
  if(abs(import_yrs[i]-1997)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-1997)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-1997)<abs(import_yrs[i]-2002) && abs(import_yrs[i]-1997)<abs(import_yrs[i]-1992) && 
     abs(import_yrs[i]-1997)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[3:3113,4])*areakm2_cnty
  }
  #if the year is closest to 1992, use 1992 population
  if(abs(import_yrs[i]-1992)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-1992)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-1992)<abs(import_yrs[i]-2002) && abs(import_yrs[i]-1992)<abs(import_yrs[i]-1997) && 
     abs(import_yrs[i]-1992)<abs(import_yrs[i]-1987)){
    populationcty[,i]=as.numeric(populationdenscty[3:3113,5])*areakm2_cnty
  }
  #if the year is closest to 1987, use 1987 population
  if(abs(import_yrs[i]-1987)<abs(import_yrs[i]-2012) && abs(import_yrs[i]-1987)<abs(import_yrs[i]-2007) && 
     abs(import_yrs[i]-1987)<abs(import_yrs[i]-2002) && abs(import_yrs[i]-1987)<abs(import_yrs[i]-1997) && 
     abs(import_yrs[i]-1987)<abs(import_yrs[i]-1992)){
    populationcty[,i]=as.numeric(populationdenscty[3:3113,6])*areakm2_cnty
  }
  populationws[,i]=populationcty[,i]%*%cnty_ws
  populationdensws[,i]=populationws[,i]/area
}

# write text files
write_name = "InputFiles/population.txt"
write.table(populationdensws, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/population_key.txt"
population_key = array(" ", c(length(populationdensws)+1,length(import_yrs)+1))
population_key[1,]=c(" ",import_yrs) #column headings
population_key[,1]=c("ws_num", 1:length(populationdensws)) #row headings
write.table(population_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)