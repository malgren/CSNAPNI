#fertrates_data.r
#This script pulls the crop fertilization rates from a summary spreadsheet, cropfertrates.xlsx, and converts the values from lbs/acre to kg/km

if(print_tags == 1){
  print("CreateInputsSubs/fertrates_data.R")
}

read_file = 'RawData/cropfertrates.xlsx' #these areas are planted areas 
import_yrs6 = c("1997","2002","2007","2012","2017") #this file contains data from years, 1997-2017
read_sheet1 = 'Nfert'
read_sheet2 = 'Pfert'

data1 = as.matrix(read_excel(read_file, sheet = read_sheet1)) #nfert
data2 = as.matrix(read_excel(read_file, sheet = read_sheet2)) #pfert

data_range1 = data1[2:17,2:(length(import_yrs6)+1)] #exclude some cells
data_range2 = data2[2:17,2:(length(import_yrs6)+1)] #exclude some cells

Nfert_lbsperacre=data_range1[1:length(data_range1[,1]),]
Nfert=as.numeric(Nfert_lbsperacre) / array(lbsperkg,c(16,length(import_yrs6))) / array(km2peracre,c(16,length(import_yrs6))) #kg/km2
Pfert_lbsperacre=data_range2[1:length(data_range2[,1]),]
Pfert=as.numeric(Pfert_lbsperacre)/array(lbsperkg,c(16,length(import_yrs6)))/array(km2peracre,c(16,length(import_yrs6))) #kg/km2

#write files
write_name1 = paste('InputFiles/Nfert.txt')
write_name2 = paste('InputFiles/Pfert.txt')
write.table(Nfert, file = write_name1, sep = " ", row.names = FALSE, col.names = FALSE)
write.table(Pfert, file = write_name2, sep = " ", row.names = FALSE, col.names = FALSE)

#write keys
write_name1 = paste("InputFileKeys/Nfert_key.txt")
Nfert_key = array(" ", c(16+1,length(import_yrs6)+1))
Nfert_key = c("kgN/km2", import_yrs6) #row headings
write.table(Nfert_key, file = write_name1, sep = " ", row.names = FALSE, col.names = FALSE)
write_name2 = paste("InputFileKeys/Pfert_key.txt")
Pfert_key = array(" ", c(16+1,length(import_yrs6)+1))
Pfert_key = c("kgP2O5/km2", import_yrs6) #row headings
write.table(Pfert_key, file = write_name2, sep = " ", row.names = FALSE, col.names = FALSE)