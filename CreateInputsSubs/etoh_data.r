#etoh_data.r
#This script loads and organizes ethanol production data from the ethanol supply and disappearance file from the
#USDA ERS U.S. Bioenergy Statistics page: https://www.ers.usda.gov/data-products/us-bioenergy-statistics/fuel 

etohproddata = t(array(scan("RawData/etohproddata.txt"), c(1,37))) #this file contains data from every year, 1999-2017, in 1000s of gallons, see etohproddata.xls
import_yrs5=c(1997,2002,2007,2012,2017)

etohprodselectyrs = array(0,length(import_yrs5))
for(n in 1:length(import_yrs5)){
  etohprodselectyrs[n]=etohproddata[import_yrs5[n]-1980]
}
etohprodallyears=etohproddata*literspergal*10^3
etohprod=etohprodselectyrs*literspergal*10^3

#write etohprod file
write_name = paste('InputFiles/etohprod.txt')
write.table(etohprod, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write etohprod key
write_name = paste("InputFileKeys/etohprod_key.txt")
etohprod_key = array(" ", c(length(import_yrs5)+1,1))
etohprod_key = c("liters", import_yrs5) #row headings
write.table(etohprod_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)