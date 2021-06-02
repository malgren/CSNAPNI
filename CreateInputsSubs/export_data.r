# export_data.r
## Pulls crop export data from USDA NASS and GATS databases for estimation of domestically available crop quantities
## https://www.ers.usda.gov/data-products/feed-grains-database/feed-grains-yearbook-tables.aspx
## https://www.ers.usda.gov/data-products/us-bioenergy-statistics/us-bioenergy-statistics/#Supply and Disappearance

if(print_tags == 1){
  print("CreateInputsSubs/export_data.R")
}

read_file = 'RawData/CropExportData.xlsx'
import_yrs1 = c("1997/98","2002/03","2007/08","2012/13","2017/18")
import_yrs2 = c(1997,2002,2007,2012,2017) #for potato data and data from GATS

n_sheets = 18
range=array("",c(n_sheets,1))
yearindex=array(0,c(n_sheets,1))
exportindex=array(0,c(n_sheets,1))
n_export_crops=19
  
#define sheet names
read_sheet=array("",c(n_sheets,1))
read_sheet[1] = 'CornGrain' # (1000 bushels)
read_sheet[2] = 'CornTotal' # (1000 bushels)
read_sheet[3] = 'SorghumTotal' # (1000 bushels)
read_sheet[4] = 'SorghumGrain' # (metric tons)
read_sheet[5] = 'Wheat' # (million bushels)
read_sheet[6] = 'Oats' # (bushels)
read_sheet[7] = 'Barley' # (bushels)
read_sheet[8] = 'Soybeans' # (million bushels)
read_sheet[9] = 'Potatoes_fresh' # (million lbs farm weight)
read_sheet[10] = 'Potatoes_can' # (million lbs farm weight)
read_sheet[11] = 'Potatoes_chip' # (million lbs farm weight)
read_sheet[12] = 'Potatoes_dehy' # (million lbs farm weight)
read_sheet[13] = 'Potatoes_frz' # (million lbs farm weight)
read_sheet[14] = 'Rye' # (million bushels)
read_sheet[15] = 'Rice' # (100 million lbs)
read_sheet[16] = 'Hay' # kg
read_sheet[17] = 'Peanuts' # (million lbs farm weight)
read_sheet[18] = 'DGS' # (million metric tons) these exports may include some beverage distillers
#ignoring for CGF and CGM (production totals of these in "alcohol for fuel" are about 1% and less than 1% of DDGS production, respectively)

#define data ranges for each sheet
#sheet1
range[1] = 'A1:O32' #corn grain
range[2] = 'A1:O32' #corn total
range[3] = 'A1:O32' #sorghum total
range[4] = 'A1:AK13' #sorghum grain
range[5] = 'A1:M244' #wheat
range[6] = 'A1:O32' #oats
range[7] = 'A1:O32' #barley
range[8] = 'A1:L45' #soybeans
range[9] = 'A1:I52' #fresh potatoes
range[10] = 'A1:G52' #canned potatoes
range[11] = 'A1:G52' #chipped potatoes
range[12] = 'A1:G52' #dehydrated potatoes
range[13] = 'A1:I52' #frozen potatoes
range[14] = 'A1:M39' #rye
range[15] = 'A1:H57' #rice
range[16] = 'A1:C9' #hay (alfalfa and other)
range[17] = 'A1:M46' #peanuts
range[18] = 'A1:H32' #DDGS
#specify column or row that contains the data years
yearindex[1] = 2 #column
yearindex[2] = 2 #column
yearindex[3] = 2 #column
yearindex[4] = 8 #row
yearindex[5] = 1 #column
yearindex[6] = 2 #column
yearindex[7] = 2 #column
yearindex[8] = 1 #column
yearindex[9] = 1 #column
yearindex[10] = 1 #column
yearindex[11] = 1 #column
yearindex[12] = 1 #column
yearindex[13] = 1 #column
yearindex[14] = 1 #column
yearindex[15] = 1 #column
yearindex[16] = 1 #column
yearindex[17] = 1 #column
yearindex[18] = 1 #column
#specify column or row that contains the export data
exportindex[1] = 15 #column
exportindex[2] = 15 #column
exportindex[3] = 15 #column
exportindex[4] = 11 #row
exportindex[5] = 11 #column
exportindex[6] = 15 #column
exportindex[7] = 15 #column
exportindex[8] = 8 #column
exportindex[9] = 5 #column
exportindex[10] = 5 #column
exportindex[11] = 5 #column
exportindex[12] = 5 #column
exportindex[13] = 6 #column
exportindex[14] = 11 #column
exportindex[15] = 8 #column
exportindex_hay = c(2,3)
exportindex[16] = 8 #column
exportindex[17] = 6 #column

#specify the number of rows to jump between year indices and data of interest
#(yearly totals)
jumps_wheat = 4 #for data years after 1974

#load data (not doing this in a loop because all of the ranges are different dimensions)
data1 = as.matrix(read_excel(read_file, sheet = read_sheet[1]))
data2 = as.matrix(read_excel(read_file, sheet = read_sheet[2]))
data3 = as.matrix(read_excel(read_file, sheet = read_sheet[3]))
data4 = as.matrix(read_excel(read_file, sheet = read_sheet[4]))
data5 = as.matrix(read_excel(read_file, sheet = read_sheet[5]))
data6 = as.matrix(read_excel(read_file, sheet = read_sheet[6]))
data7 = as.matrix(read_excel(read_file, sheet = read_sheet[7]))
data8 = as.matrix(read_excel(read_file, sheet = read_sheet[8]))
data9 = as.matrix(read_excel(read_file, sheet = read_sheet[9]))
data10 = as.matrix(read_excel(read_file, sheet = read_sheet[10]))
data11 = as.matrix(read_excel(read_file, sheet = read_sheet[11]))
data12 = as.matrix(read_excel(read_file, sheet = read_sheet[12]))
data13 = as.matrix(read_excel(read_file, sheet = read_sheet[13]))
data14 = as.matrix(read_excel(read_file, sheet = read_sheet[14]))
data15 = as.matrix(read_excel(read_file, sheet = read_sheet[15]))
data16 = as.matrix(read_excel(read_file, sheet = read_sheet[16]))
data17 = as.matrix(read_excel(read_file, sheet = read_sheet[17]))
data18 = as.matrix(read_excel(read_file, sheet = read_sheet[18]))


#allocate space to matrices
ind=array(0,c(length(read_sheet),length(import_yrs1))) #indices corresponding to the data years selected for each
exports=array(0,c(n_export_crops,length(import_yrs1)))

for(y in 1:length(import_yrs1)){
  #find indices of data years
  ##the feed grains yearbooks don't have data that goes all the way back to 1987 for some crops
  if(import_yrs1[y]=='1987/88'){
    ind[1,y]=which(data1[,yearindex[1]] %in% '1989/90') #corn grain
    ind[2,y]=which(data2[,yearindex[2]] %in% '1989/90') #corn total
    ind[3,y]=which(data3[,yearindex[3]] %in% '1989/90') #sorghum total
    ind[4,y]=which(data4[yearindex[4],] %in% 1989) #sorghum grain
    ind[6,y]=which(data6[,yearindex[6]] %in% '1989/90') #oats
    ind[7,y]=which(data7[,yearindex[7]] %in% '1989/90') #barley
    ind[18,y]=which(data18[,yearindex[18]] %in% '1992/93') #DDGS
  } else {
    ind[1,y]=which(data1[,yearindex[1]] %in% import_yrs1[y]) #corn grain
    ind[2,y]=which(data2[,yearindex[2]] %in% import_yrs1[y]) #corn total
    ind[3,y]=which(data3[,yearindex[3]] %in% import_yrs1[y]) #sorghum total
    ind[4,y]=which(data4[yearindex[4],] %in% import_yrs2[y]) #sorghum grain
    ind[6,y]=which(data6[,yearindex[6]] %in% import_yrs1[y]) #oats
    ind[7,y]=which(data7[,yearindex[7]] %in% import_yrs1[y]) #barley
    ind[18,y]=which(data18[,yearindex[18]] %in% import_yrs1[y]) #DDGS
  }
  
  ind[5,y]=which(data5[,yearindex[5]] %in% import_yrs1[y]) #wheat
  ind[8,y]=which(data8[,yearindex[8]] %in% import_yrs2[y]) #soybeans
  ind[9,y]=which(data9[,yearindex[9]] %in% import_yrs2[y]) #potatoes fresh
  ind[10,y]=which(data10[,yearindex[10]] %in% import_yrs2[y]) #potatoes canned
  ind[11,y]=which(data11[,yearindex[11]] %in% import_yrs2[y]) #potatoes chipped
  ind[12,y]=which(data12[,yearindex[12]] %in% import_yrs2[y]) #potatoes dehydrated
  ind[13,y]=which(data13[,yearindex[13]] %in% import_yrs2[y]) #potatoes frozen
  ind[14,y]=which(data14[,yearindex[14]] %in% import_yrs1[y]) #rye
  ind[15,y]=which(data15[,yearindex[15]] %in% import_yrs1[y]) #rice
  ind[16,y]=which(data16[,yearindex[16]] %in% import_yrs2[y]) #hay
  ind[17,y]=which(data17[,yearindex[17]] %in% import_yrs1[y]) #peanuts

  exports[1,y]=as.numeric(data1[ind[1,y],exportindex[1]])*1000/bushelperton_corn*1000 #corn for grain (kg)
  exports[2,y]=as.numeric(data2[ind[2,y],exportindex[2]])*1000/bushelperton_corn*1000-exports[1,y] #corn for silage (corn total-corn grain)
  exports[3,y]=as.numeric(data5[ind[5,y]+jumps_wheat,exportindex[5]])*10^6/bushelperton_wheat*1000 #wheat (kg)
  exports[4,y]=as.numeric(data6[ind[6,y],exportindex[6]])/bushelperton_oats*1000 #oats (kg)
  exports[5,y]=as.numeric(data7[ind[7,y],exportindex[7]])/bushelperton_barley*1000 #barley (kg)
  exports[6,y]=as.numeric(data4[exportindex[4],ind[4,y]])*1000 #sorghum for grain (kg)
  exports[7,y]=as.numeric(data3[ind[3,y],exportindex[3]])*1000/bushelperton_sorghum*1000-exports[6,y] #sorghum for silage (kg)
  exports[8,y]=as.numeric(data9[ind[9,y],exportindex[9]])*10^6/lbsperkg+
                          as.numeric(data10[ind[10,y],exportindex[10]])*10^6/lbsperkg+
                                     as.numeric(data11[ind[11,y],exportindex[11]])*10^6/lbsperkg+
                                                as.numeric(data12[ind[12,y],exportindex[12]])*10^6/lbsperkg+
                                                           as.numeric(data13[ind[13,y],exportindex[13]])*10^6/lbsperkg #potatoes (kg)
  exports[9,y]=as.numeric(data14[ind[14,y],exportindex[14]])*10^6/bushelperton_rye*1000 #rye (kg)
  exports[10,y]=as.numeric(data16[ind[16,y],exportindex_hay[1]]) #alfalfa hay (kg)
  exports[11,y]=as.numeric(data16[ind[16,y],exportindex_hay[2]]) #other hay (kg)
  exports[12,y]=as.numeric(data8[ind[8,y],exportindex[8]])*10^6/bushelperton_soybeans*1000 #soybeans (kg)
  exports[13,y]=0 #no cropland pasture is exported
  exports[14,y]=0 #no noncropland pasture is exported
  exports[15,y]=as.numeric(data15[ind[15,y],exportindex[15]])*10^6 #rice (kg)
  exports[16,y]=as.numeric(data17[ind[17,y],exportindex[16]])*10^6/lbsperkg #peanuts (kg)
  exports[17,y]=0 #CGF
  exports[18,y]=0 #CGM
  exports[19,y]=as.numeric(data18[ind[18,y],exportindex[17]])*10^9 #DDGS (kg)

  #The following code is included to resolve discrepancies between USDA and GATS sorghum database (when
  #GATS exported sorghum grain>USDA total exported sorghum), which was used
  #to calculate quantity of sorghum silage exported
  for(u in 1:19){ #all crops
    if(exports[u,y]<0){ #if the export quantity is less than 0
      exports[u,y]=0 #make it 0
    }
  }
}

cropname=array("",c(n_crops))
cropname[1] = 'corn for grain'
cropname[2] = 'corn for silage'
cropname[3] = 'wheat'
cropname[4] = 'oats'
cropname[5] = 'barley'
cropname[6] = 'sorghum for grain'
cropname[7] = 'sorghum for silage'
cropname[8] = 'potatoes'
cropname[9] = 'rye'
cropname[10] = 'alfalfa hay'
cropname[11] = 'other hay'
cropname[12] = 'soybeans'
cropname[13] = 'cropland pasture'
cropname[14] = 'noncropland pasture'
cropname[15] = 'rice'
cropname[16] = 'peanuts'
cropname[17] = 'CGF'
cropname[18] = 'CGM'
cropname[19] = 'DGS'

#write exports to text file
write_name = "InputFiles/exports.txt"
write.table(exports, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/exports_key.txt"
exports_key = array(" ", c(length(cropname)+1,length(import_yrs2)+1))
exports_key[1,]=c(" ", import_yrs2) #column headings
exports_key[,1]=c(" ", cropname) #row headings
write.table(exports_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)