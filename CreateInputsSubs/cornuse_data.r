#This script pulls corn supply and disappearance and DDGS data from an excel file with data compiled from
#from https://www.ers.usda.gov/data-products/feed-grains-database/feed-grains-yearbook-tables.aspx
#and https://www.ers.usda.gov/data-products/us-bioenergy-statistics/us-bioenergy-statistics/#Supply and Disappearance
read_file = 'RawData/CornSupplyandDisapp-AllYears.xlsx' 
import_yrs = c("1997/98", "2002/03", "2007/08", "2012/13", "2017/18") #data years to import
read_sheet1 = 'SuppandDisap' #corn supply and disappearance (million bushels)
read_sheet2 = 'FoodIndUse' #corn for food, seed, and industrial use (million bushels)
read_sheet3 = 'DDGS' #Dried distillers grain with solubles: Supply and disappearance (million metric tons)
data1 = as.matrix(read_excel(read_file, sheet = read_sheet1))
data2 = as.matrix(read_excel(read_file, sheet = read_sheet2))
data3 = as.matrix(read_excel(read_file, sheet = read_sheet3))
year_col = 1
  
#Specify number and names of corn use categories
n_cornuses = 11 #number of corn use categories to import
name_row1 = 3 #specify the sheet row that contains corn use category names
name_row2 = 1 #specify the sheet row that contains corn use category names
name_row3 = 4 #specify the sheet row that contains corn use category names

#Specify data categories to extract
cornusename=array(" ", c(n_cornuses,1))
#sheet1
sheet1_ind = c(1,4,10,11,12)
cornusename[1] = 'Feed and residual use'
cornusename[4] = 'Seed use'
cornusename[10] = 'Exports'
cornusename[11] = 'Ending stocks'
cornusename[12] = 'Total supply 2/'
#sheet2
sheet2_ind = c(2,3,5,7,8,9)
cornusename[2] = 'Glucose and dextrose'
cornusename[3] = 'High-fructose corn syrup (HFCS)'
cornusename[5] = 'Alcohol for fuel' # need to split these using DDGS fraction calculated from sheet3
cornusename[7] = 'Alcohol for beverages and manufacturing'
cornusename[8] = 'Cereals and other products'
cornusename[9] = 'Starch'
#sheet3
sheet3_ind = 6
cornusename[6] = 'Ethanol plants' # this is for DDGS in M(metric)tons
Mton_Mbushel = 39.368 # Mbushels corn / Mmetric tons
starchpergrain = 0.72 # proportion of corn grain mass consumed in etoh production
  
#Allocate space to matrices
row1=array(0,c(length(import_yrs),1))
col1=array(0,c(length(sheet1_ind),1))
jump1=4 # number of rows to jump down from year row to get to yearly data
row2=array(0,c(length(import_yrs),1))
col2=array(0,c(length(sheet2_ind),1))
jump2=4
row3=array(0,c(length(import_yrs),1))
col3=array(0,c(length(sheet3_ind),1))
jump3=0
totalcornuse=array(0,c(length(import_yrs),1))
cornuse=array(0,c(length(cornusename)-1,length(import_yrs))) #proportion of corn disappearance to each product in each year

for(n in 1:length(import_yrs)){
  # find the row that corresponds to each year of interest
  row1[n]=which(data1[,year_col] %in% import_yrs[n]) #find the rows where the data for each year start
  row2[n]=which(data2[,year_col] %in% import_yrs[n]) #find the rows where the data for each year start
  if(import_yrs[n]=="1987/88"){ # there's no data available for DDGS in 1987
    row3[n]=which(data3[,year_col] %in% "1992/93") # load the data from 1992
  }else{
    row3[n]=which(data3[,year_col] %in% import_yrs[n]) #find the rows where the data for each year start
  }
  #get total corn supply data
  totalcol=which(data1[name_row1,] %in% cornusename[12])
  totalcornuse[n]=data1[row1[n]+jump1,totalcol]
  #find column numbers and assign cornuse values
  for(j in 1:(length(sheet1_ind)-1)){
    col1[j]=which(data1[name_row1,] %in% cornusename[sheet1_ind[j]])
    cornuse[sheet1_ind[j],n]=as.numeric(data1[row1[n]+jump1,col1[j]])/as.numeric(totalcornuse[n])
  }
  for(k in 1:length(sheet2_ind)){
    col2[k]=which(data2[name_row2,] %in% cornusename[sheet2_ind[k]])
    cornuse[sheet2_ind[k],n]=as.numeric(data2[row2[n]+jump2,col2[k]])/as.numeric(totalcornuse[n])
  }
  # assign values for distillers grains
  col3=which(data3[name_row3,] %in% cornusename[sheet3_ind])
  cornuse[sheet3_ind,n]=as.numeric(data3[row3[n]+jump3,col3])*Mton_Mbushel/(1-starchpergrain)/as.numeric(totalcornuse[n])
  cornuse[5,n]=cornuse[5,n]-cornuse[sheet3_ind,n]
}
cornusename[5] = "Wet milling ethanol"
cornusename[6] = "Dry milling ethanol"
# write text files
write_name = "InputFiles/cornuse.txt"
write.table(cornuse, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/cornuse_key.txt"
cornuse_key = array(" ", c(n_cornuses+1,length(import_yrs)+1))
cornuse_key[,1]=c(" ",cornusename[1:11])
cornuse_key[1,]=c(" ",import_yrs)
write.table(cornuse_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)