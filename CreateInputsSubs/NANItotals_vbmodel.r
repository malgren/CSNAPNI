#NANItotals_vbmodel.r
#This script imports Excel data from the NANI model
#For 
#Atm_N_Dep in kg/km2/yr (column 1)
#Fert_N_App in kgN/km2/yr (columns 2, 3, 4)
#Ag_N_Fix in kgN/km2/yr (columns 5, 6, 7, 8, 9, 10, 11)
#Food_Feed_N in kgN/km2/yr (column 12)
#Non_Food_Crops in kgN/km2/yr (columns 13, 14, 15)

if(print_tags == 1){
  print("CreateInputsSubs/NANItotals_vbmodel.R")
}

read_file = 'RawData/NANIdata_extract.xlsx'
import_yrs3 = c("1997","2002","2007","2012","2017") #data years to import
import_yrs = c("97","02","07","12","17")  #years for file names
read_sheet1 = 'Atm_N_Dep'
read_sheet2 = 'Fert_N_App'
read_sheet3 = 'Ag_N_Fix'
read_sheet4 = 'Food_Feed_N'
read_sheet5 = 'Non_Food_Crops'

data1 = as.matrix(read_excel(read_file, sheet = read_sheet1))
data2 = as.matrix(read_excel(read_file, sheet = read_sheet2))
data3 = as.matrix(read_excel(read_file, sheet = read_sheet3))
data4 = as.matrix(read_excel(read_file, sheet = read_sheet4))
data5 = as.matrix(read_excel(read_file, sheet = read_sheet5))

data_range1 = data1[4:3114,2:5] #exclude some cells
data_range2 = data2[3:3114,2:19] #exclude some cells
data_range3 = data3[3:3114,2:43] #exclude some cells
data_range4 = data4[3:3114,2:7] #exclude some cells
data_range5 = data5[3:3114,2:19] #exclude some cells

ncolumns=15
NANIdatactydens=array(0,c(n_cnty,ncolumns,length(import_yrs3)))
NANIdatacty=array(0,c(n_cnty,ncolumns,length(import_yrs3)))
NANIdataws=array(0,c(n_ws_tbx,ncolumns,length(import_yrs3)))
NANIdatadensws=array(0,c(n_ws_tbx,ncolumns,length(import_yrs3)))

#Atm_N_Dep in kg/km2/yr (column 1)
ndepcols=1
index02=4
index06=3
index07=2
index08=1
for(n in 1:length(import_yrs3)){
  for(i in 1:ndepcols){
    if(as.double(import_yrs3[n])<2004){ #use the 2002 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[,index02]
    }
    if(as.double(import_yrs3[n])>2004 && as.double(import_yrs3[n])<2007){ #use the 2006 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[,index06]
    }
    if(as.double(import_yrs3[n])==2007){ #use the 2007 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[,index07]
    }
    if(as.double(import_yrs3[n])>2007){ #use the 2008 CMAQ
      NANIdatactydens[,ndepcols,n]=data_range1[,index08]
    }
    NANIdatacty[,ndepcols,n]=as.numeric(NANIdatactydens[,ndepcols,n])*areakm2_cnty
  }
}

#Fert_N_App in kgN/km2/yr (columns 2, 3, 4)
nfertcols=3
yr_cols=array(0,c(length(import_yrs3),nfertcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range2[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range2[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nfertcols){
    NANIdatactydens[,ndepcols+i,n]=data_range2[2:length(data_range2[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+i,n])*areakm2_cnty
  }
}

#Ag_N_Fix in kgN/km2/yr (columns 5, 6, 7, 8, 9, 10, 11)
nfixcols=7
yr_cols=array(0,c(length(import_yrs3),nfixcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range3[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range3[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nfixcols){
    NANIdatactydens[,ndepcols+nfertcols+i,n]=data_range3[2:length(data_range3[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+nfertcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+nfertcols+i,n])*areakm2_cnty
  }
}

#Food_Feed_N in kgN/km2/yr (column 12)
nffcols=1
yr_cols=array(0,c(length(import_yrs3),nffcols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range4[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range4[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nffcols){
    NANIdatactydens[,ndepcols+nfertcols+nfixcols+i,n]=data_range4[2:length(data_range4[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+nfertcols+nfixcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+nfertcols+nfixcols+i,n])*areakm2_cnty
  }
}

#Non_Food_Crops in kgN/km2/yr (columns 13, 14, 15)
nnfccols=3
yr_cols=array(0,c(length(import_yrs3),nnfccols))
for(n in 1:length(import_yrs3)){
  if(length(which(as.numeric(data_range5[1,]) %in% import_yrs3[n]))==0){
    yr_cols[n,] = yr_cols[n-1,] #if there is no data from year n use the data from the previous year
  }else{
    yr_cols[n,]=which(as.numeric(data_range5[1,]) %in% import_yrs3[n])
  }
  for(i in 1:nnfccols){
    NANIdatactydens[,ndepcols+nfertcols+nfixcols+nffcols+i,n]=data_range5[2:length(data_range5[,1]),yr_cols[n,i]]
    NANIdatacty[,ndepcols+nfertcols+nfixcols+nffcols+i,n]=as.numeric(NANIdatactydens[,ndepcols+nfertcols+nfixcols+nffcols+i,n])*areakm2_cnty
  }
  #watershed crop production
  NANIdataws[,,n] = t(cnty_ws)%*%NANIdatacty[,,n]
  for(c in 1:length(NANIdataws[1,,n])){ #for each column, divide by watershed area to get density
    NANIdatadensws[,c,n] = NANIdataws[,c,n]/area
  }
  #write text files
  write_name =paste("InputFiles/NANIdata",import_yrs[n],".txt",sep = "")
  write.table(NANIdatadensws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
#write key
write_name = paste("InputFileKeys/NANIdata_key.txt")
NANIdata_key = array(" ", c(n_ws_tbx+1,length(NANIdatadensws[1,,1])+1))
NANIdata_key[1,] = c(" ", data1[2,2],data2[2,2],data2[2,8],data2[2,14],
                     paste(data3[2,2], "fix"),paste(data3[2,8], "fix"),paste(data3[2,14], "fix"),paste(data3[2,20], "fix"),
                     paste(data3[2,26], "fix"),paste(data3[2,32], "fix"),paste(data3[2,38], "fix"),
                     data4[2,2],paste(data5[2,2], "nonfoodcropfert"),paste(data5[2,8], "fert"),paste(data5[2,14], "fert")) #column headings
NANIdata_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(NANIdata_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
