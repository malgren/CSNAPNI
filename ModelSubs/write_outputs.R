# write_outputs.R
# this file writes some of the resulting calculations from CSNAPNI to text files

if(print_tags == 1){
  print("ModelSubs/write_outputs.R")
}

#OUTPUT MATRICES
NANIBtot_US = array(0,c(length(NANIBtot[1,,1]),nyrs))
NANIorigtot_US = array(0,c(length(NANIorigtot[1,,1]),nyrs))
NAPIBtot_US = array(0,c(length(NAPIBtot[1,,1]),nyrs))
NAPIorigtot_US = array(0,c(length(NAPIorigtot[1,,1]),nyrs))
NANI_wsspec = array(0,c(length(NANIBtot[1,,1]),nyrs))
NAPI_wsspec = array(0,c(length(NAPIBtot[1,,1]),nyrs))

for(n in 1:nyrs){
  NANIBtot_US[,n] = colSums(NANIBtot[,,n])/(10^9)
  NANIorigtot_US[,n] = colSums(NANIorigtot[,,n])/(10^9)
  NAPIBtot_US[,n] = colSums(NAPIBtot[,,n])/(10^9)
  NAPIorigtot_US[,n] = colSums(NAPIorigtot[,,n])/(10^9)
  if (length(ws)>1){
    NANI_wsspec[,n] = colSums(NANIBtot[ws,,n])/(10^9)
    NAPI_wsspec[,n] = colSums(NAPIBtot[ws,,n])/(10^9)
  } else {
    NANI_wsspec[,n] = NANIBtot[ws,,n]/(10^9)
    NAPI_wsspec[,n] = NAPIBtot[ws,,n]/(10^9)
  }
}

total_commoddisag_fertN = array(c(t(colSums(CfertNwswE)), t(colSums(etohfertNws))),c(nyrs,(n_crops+1)))/(10^9) #total disaggregated fert N inputs (10^9 kg N)
total_commoddisag_fertP = array(c(t(colSums(CfertPwswE)), t(colSums(etohfertPws))),c(nyrs,20))/(10^9) #total disaggregated fert P inputs (10^9 kg P)
total_commoddisag_fixN = colSums(CfixNwswE[ws,,])/10^9 #total commodity disaggregated fixation N inputs in billion kg N

if (length(ws)>1){
  total_commoddisag_fertN_ws = array(c(t(colSums(CfertNwswE[ws,,])), t(colSums(etohfertNws[ws,]))),c(nyrs,(n_crops+1)))/(10^9) #total disaggregated fert N inputs (10^9 kg N)
  total_commoddisag_fertP_ws = array(c(t(colSums(CfertPwswE[ws,,])), t(colSums(etohfertPws[ws,]))),c(nyrs,(n_crops+1)))/(10^9) #total disaggregated fert P inputs (10^9 kg P)
  total_commoddisag_fixN_ws = colSums(CfixNwswE[ws,,])/10^9 #total commodity disaggregated fixation N inputs in billion kg N
} else {
  total_commoddisag_fertN_ws = array(c(t(CfertNwswE[ws,,]), t(etohfertNws[ws,])),c(nyrs,(n_crops+1)))/(10^9) #total disaggregated fert N inputs (10^9 kg N)
  total_commoddisag_fertP_ws = array(c(t(CfertPwswE[ws,,]), t(etohfertPws[ws,])),c(nyrs,(n_crops+1)))/(10^9) #total disaggregated fert P inputs (10^9 kg P)
  total_commoddisag_fixN_ws = CfixNwswE[ws,,]/10^9 #total commodity disaggregated fixation N inputs in billion kg N
}

#total nutrition produced for people
#totMprot
#totMkcal
#totCprot
#totCkcal

write_name = paste("OutputFiles/total_commoddisag_fertN.txt",sep = "")
write.table(t(total_commoddisag_fertN), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles/total_commoddisag_fertP.txt",sep = "")
write.table(t(total_commoddisag_fertP), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles/total_commoddisag_fixN.txt",sep = "")
write.table(t(total_commoddisag_fixN), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles/total_commoddisag_fertN_ws",ws_name,".txt",sep = "")
write.table(t(total_commoddisag_fertN_ws), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles/total_commoddisag_fertP_ws",ws_name,".txt",sep = "")
write.table(t(total_commoddisag_fertP_ws), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = paste("OutputFiles/total_commoddisag_fixN_ws",ws_name,".txt",sep = "")
write.table(t(total_commoddisag_fixN_ws), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write commoddisag key
write_name = paste("OutputFileKeys/commoddisag_key.txt")
commoddisag_key = array(" ", c(1+n_crops+1,1+nyrs))
commoddisag_key[1,] = c(" ", run_yrs) #column headings
commoddisag_key[,1]=c("10^9 kg N or P","corn (etoh use removed)","corn silage","wheat","oats",
                      "barley","sorghum grain","sorghum silage","rice","rye","soybeans","potatoes",
                      "alfalfa hay","other hay","cropland pasture","noncropland pasture","peanuts",
                      "CGF","CGM","DGS","etoh") #row headings
write.table(commoddisag_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#animal products 
total_meat_fertN = array(0,c(n_meats,nyrs))
total_meat_fertP = array(0,c(n_meats,nyrs))
total_meat_fixN = array(0,c(n_meats,nyrs))
total_meat_manureN = array(0,c(n_meats,nyrs))
total_meat_manureP = array(0,c(n_meats,nyrs))

for(n in 1:nyrs){
  total_meat_fertN[,n] = rowSums(fertNmeattot[,,n])/(10^9) #fertilizer inputs to meat products in each year
  total_meat_fertP[,n] = rowSums(fertPmeattot[,,n])/(10^9) #fertilizer inputs to meat products in each year
  total_meat_fixN[,n] = rowSums(fixNmeattot[,,n])/(10^9) #fertilizer inputs to meat products in each year
}
total_meat_suppP = Psupp4meat/10^9 #billion kg P
total_meat_manureN = colSums(manureNmeat)/(10^9) #billion kg N
total_meat_manureP = colSums(manurePmeat)/(10^9) #billion kg P

## Field loss proportions
Nincrop=cropdata[,1]*cropdata[,2]
NinperkgC=NinputtoC/drop(colSums(CkgwswE))
NoutperkgC=Nincrop
CNproplost=(NinperkgC-NoutperkgC)/NinperkgC

Pincrop=cropdata[,1]*cropdata[,3]
PinperkgC=PinputtoC/drop(colSums(CkgwswE))
PoutperkgC=Pincrop
CPproplost=(PinperkgC-PoutperkgC)/PinperkgC

for(n in 1:nyrs){
  #product and watershed specific outputs
  write_name = paste("OutputFiles/CfertNwswE",run_yrs[n],".txt",sep = "")
  write.table(CfertNwswE[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CfertPwswE",run_yrs[n],".txt",sep = "")
  write.table(CfertPwswE[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CfixNwswE",run_yrs[n],".txt",sep = "")
  write.table(CfixNwswE[,,], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/fertNmeattot",run_yrs[n],".txt",sep = "")
  write.table(fertNmeattot[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/fertPmeattot",run_yrs[n],".txt",sep = "")
  write.table(fertPmeattot[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/fixNmeattot",run_yrs[n],".txt",sep = "")
  write.table(fixNmeattot[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CBmanureNmeat",run_yrs[n],".txt",sep = "")
  write.table(manureNmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CBmanurePmeat",run_yrs[n],".txt",sep = "")
  write.table(manurePmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/atmNdep",run_yrs[n],".txt",sep = "")
  write.table(totNANIws[,1,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CB_NANIBtot",run_yrs[n],".txt",sep = "")
  write.table(NANIBtot[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CB_NAPIBtot",run_yrs[n],".txt",sep = "")
  write.table(NAPIBtot[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CB_ws_spec_animNreqmeat",run_yrs[n],".txt",sep = "")
  write.table(animNreqpermeat[,n]*kgmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CB_ws_spec_animPreqmeat",run_yrs[n],".txt",sep = "")
  write.table(animPreqpermeat[,n]*kgmeat[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  write_name = paste("OutputFiles/CB_noanimwsdyn",run_yrs[n],".txt",sep = "")
  write.table(noanimwsdyn[ws,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

  #total manure N by county
  write_name = paste("OutputFiles/kgmanureNcnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanureN[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #total manure P by county
  write_name = paste("OutputFiles/kgmanurePcnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanureP[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #recoverable manure N by county
  write_name = paste("OutputFiles/kgmanureNreccnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanureNrec[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #recoverable manure P by county
  write_name = paste("OutputFiles/kgmanurePreccnty",run_yrs[n],".txt",sep = "")
  write.table(kgmanurePrec[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #fertilizer N by county
  write_name = paste("OutputFiles/kgfertNcnty",run_yrs[n],".txt",sep = "")
  write.table(cropprodcnty[,,n]*unitfertNC[,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  #fertilizer P by county
  write_name = paste("OutputFiles/kgfertPcnty",run_yrs[n],".txt",sep = "")
  write.table(cropprodcnty[,,n]*unitfertPC[,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  
  # N in crop by county
  write_name = paste("OutputFiles/kgcropNcnty",run_yrs[n],".txt",sep = "")
  write.table(cropprodcnty[,,n]*cropdata[,1] * cropdata[,2], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE) #cropdata[,1] * cropdata[,2] = percent N in each crop (% DM) * (% N in DM)
  
  #P in crop by county
  write_name = paste("OutputFiles/kgcropPcnty",run_yrs[n],".txt",sep = "")
  write.table(cropprodcnty[,,n]*cropdata[,1] * cropdata[,2], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE) #cropdata[,1] * cropdata[,2] = percent N in each crop (% DM) * (% N in DM)
}

write_name = paste("OutputFiles/etohfertNtot.txt",sep = "")
write.table(t(etohfertNtot), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("OutputFiles/etohfertPtot.txt",sep = "")
write.table(t(etohfertPtot), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write etohfert key
write_name = paste("OutputFileKeys/etohfertNtot_key.txt")
etohfert_key = array(" ", c(1,1+nyrs))
etohfert_key[1,] = c(" ", run_yrs) #column headings
etohfert_key[,1]=c("kg N or P") #row headings
write.table(etohfert_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)