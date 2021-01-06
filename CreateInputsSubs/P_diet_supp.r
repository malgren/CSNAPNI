#P_diet_supp.r
#This script determines to what extent the crop allocations to animals
#in the N-requirement-based diet model fulfill the P needs of animals
#and uses this information to estimate P supplementation in animal diets

#Be sure to run constants.r and cropalloc_model.r first if running this script independently
#source('constants.r')
#source('cropalloc_model.r')

#use livestock P requirements (animPreq)
#and modeled allocation of each crop to each animal type in each year (cropPtoanim)

missingPperanim = array(0, c(n_anims,data_yrs))
missingPanimtotals = array(0, c(n_anims,data_yrs))

for(i in 1:data_yrs){
  missingPperanim[,i] = (animPreq-rowSums(cropPtoanim[,,i]))
  #remove negative values (P overallocations)
  missingPperanim[missingPperanim[,i]<0,i] = 0
  missingPanimtotals[,i] = missingPperanim[,i]*animpoptotal[,i]
}

missingPtotal = colSums(missingPanimtotals) # total missing P in each year

#check P availability for P supplements
file_name = paste("RawData/NAPI_tbx_totals.txt",sep = "")
NAPI_tbx_totals=t(array(scan(file_name), c(data_yrs,4)))
US_P_fertilizer_sales = NAPI_tbx_totals[1,]*10^9
US_P_inputs = c(4.2,4.2,4.2,4.2,3.6)*10^9 #in kg P, 2012 only (mining+imports, converted from Phosphate rock to P)
prop_P_to_phosphoric_acid = 0.95 #from USGS, 2002-2017 data says "more than 95%"
#assuming all P ore, domestic or imported, has the same use distribution
fertilizer_exports = c(1.29,1.29,1.29,1.29,0.85)*10^9 #in kg P, using 2012 and 2017 data only
phosphoric_acid_exports = c(0.2,0.2,0.2,0.2,0.13)*10^9 #in kg P, using 2012 and 2017 data only
P_to_phosphoric_acid = US_P_inputs*prop_P_to_phosphoric_acid
P_to_feed_supplements = P_to_phosphoric_acid-US_P_fertilizer_sales-fertilizer_exports-phosphoric_acid_exports

prop_P_need_avail = P_to_feed_supplements/missingPtotal

#factor-in P digestibility
P_digestibility = array(scan("RawData/P_digestibility.txt"), c(19,19))
missingPperanim_dig = array(0, c(n_anims,data_yrs))
missingPanimtotals_dig = array(0, c(n_anims,data_yrs))
cropPtoanim_dig = cropPtoanim * array(P_digestibility,c(19,19,data_yrs))
for(i in 1:data_yrs){
  missingPperanim_dig[,i] = (animPreq-rowSums(cropPtoanim_dig[,,i]))
  #remove negative values (P overallocations)
  missingPperanim_dig[missingPperanim_dig[,i]<0,i] = 0
  missingPanimtotals_dig[,i] = missingPperanim_dig[,i]*animpoptotal[,i]
}
missingPtotal_dig = colSums(missingPanimtotals_dig) # total missing P in each year

prop_digP_need_avail = P_to_feed_supplements/missingPtotal_dig

for(i in 1:data_yrs){
  Psupp_peranim[,i] = missingPperanim[,i]*prop_digP_need_avail[i]
  Psupp_animtotals[,i] = Psupp_peranim[,i]*animpoptotal[,i]
}
Psupp_total = colSums(Psupp_animtotals)

#write data files
write_name = paste("InputFiles/Psupp_peranim.txt",sep = "") #kg P supplementation per animal for each livestock category in each year
write.table(Psupp_peranim, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles/Psupp_animtotals.txt",sep = "") #total kg P supplementation for each livestock category in each year
write.table(Psupp_animtotals, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles/Psupp_total.txt",sep = "") #total kg P supplementation in each year
write.table(Psupp_total, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

write_name = "InputFileKeys/Psupp_key.txt"
Psupp_key = array(" ", c(n_anims+1,data_yrs+1))
Psupp_key[1,]=c(" ", year_labels) #column headings
Psupp_key[,1]=c("(kg P)", animtyp) #row headings
write.table(Psupp_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE, quote=FALSE)
