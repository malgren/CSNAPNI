#P_diet_supp.r
#This script determines to what extent the crop allocations to animals
#in the N-intake-based diet model fulfill the P needs of animals
#and uses this information to estimate P supplementation and animal protein feed consumption in animal diets

#Be sure to run constants.r and cropalloc_model.r first if running this script independently
#source('constants.r')
#source('cropalloc_model.r')

#use livestock P requirements (animPreq) 
#-- contrary to what the variable name suggests, this is a P intake estimate, not animal P needs --
#and N-intake based allocation of each crop to each animal type in each year (cropPtoanim)
#to calculate the diet P deficit

missingPperanim = array(0, c(n_anims,data_yrs))
missingPanimtotals = array(0, c(n_anims,data_yrs))

#waste_P_from_slaughter
anim_prot_feed_P = array(0,c(n_anims,data_yrs))

for(i in 1:data_yrs){
  missingPperanim[,i] = (animPreq-rowSums(cropPtoanim[,,i]))
  #remove negative values (P overallocations)
  missingPperanim[missingPperanim[,i]<0,i] = 0
  missingPanimtotals[,i] = missingPperanim[,i]*animpoptotal[,i]
  # estimate P available for animal protein feed use (P intake - P in meat - P in manure - P waste from slaughter)
  for(j in 1:n_anims){
    anim_prot_feed_P[j,i] = (animPreq[j]*animpoptotal[j,i])*0.041 #4.1% factor backcalculated from Suh and Yee's estimate of .121 billion kg P used as animal protein feed in 2007
  }
}


missingPtotal = colSums(missingPanimtotals) # total missing P in each year

#result of calculations in Pfeedsupplementprod.xlsx
mineral_P_to_feed_supplements = c(1.048,0.684,0.353,0.683,0.924)*10^9
prop_P_need_avail = mean(mineral_P_to_feed_supplements)/missingPtotal
other_p_intake = missingPtotal - mineral_P_to_feed_supplements

Psupp_peranim = array(0,c(n_anims,data_yrs))
Psupp_animtotals = array(0,c(n_anims,data_yrs))
otherP_peranim = array(0,c(n_anims,data_yrs))
otherP_animtotals = array(0,c(n_anims,data_yrs))

for(i in 1:data_yrs){
  if(prop_P_need_avail[i]>1){
    Psupp_peranim[,i] = missingPperanim[,i]
    otherP_peranim[,i] = missingPperanim[,i]*0
  }else{
    Psupp_peranim[,i] = missingPperanim[,i]*prop_P_need_avail[i]
    otherP_peranim[,i] = missingPperanim[,i]*(1-prop_P_need_avail[i])
  }
  Psupp_animtotals[,i] = Psupp_peranim[,i]*animpoptotal[,i]
  otherP_animtotals[,i] = otherP_peranim[,i]*animpoptotal[,i]
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
