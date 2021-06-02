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

if(print_tags == 1){
  print("CreateInputsSubs/P_diet_supp.R")
}

missingPperanim = array(0, c(n_anims,data_yrs))
missingPanimtotals = array(0, c(n_anims,data_yrs))

#waste_P_from_slaughter
total_anim_feed_P = array(0,c(n_anims,data_yrs))

for(i in 1:data_yrs){
  for(j in 1:n_anims){
    total_anim_feed_P[j,i] = (animPreq[j]*animpoptotal[j,i])
    if(total_anim_feed_P[j,i]>0){
      missingPperanim[j,i] = (animPreq[j]-sum(cropPtoanim[j,,i]))
    }
  }
  #remove negative values (P overallocations)
  missingPperanim[missingPperanim[,i]<0,i] = 0
  missingPanimtotals[,i] = missingPperanim[,i]*animpoptotal[,i]
}


missingPtotal = colSums(missingPanimtotals) # total missing P in each year

#result of calculations in Pfeedsupplementprod.xlsx
mineral_P_to_feed_supplements = c(0.89,0.36,0.28,0.68,0.93)*10^9
avg_prop_P_need_avail = mean(mineral_P_to_feed_supplements)/missingPtotal
other_p_intake = missingPtotal - mineral_P_to_feed_supplements

Psupp_peranim = array(0,c(n_anims,data_yrs))
Psupp_animtotals = array(0,c(n_anims,data_yrs))
otherP_peranim = array(0,c(n_anims,data_yrs))
otherP_animtotals = array(0,c(n_anims,data_yrs))

for(i in 1:data_yrs){
  if(avg_prop_P_need_avail[i]>1){
    Psupp_peranim[,i] = missingPperanim[,i]
    otherP_peranim[,i] = missingPperanim[,i]*0
  }else{
    Psupp_peranim[,i] = missingPperanim[,i]*avg_prop_P_need_avail[i]
    otherP_peranim[,i] = missingPperanim[,i]*(1-avg_prop_P_need_avail[i])
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
