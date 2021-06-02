#Cprodfertfix.r
#derive total and per unit values, i.e., kg N / kg edible food

if(print_tags == 1){
  print("ModelSubs/Cprodfertfix.R")
}

# unit N fixation
unitfixNC = array(0,c(n_crops,nyrs))  # kg N from fixation per kg crop

for(n in 1:nyrs){ #(NANIdata columns 5, 6, 7, 8, 9, 10, 11)
  unitfixNC[10,n] = sum(totNANIws[,7,n]) / sum(CkgwswE_orig[,10,n])
  unitfixNC[11,n] = sum(totNANIws[,8,n]) / sum(CkgwswE_orig[,11,n])
  unitfixNC[12,n] = sum(totNANIws[,6,n]) / sum(CkgwswE_orig[,12,n])
  #     unitfixNC(10,n] = 0.030994296 # alfalfa
  #     unitfixNC(11,n] = 0.003375667 # non-alfalfa hay
  #     unitfixNC(12,n] = 0.065701746 # soy
  unitfixNC[13,n] = sum(totNANIws[,9,n]) / sum(CkgwswE_orig[,13,n]) #cropland pasture
  unitfixNC[16,n] = sum(totNANIws[,11,n]) / sum(CkgwswE_orig[,16,n]) #peanuts
}

#allocate space to matrices
CfertNtot = array(0,c(n_crops,nyrs))  # total fertilizer applied per crop based on an average fert app rate
CfertPtot = array(0,c(n_crops,nyrs))  # total fertilizer applied per crop based on an average fert app rate
etohfertNtot = array(0,c(nyrs,1))
etohfertPtot = array(0,c(nyrs,1))
unitfertNC = array(0,c(n_crops,nyrs))
unitfertPC = array(0,c(n_crops,nyrs))
unitfertNetoh = array(0,c(nyrs))
unitfertPetoh = array(0,c(nyrs))
fertNresidual = array(0,c(n_ws_NEEA,nyrs))
fertPresidual = array(0,c(n_ws_NEEA,nyrs))
fertresidualNANI = array(0,c(n_ws_NEEA,nyrs))
fertresidualNAPI = array(0,c(n_ws_NEEA,nyrs))
fertexpNANI = array(0,c(n_ws_NEEA,nyrs))
fertexpNAPI = array(0,c(n_ws_NEEA,nyrs))
fertNdiff = array(0,c(n_ws_NEEA,nyrs))
fertPdiff = array(0,c(n_ws_NEEA,nyrs))
sum_commod_spec_fertN = array(0,c(n_ws_NEEA,nyrs))
sum_commod_spec_fertP = array(0,c(n_ws_NEEA,nyrs))
#4.3.15 added 3 columns to all below to account for etoh feeds
CfertNwswE = array(0,c(n_ws_NEEA,n_crops,nyrs))
CfertPwswE = array(0,c(n_ws_NEEA,n_crops,nyrs))
etohfertNws = array(0,c(n_ws_NEEA,nyrs))
etohfertPws = array(0,c(n_ws_NEEA,nyrs))
CfertNwE = array(0,c(n_ws_tbx,n_crops,nyrs)) # for tile drainage calcs
CfertPwE = array(0,c(n_ws_tbx,n_crops,nyrs)) # for tile drainage calcs
etohfertN = array(0,c(n_ws_tbx,nyrs))
etohfertP = array(0,c(n_ws_tbx,nyrs))
CfixNwE = array(0,c(n_ws_tbx,n_crops,nyrs)) # preloss, preexport value
CfixNwswE = array(0,c(n_ws_NEEA,n_crops,nyrs))
CfertNexp = array(0,c(n_ws_NEEA,n_crops,nyrs))
CfertPexp = array(0,c(n_ws_NEEA,n_crops,nyrs))
CfixNexp = array(0,c(n_ws_NEEA,n_crops,nyrs))
CfixwswENANI = array(0,c(n_ws_NEEA,n_crops,nyrs))
CNkgwE = array(0,c(n_crops,nyrs))
CPkgwE = array(0,c(n_crops,nyrs))
NinputtoC = array(0,c(n_crops,nyrs))
PinputtoC = array(0,c(n_crops,nyrs))
grainNperNinput = array(0,c(n_crops,nyrs))
grainPperPinput = array(0,c(n_crops,nyrs))


#fertilizer per crop
for(n in 1:nyrs){
  CfertNtot[1:16,n] = Nfert[,n] * croparea[1:16,n] # total fertilizer applied per crop based on an average fert app rate
  # Food in NANI
  # model_CURRENT.xlsx
  # harvested or planted acreage based on setting, this value is for NANI crops only
  CfertPtot[1:16,n] = Pfert[,n] * croparea[1:16,n] # total fertilizer applied per crop based on an average fert app rate
  # Food in NAPI
  # harvested or planted acreage based on setting, this value is for NAPI crops only
  #for etoh coproducts, use corn fert
  #the areas for these are the proportion of corn production area for
  #corn etoh
  for(i in 17:19){
    CfertNtot[i,n]=Nfert[1,n] * croparea[i,n]
    CfertPtot[i,n]=Pfert[1,n] * croparea[i,n]
  }
  
  etohfertNtot[n] = Nfert[1,n] * etoh_landuse[n] #kg N fert allocated to etoh based on corn
  etohfertPtot[n] = Pfert[1,n] * etoh_landuse[n] #kg P fert allocated to etoh based on corn
  
  for(i in 1:n_crops){
    if(CfertNtot[i,n] > 0){
      unitfertNC[i,n] = CfertNtot[i,n] / (sum(CkgwE[,i,n])) # kg fert / kg crop. based on production without subtracting "loss/waste"
    }else{
      unitfertNC[i,n] = 0
    }
    if(CfertPtot[i,n] > 0){
      unitfertPC[i,n] = CfertPtot[i,n] / (sum(CkgwE[,i,n])) # kg fert / kg crop. based on production without subtracting "loss/waste"
    }else{
      unitfertPC[i,n] = 0
    }
  }
  
  unitfertNetoh[n] = etohfertNtot[n] / (sum(etohL[,n])) # kg fert / L etoh
  unitfertPetoh[n] = etohfertPtot[n] / (sum(etohL[,n])) # kg fert / L etoh
  
  # check # N in:N in crop
  # total fertilizer per crop per watershed
  
  for(i in 1:n_crops){
    CfertNwswE[,i,n] = CkgwswE_orig[,i,n] * unitfertNC[i,n] # 4.23.13, pre-loss production values
    CfertPwswE[,i,n] = CkgwswE_orig[,i,n] * unitfertPC[i,n]
    CfertNwE[,i,n] = CkgwE[,i,n] * unitfertNC[i,n] # for tile drainage calcs, 450 watersheds instead of n_ws_NEEA to convert to county level
    CfertPwE[,i,n] = CkgwE[,i,n] * unitfertPC[i,n]
    CfixNwswE[,i,n] = CkgwswE_orig[,i,n] * unitfixNC[i,n]
    CfertNexp[,i,n] = Cexpkg[,i,n] * unitfertNC[i,n]
    CfertPexp[,i,n] = Cexpkg[,i,n] * unitfertPC[i,n]
    CfixNwE[,i,n] = CkgwE[,i,n] * unitfixNC[i,n]
    CfixNexp[,i,n] = Cexpkg[,i,n] * unitfixNC[i,n]
  }
  
  etohfertNws[,n] = etohL_orig[,n] * unitfertNetoh[n]
  etohfertPws[,n] = etohL_orig[,n] * unitfertPetoh[n]
  etohfertN[,n] = etohL[,n] * unitfertNetoh[n]
  etohfertP[,n] = etohL[,n] * unitfertPetoh[n]
  
  for(i in 10:13){
    CfixwswENANI[,i,n] = CfixNwswE[,i,n] / areaws
  }
  
  # N in grain / N in fert + fix
  NinputtoC[,n] = t(colSums(CfertNwE[,,n])) + t(colSums(CfixNwswE[,,n]))
  PinputtoC[,n] = t(colSums(CfertNwE[,,n])) + t(colSums(CfixNwswE[,,n]))
  CNkgwE[,n] = sumCkgwE[,n] * NperC #no adjustments to crop production are made here b/c even if this grain is lost it was produced.
  CPkgwE[,n] = sumCkgwE[,n] * PperC
  grainNperNinput[,n] = CNkgwE[,n] / NinputtoC[,n] # serves as a check
  grainPperPinput[,n] = CNkgwE[,n] / NinputtoC[,n] # serves as a check
  
  # calculates the remaining Ag fertilizer applied in a given watershed after
  # accounting for crop-specific estimates. this is agricultural fertilzer,
  # not residential
  sum_commod_spec_fertN[,n]=rowSums(CfertNwswE[,,n]) + etohfertNws[,n]
  sum_commod_spec_fertP[,n]=rowSums(CfertPwswE[,,n]) + etohfertPws[,n]
  fertNresidual[,n] = NEEAws_NANI[,3,n] - sum_commod_spec_fertN[,n] # tot kg in orig NANI calc - sum of kg N fert calculations
  # for crops and FCs - etohfert estimate
  fertPresidual[,n] = NEEAws_NAPI[,3,n] - sum_commod_spec_fertP[,n] # tot kg in orig NAPI calc - sum of kg P fert calculations 
  # for crops and FCs - etohfert estimate
  fertNdiff[,n]=-fertNresidual[,n] / sum_commod_spec_fertN[,n] # error estimation
  fertPdiff[,n]=-fertPresidual[,n] / sum_commod_spec_fertP[,n] # error estimation
  fertresidualNANI[,n] = fertNresidual[,n] / areaws
  fertresidualNAPI[,n] = fertPresidual[,n] / areaws
  fertexpNANI[,n] = rowSums(CfertNexp[,,n]) / areaws
  fertexpNAPI[,n] = rowSums(CfertPexp[,,n]) / areaws 
}