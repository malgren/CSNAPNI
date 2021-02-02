## NPinputs_aggregate.R

## FF calculation, in "static" NANI calculations animal N available for
# humans is calculated by subtracting approximate N excreted from animal N
# requirements, values are on a per animal basis. In the "dynamic" version 
# approximate animal N available is calculated by estimating animal carcass 
# weight, edible portion of carcass and N in edible portion.  In this 
# analysis animal N requirements are calculated for animal populations based 
# on dynamic population estimates for all animals . N in animals
# is calculated using the original NANI assumption N req - N manure, rather
# than N in meat.

#allocate space for arrays
NANIanimreq = array(0,c(n_ws_NEEA,nyrs))
NAPIanimreq = array(0,c(n_ws_NEEA,nyrs))
NANIanimN = array(0,c(n_ws_NEEA,nyrs))
NAPIanimP = array(0,c(n_ws_NEEA,nyrs))
NANIcropN = array(0,c(n_ws_NEEA,nyrs))
NAPIcropP = array(0,c(n_ws_NEEA,nyrs))
NAPIsuppP = array(0,c(n_ws_NEEA,nyrs))
Psuppws = array(0,c(n_ws_NEEA,nyrs))
FF_N = array(0,c(n_ws_NEEA,nyrs))
FF_P = array(0,c(n_ws_NEEA,nyrs))
FFtotN = array(0,c(n_ws_NEEA,nyrs))
FFtotP = array(0,c(n_ws_NEEA,nyrs))
NANIfertB = array(0,c(n_ws_NEEA,nyrs))
NAPIfertB = array(0,c(n_ws_NEEA,nyrs))
NANIbase = array(0,c(n_ws_NEEA,nyrs))
NAPIbase = array(0,c(n_ws_NEEA,nyrs))
origNANI = array(0,c(n_ws_NEEA,nyrs))
origNAPI = array(0,c(n_ws_NEEA,nyrs))
NANIorigtot = array(0,c(n_ws_NEEA,6,nyrs))
NAPIorigtot = array(0,c(n_ws_NEEA,4,nyrs))
NANIBtot = array(0,c(n_ws_NEEA,6,nyrs))
NAPIBtot = array(0,c(n_ws_NEEA,6,nyrs))
NANIBtotsum = array(0,c(n_ws_NEEA,nyrs))
NAPIBtotsum = array(0,c(n_ws_NEEA,nyrs))
NANIBperkm = array(0,c(n_ws_NEEA,nyrs))
NAPIBperkm = array(0,c(n_ws_NEEA,nyrs))
N4human = array(0,c(nyrs,1))
P4human = array(0,c(nyrs,1))
N4humanpp = array(0,c(nyrs,1))
P4humanpp = array(0,c(nyrs,1))
N4humanWW = array(0,c(nyrs,1))
P4humanWW = array(0,c(nyrs,1))
N4humanppWW = array(0,c(nyrs,1))
P4humanppWW = array(0,c(nyrs,1))


for(n in 1:nyrs){
  NANIanimreq[,n] = rowSums(kganimNreqs[,,n]) / areaws
  NAPIanimreq[,n] = rowSums(kganimPreqs[,,n]) / areaws
  
  NANIanimN[,n] = rowSums(animN[,,n]) / areaws # calculated as anim N req - anim N manure, rather than from meat
  NAPIanimP[,n] = rowSums(animP[,,n]) / areaws # should be calculated just from meat!
  
  NANIcropN[,n] = (rowSums(C4humanN[,,n],2) + rowSums(C4animNadj[,,n])) / areaws
  NAPIcropP[,n] = (rowSums(C4humanP[,,n],2) + rowSums(C4animPadj[,,n])) / areaws
  
  Psuppws[,n] = rowSums(t(array(Psupp_peranim[,n], c(19,144))) * noanimwsdyn[,,n])
  NAPIsuppP[,n] = Psuppws[,n] / areaws 
  
  FF_N[,n] = (NANIanimreq[,n] + hmnNreqs[,n]) - ((NANIanimN[,n] + NANIcropN[,n]) + NANIcropNexp[,n])
  #NANIcropN values have exported quantities removed
  
  FF_P[,n] = (NAPIanimreq[,n] + hmnfoodPreqs[,n]) - ((NAPIanimP[,n] + NAPIcropP[,n]) + NAPIcropPexp[,n] + NAPIsuppP[,n]) # + NAPIanimreq[,n]*0.041) # is a proxy for animal protein feed recycling into animal diets, 
                                                                                                                                                # which reduces the need for new P imports 
  #NAPIcropP values have exported quantities removed
  
  FFtotN[,n] = FF_N[,n] * areaws
  FFtotP[,n] = FF_P[,n] * areaws
  
  NANIfertB[,n] = rowSums(CfertNwswE[,,n]) / areaws
  NAPIfertB[,n] = rowSums(CfertPwswE[,,n]) / areaws
  
  NANIbase[,n] = newNANIws[,1,n] + rowSums(CfixwswENANI[,,n]) + fixresNANI[,n] + NANIfertB[,n] 
  + fertresidualNANI[,n] + newNANIws[,4,n] + FF_N[,n]
  #atm dep + N fixation + total fertilizer (crops in analysis + residual fertilizer for crops + non-ag) * food/feed
  
  NAPIbase[,n] = NAPIfertB[,n] + fertresidualNAPI[,n] + newNAPIws[,3,n] + FF_P[,n]
  #total fertilizer (crops in analysis + residual fertilizer for crops + non-ag) * food/feed
  
  # original, uses the static food/feed and fertilizer values
  origNANI[,n] = newNANIws[,1,n] + newNANIws[,5,n] + newNANIws[,2,n] + newNANIws[,12,n] + newNANIws[,13,n]
  #atm dep + N fixation + total fertilizer + food/feed + nonfood crop N
  #prod
  
  origNAPI[,n] = newNAPIws[,1,n] + newNAPIws[,4,n] + newNAPIws[,5,n]
  #total fertilizer + food/feed + nonfood crop N prod
  
  # organize NANI values, total kg into one matrix, B = baseline
  
  NANIorigtot[,1,n] = totNANIws[,1,n]     # atm dep
  NANIorigtot[,2,n] = totNANIws[,5,n]     # N fix
  NANIorigtot[,3,n] = totNANIws[,3,n]     # ag fertilizer
  NANIorigtot[,4,n] = totNANIws[,12,n]    # food/feed
  NANIorigtot[,5,n] = totNANIws[,4,n]     # non-Ag fertilizer
  NANIorigtot[,6,n] = totNANIws[,13,n]    # nonfood fertilizer
  
  NAPIorigtot[,1,n] = totNAPIws[,2,n]     # ag fertilizer
  NAPIorigtot[,2,n] = totNAPIws[,4,n]     # food/feed
  NAPIorigtot[,3,n] = totNAPIws[,3,n]     # non-Ag fertilizer
  NAPIorigtot[,4,n] = totNAPIws[,5,n]     # non-food fertilizer
  
  NANIBtot[,1,n] = totNANIws[,1,n]            # atm dep
  NANIBtot[,2,n] = rowSums(CfixNwswE[,,n])    # N fix
  NANIBtot[,3,n] = sum_commod_spec_fertN[,n]  # ag fertilizer
  NANIBtot[,4,n] = FFtotN[,n]                 # food/feed
  NANIBtot[,5,n] = totNANIws[,4,n]            # domestic fertilizer
  NANIBtot[,6,n] = totNANIws[,13,n]           # nonfood fertilizer
  NANIBtotsum[,n] = rowSums(NANIBtot[,,n])
  NANIBperkm[,n] = NANIBtotsum[,n] / areaws
  
  NAPIBtot[,1,n] = sum_commod_spec_fertP[,n]  # ag fertilizer
  NAPIBtot[,2,n] = FFtotP[,n]                 # food/feed
  NAPIBtot[,3,n] = totNAPIws[,3,n]            # domestic fertilizer
  NAPIBtot[,4,n] = totNAPIws[,5,n]            # non-food fertilizer
  NAPIBtot[,5,n] = tothmndetPreqs[,n]         # detergent
  NAPIBtot[,6,n] = Psuppws[,n]                # diet supplements to animals
  NAPIBtotsum[,n] = rowSums(NAPIBtot[,,n])
  NAPIBperkm[,n] = NAPIBtotsum[,n] / areaws
  
  ## don't calculate until meat exports are updated
  ## N per human calculated within this model
  #     N4human(n) = sum(sum(C4humanN[,,n])) + (sum(sum(animN[,,n])) - sum(sum(expMN[,1))))
  #     N4humanpp(n) = N4human(n) / sum(popws[,n])
  #     
  #     # N per human using WW values
  #     N4humanWW(n) = sum(sum(C4humanNWW[,,n])) + (sum(sum(kgNmeatWW[,,n])) - sum(sum(expMN[,n])))
  #     N4humanppWW(n) = N4humanWW(n) / sum(popws[,n])
}