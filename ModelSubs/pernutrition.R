## pernutrition.R

## Impact (N, GHG) per nutritional unit
#allocate space for matrices
fertNperCkcal = array(0,c(n_crops,nyrs))
fertNperCprot = array(0,c(n_crops,nyrs))
fertNmeattot = array(0,c(n_meats,n_crops,nyrs))
fertPperCkcal = array(0,c(n_crops,nyrs))
fertPperCprot = array(0,c(n_crops,nyrs))
fertPmeattot = array(0,c(n_meats,n_crops,nyrs))
fixNmeattot = array(0,c(n_meats,n_crops,nyrs)) 
fixNperCkcal = array(0,c(n_crops,nyrs))
fixNperCprot = array(0,c(n_crops,nyrs))
fertNperMkcal = array(0,c(n_meats,n_crops,nyrs))  #kg N fertilizer per kcal in 1 kg crop or animal product  M = meat
fertNperMprot = array(0,c(n_meats,n_crops,nyrs))  #kg N fertilizer per g protein in 1 kg crop or animal product
fixNperMkcal = array(0,c(n_meats,n_crops,nyrs))   #kg N fixation per kcal in 1 kg crop or animal product
fixNperMprot = array(0,c(n_meats,n_crops,nyrs))   #kg N fixation per g protein in 1 kg crop or animal product
fertPperMkcal = array(0,c(n_meats,n_crops,nyrs))  #kg P fertilizer per kcal in 1 kg crop or animal product  M = meat
fertPperMprot = array(0,c(n_meats,n_crops,nyrs))  #kg P fertilizer per g protein in 1 kg crop or animal product

for(n in 1:nyrs){
  #crops
  fertNperCkcal[,n] = unitfertNC[,n] / cropdata[,8] # kg N fert/kcal crop  C = crop
  fertNperCprot[,n] = unitfertNC[,n] / (cropdata[,9] / 1000) # kg N fert/kg protein crop
  
  fertPperCkcal[,n] = unitfertPC[,n] / cropdata[,8] # kg N fert/kcal crop  C = crop
  fertPperCprot[,n] = unitfertPC[,n] / (cropdata[,9] / 1000) # kg N fert/kg protein crop
  
  fixNperCkcal[,n] = unitfixNC[,n] / cropdata[,8]
  fixNperCprot[,n] = unitfixNC[,n] / (cropdata[,9]/1000)
  
  #correct for errors, rows 2 & 7 silage, no human consumption
  #rows 10, 11, 13, 14
  fertNperCkcal[2,n] = 0
  fertNperCkcal[7,n] = 0
  fertNperCkcal[10,n] = 0
  fertNperCkcal[11,n] = 0
  fertNperCkcal[13,n] = 0
  fertNperCkcal[14,n] = 0
  fertNperCprot[2,n] = 0
  fertNperCprot[7,n] = 0
  fertNperCprot[10,n] = 0
  fertNperCprot[11,n] = 0
  fertNperCprot[13,n] = 0
  fertNperCprot[14,n] = 0
  
  fertPperCkcal[2,n] = 0
  fertPperCkcal[7,n] = 0
  fertPperCkcal[10,n] = 0
  fertPperCkcal[11,n] = 0
  fertPperCkcal[13,n] = 0
  fertPperCkcal[14,n] = 0
  fertPperCprot[2,n] = 0
  fertPperCprot[7,n] = 0
  fertPperCprot[10,n] = 0
  fertPperCprot[11,n] = 0
  fertPperCprot[13,n] = 0
  fertPperCprot[14,n] = 0
  
  #meat
  # note that these values are domestically produced values adjustments for
  # net traded quantities are calculated in meattrade.m all meat is produced
  # domestically, N impacts are incurred domestically
  
  #10.3.12 removed "ADJ" from fertpermeat
  for(i in 1:n_meats){
    fertNmeattot[i,,n] = fertNpermeat[i,,n] * totmeat[n,i] # total kg fertilizer by crop 
    # per animal product (function of fertilizer applied to crops), domestic
    fixNmeattot[i,,n] = fixNpermeat[i,,n] * totmeat[n,i]
    fertNperMkcal[i,,n] = (fertNpermeat[i,,n] / meatdata[i,7]) # kg N fert/kcal in 1 kg meat
    fertNperMprot[i,,n] = (fertNpermeat[i,,n] / (meatdata[i,z] / 1000)) # kg N fert / g protein in 1 kg meat
    fixNperMkcal[i,,n] = (fixNpermeat[i,,n] / meatdata[i,7]) # kg of N in N fixed per kcal in 1 kg meat
    fixNperMprot[i,,n] = (fixNpermeat[i,,n] / (meatdata[i,z] / 1000)) # kg of N in N fixed per kg protein
    
    fertPmeattot[i,,n] = fertPpermeat[i,,n] * totmeat[n,i] # total kg fertilizer by crop per animal product (function of fertilizer applied to crops), domestic
    fertPperMkcal[i,,n] = (fertPpermeat[i,,n] / meatdata[i,7]) # kg P fert/kcal in 1 kg meat
    fertPperMprot[i,,n] = (fertPpermeat[i,,n] / (meatdata[i,z] / 1000)) # kg P fert / g protein in 1 kg meat
  }
  
  fertNpermeat[5,,n] = 0
  fertNmeattot[5,,n] = 0
  fertNperMkcal[5,,n] = 0
  fertNperMprot[5,,n] = 0
  fixNperMkcal[5,,n] = 0
  fixNperMprot[5,,n] = 0
  
  fertPpermeat[5,,n] = 0
  fertPmeattot[5,,n] = 0
  fertPperMkcal[5,,n] = 0
  fertPperMprot[5,,n] = 0
}

## total kcal and protein for each crop or animal food product directly consumed by humans
#allocate space to matrices
totMkcal = array(0,c(n_meats,nyrs)) # kcal for each animal food product produced
totMprot = array(0,c(n_meats,nyrs)) # kg protein for each animal food product produced
totCkcal = array(0,c(n_crops,nyrs)) # kcal provided by each crop food product FOR HUMAN CONSUMPTION
totCprot = array(0,c(n_crops,nyrs)) # kg protein provided by each crop food product FOR HUMAN CONSUMPTION
MNsumkg = array(0,c(n_meats,4,nyrs)) # fert, fix, NH3, manure N normalized by mass
MNsumkcal = array(0,c(n_meats,4,nyrs)) # fert, fix, NH3, manure N normalized by kcal
MNsumprot = array(0,c(n_meats,4,nyrs)) # fert, fix, NH3, manure N normalized by protein
MPsumkg = array(0,c(n_meats,2,nyrs)) # fert, manure N normalized by mass
MPsumkcal = array(0,c(n_meats,2,nyrs)) # fert, manure N normalized by kcal
MPsumprot = array(0,c(n_meats,2,nyrs)) # fert, manure N normalized by protein
CNsumkg = array(0,c(n_crops,2,nyrs))  # col 1 sum of fert per kg crop col 2 sum of N fix per kg crop
CPsumkg = array(0,c(n_crops,nyrs))  # col 1 sum of fert per kg crop
CNsumkcal = array(0,c(n_crops,2,nyrs)) # col 1 sum of fert per kcal (in 1 kg) crop col 2 sum of N fix per kcal in crop
CPsumkcal = array(0,c(n_crops,nyrs)) # col 1 sum of fert per kcal (in 1 kg) crop
CNsumprot = array(0,c(n_crops,2,nyrs)) # col 1 sum of fert per g protein (in 1 kg) crop col 2 sum of N fix per g protein in crop
CPsumprot = array(0,c(n_crops,nyrs)) # col 1 sum of fert per g protein (in 1 kg) crop

for(n in 1:nyrs){
  totMkcal[,n] = t(totmeat[n,]) * meatdata[,7] # kcal for each animal food product produced
  totMprot[,n] = (t(totmeat[n,]) * meatdata[,z])/1000 # kg protein for each animal food product produced
  totCkcal[,n] = t(sumC4human[n,]) * cropdata[,8] # kcal provided by each crop food product FOR HUMAN CONSUMPTION
  totCprot[,n] = (t(sumC4human[n,]) * cropdata[,9]) / 1000 # kg protein provided by each crop food product FOR HUMAN CONSUMPTION
  
  # total unit N for each kg food product
  #meat
  MNsumkg[,1,n] = rowSums(fertNpermeat[,,n])
  MNsumkg[,2,n] = rowSums(fixNpermeat[,,n])
  MNsumkg[,3,n] = t(NH3permeat[n,])
  MNsumkg[,4,n] = t(manureNpermeat[n,])
  
  MNsumkcal[,1,n] = rowSums(fertNperMkcal[,,n])
  MNsumkcal[,2,n] = rowSums(fixNperMkcal[,,n])
  MNsumkcal[,3,n] = t(NH3perMkcal[,n])
  MNsumkcal[,4,n] = t(manureNperkcal[,n])
  
  MNsumprot[,1,n] = rowSums(fertNperMprot[,,n])
  MNsumprot[,2,n] = rowSums(fixNperMprot[,,n])
  MNsumprot[,3,n] = t(NH3perMprot[,n])
  MNsumprot[,4,n] = t(manureNperprot[,n])
  
  MPsumkg[,1,n] = rowSums(fertPpermeat[,,n])
  MPsumkg[,2,n] = t(manurePpermeat[n,])
  
  MPsumkcal[,1,n] = rowSums(fertPperMkcal[,,n])
  MPsumkcal[,2,n] = t(manurePperkcal[,n])
  
  MPsumprot[,1,n] = rowSums(fertPperMprot[,,n])
  MPsumprot[,2,n] = t(manurePperprot[,n])
  
  #crops
  CNsumkg[,1,n] = unitfertNC[,n]
  CPsumkg[,n] = unitfertPC[,n]
  CNsumkg[,2,n] = unitfixNC[,n]
  
  CNsumkcal[,1,n] = fertNperCkcal[,n]
  CPsumkcal[,n] = fertPperCkcal[,n]
  CNsumkcal[,2,n] = fixNperCkcal[,n]
  
  CNsumprot[,1,n] = fertNperCprot[,n]
  CPsumprot[,n] = fertPperCprot[,n]
  CNsumprot[,2,n] = fixNperCprot[,n]
}

## crop export-related fertilizer and N fixation. crop-related fertilizer 
# and N fixation for crops fed to exported animal food products
# calculated seperately in meattrade.m
fertNforCexp = array(0,c(n_crops,nyrs))
fertPforCexp = array(0,c(n_crops,nyrs))
fixNforCexp = array(0,c(n_crops,nyrs))
fixNresidual = array(0,c(n_ws_NEEA,nyrs)) #total, not per kg.
fixresNANI = array(0,c(n_ws_NEEA,nyrs)) #per area
NANIcropNexp = array(0,c(n_ws_NEEA,nyrs)) #crop N exported per area for each watershed
NAPIcropPexp = array(0,c(n_ws_NEEA,nyrs)) #crop N exported per area for each watershed

for(n in 1:nyrs){
  fertNforCexp[1,n] = cornexp[,n] * unitfertNC[1,n]
  fertPforCexp[1,n] = cornexp[,n] * unitfertPC[1,n]
  
  for(i in 2:n_crops){
    fertNforCexp[i,n] = exports[i,n] * unitfertNC[i,n]
    fertPforCexp[i,n] = exports[i,n] * unitfertPC[i,n]
  }
  
  fixNforCexp[10,n] = exports[10,n] * unitfixNC[10,n]     #alfalfa
  fixNforCexp[11,n] = exports[11,n] * unitfixNC[11,n]     #non-alfalfa hay
  fixNforCexp[12,n] = exports[12,n] * unitfixNC[12,n]     #soy
  fixNforCexp[16,n] = exports[16,n] * unitfixNC[16,n]     #soy
  
  fixNresidual[,n] = totNANIws[,5,n]-rowSums(CfixNwswE[,,n]) #total, not per kg.
  fixresNANI[,n] = fixNresidual[,n] / areaws
  NANIcropNexp[,n] = rowSums(CNexpkg[,,n]) / areaws
  NAPIcropPexp[,n] = rowSums(CPexpkg[,,n]) / areaws
}