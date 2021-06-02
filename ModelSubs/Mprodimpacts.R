## Mprodimpacts.R
## MEAT FERT, FIX, NH3, N2O, CH4, LU, and MANURE
## impact per meat

if(print_tags == 1){
  print("ModelSubs/Mprodimpacts.R")
}

#allocate space for matrices
animNfromC = array(0,c(n_anims,n_crops,nyrs))
animPfromC = array(0,c(n_anims,n_crops,nyrs))
C4animNtot = array(0,c(n_crops,nyrs))
C4animPtot = array(0,c(n_crops,nyrs))
totmeat = array(0,c(nyrs,n_meats))
totNH3 = array(0,c(nyrs,n_anims))
totmanureN = array(0,c(nyrs,n_anims))
totmanureP = array(0,c(nyrs,n_anims))
feedN4anim = array(0,c(n_anims,n_crops,nyrs)) # total kg N for all animals of each type by crop
feedP4anim = array(0,c(n_anims,n_crops,nyrs)) # total kg P for all animals of each type by crop
feed4anim = array(0,c(n_anims,n_crops,nyrs)) # total kg crop for all animals of each type by crop
feed4animP = array(0,c(n_anims,n_crops,nyrs)) # total kg crop for all animals of each type by crop, using P as check
feedperanim = array(0,c(n_anims,n_crops,nyrs)) # kg crop per animal
feedNpermeat = array(0,c(n_meats,n_crops,nyrs)) # kg N in crops per kg of edible animal product
feedPpermeat = array(0,c(n_meats,n_crops,nyrs)) # kg P in crops per kg of edible animal product
feedpermeat = array(0,c(n_meats,n_crops,nyrs)) # kg crop per kg of edible animal product, before ADJUSTMENT (if the adjustment is done)
# assumption is that breeding stock feed is allocated to meat production
# ratios of fiber, milk and meat from sheep and goats are not clear,
# clearly all of these animals are not utilized for meat production (based on USDA slaughter data).
# especially since these animals are small compared to others they are
# omitted from this study, though animal-related values are still
# calculated for these animals, they are not to be used.
feedN4meat = array(0,c(n_meats,n_crops,nyrs))
feedP4meat = array(0,c(n_meats,n_crops,nyrs))
animNreqmeat = array(0,c(n_meats,nyrs))
animPreqmeat = array(0,c(n_meats,nyrs)) 
# this is calculated to create animNreqpermeat,
# used in meattrade to estimate animNreqs -
# manure for FF calcs in WW (waste wedge) and
# PG (protein gap)
# calculations
animNreqpermeat = array(0,c(n_meats,nyrs)) # total animal N requirements for each meat category divided by all meat produced in the same year
animPreqpermeat = array(0,c(n_meats,nyrs)) # total animal P requirements for each meat category divided by all meat produced in the same year
totfeed4meat = array(0,c(n_meats,n_crops,nyrs))
totfeed4anim = array(0,c(n_meats,n_crops,nyrs))
chngC4animN = array(0,c(n_crops,nyrs))
chngC4animP = array(0,c(n_crops,nyrs))
C4animNadj = array(0,c(n_ws_NEEA,n_crops,nyrs)) #hold revised N in crops needed to meet anim N reqs by watershed
C4animPadj = array(0,c(n_ws_NEEA,n_crops,nyrs)) #hold revised N in crops needed to meet anim P reqs by watershed
fertNpermeat = array(0,c(n_meats,n_crops,nyrs)) # kg fert (applied to crops) per kg of edible animal product (based on amount of crop assumed to be consumed per animal)
fertPpermeat = array(0,c(n_meats,n_crops,nyrs)) # kg fert (applied to crops) per kg of edible animal product (based on amount of crop assumed to be consumed per animal)
fertNperanim = array(0,c(n_anims,n_crops,nyrs))
fertPperanim = array(0,c(n_anims,n_crops,nyrs))
fertNanimtot = array(0,c(n_anims,n_crops,nyrs))
fertPanimtot = array(0,c(n_anims,n_crops,nyrs))
fixNanimtot = array(0,c(n_anims,n_crops,nyrs))
fixNmeattot = array(0,c(n_meats,n_crops,nyrs))
fixNpermeat = array(0,c(n_meats,n_crops,nyrs))
N2O100permeat = array(0,c(n_meats,n_crops,nyrs)) # 100 GWP value from Shindell
CH4100permeat = array(0,c(n_meats,n_crops,nyrs)) # 100 GWP value from Shindell
N2O20permeat = array(0,c(n_meats,n_crops,nyrs)) # 100 GWP value from Shindell
CH420permeat = array(0,c(n_meats,n_crops,nyrs)) # 100 GWP value from Shindell
LUforfeedpermeat = array(0,c(n_meats,n_crops,nyrs))
NH3meat = array(0,c(n_ws_NEEA,n_meats,nyrs))
NH3permeat = array(0,c(nyrs,n_meats))
NH3perMkcal = array(0,c(n_meats,nyrs))
NH3perMprot = array(0,c(n_meats,nyrs))
manureNmeat = array(0,c(n_ws_NEEA,n_meats,nyrs))
manurePmeat = array(0,c(n_ws_NEEA,n_meats,nyrs))
manureNmeattot = array(0,c(nyrs,n_meats))
manurePmeattot = array(0,c(nyrs,n_meats))
manureNpermeat = array(0,c(nyrs,n_meats))
manureNperkcal = array(0,c(n_meats,nyrs))
manureNperprot = array(0,c(n_meats,nyrs))
manurePpermeat = array(0,c(nyrs,n_meats))
manurePperkcal = array(0,c(n_meats,nyrs))
manurePperprot = array(0,c(n_meats,nyrs))
kganimPintakefromC = array(0,c(n_ws_NEEA,n_anims,nyrs))

#new (7/21/20), estimates of P supplementation based on mineral P availability and animal P needs
Psupp4meat = array(0,c(n_meats,nyrs))
Psupp_permeat = array(0,c(n_meats,nyrs))
Psupp_perkcal = array(0,c(n_meats,nyrs))
Psupp_perprot = array(0,c(n_meats,nyrs))

Psupp4anim = Psupp_animtotals

# impact re-allocation to account for dairy industry beef production (not yet used for anything)
dairycont = 0.2 # proportion of beef that comes from the dairy industry in the US
meatprottot = meatprod*meatdata[,8]
dairyindprot = meatprottot[1,]*dairycont+meatprottot[2,]
dairybeef_factor = meatprottot[1,]*dairycont/dairyindprot
dairymilk_factor = meatprottot[2,]/dairyindprot


#Costello's (1) or DeVries' (2) protein assumptions for meat?
if(protassump == 1){
  z = 8
}else if(protassump == 2){
  z = 9
}

for(n in 1:nyrs){
  animNfromC[,,n] = cropNtoanim[,,n]
  animPfromC[,,n] = cropPtoanim[,,n]
  
  #sum crop N for animals for each animal product category over all watersheds
  C4animNtot[,n] = t(colSums(C4animN[,,n]))
  
  #sum crop P for animals for each animal product category over all watersheds
  C4animPtot[,n] = t(colSums(C4animP[,,n]))
  
  #sum meat, NH3, and manure N and P for each category over all of the watersheds
  totmeat[n,] = colSums(kgmeat[,,n]) # domestically produced, these estimates are from USDA data
  totNH3[n,] = colSums(kgNH3[,,n])
  totmanureN[n,] = colSums(kgmanureN[,,n])
  totmanureP[n,] = colSums(kgmanureP[,,n])
  
  for(i in 1:n_anims){
    feedN4anim[i,,n] = animNfromC[i,,n] * totnoanimws[n,i]
    feedP4anim[i,,n] = animPfromC[i,,n] * totnoanimws[n,i]
    kganimPintakefromC[,i,n] = sum(animPfromC[i,,n]) * noanimwsdyn[,i,n]
    #this is not the same as
    #kganimreqs, because it's based not on the anim P intake inputs,
    #but on the modeled crop and P supplement allocations to animals
  }
  
  
  for(i in 1:n_crops){
    # kg crop for each animal
    feed4anim[,i,n] = feedN4anim[,i,n] / NperC[i] #total amount of feed for all animals
    feed4animP[,i,n] = feedP4anim[,i,n] / PperC[i] #total amount of feed for all animals ->> use as a check?
  }
  
  
  for(i in 1:n_anims){
    feedperanim[i,,n] = feed4anim[i,,n] / sum(noanimwsdyn[,i,n]) # feed for each animal in a year
  }
  feedperanim[16,,n] = 0 # zero feed for dairy stockers
  
  feedN4meat[1,,n] = (((feedN4anim[1,,n] + feedN4anim[10,,n]) + feedN4anim[11,,n]) + feedN4anim[13,,n]) + feedN4anim[15,,n]
  feedN4meat[2,,n] = ((feedN4anim[2,,n] + feedN4anim[12,,n]) + feedN4anim[14,,n]) + feedN4anim[16,,n]
  feedN4meat[3,,n] = feedN4anim[3,,n] + feedN4anim[4,,n]
  feedN4meat[4,,n] = feedN4anim[17,,n] #sheep, feedN4anim(17,,n]
  feedN4meat[5,,n] = feedN4anim[18,,n] #horses...maybe make 0 also?
  feedN4meat[6,,n] = feedN4anim[5,,n]
  feedN4meat[7,,n] = feedN4anim[7,,n] + feedN4anim[8,,n]
  feedN4meat[8,,n] = feedN4anim[6,,n] + feedN4anim[9,,n]
  feedN4meat[9,,n] = feedN4anim[19,,n] #goats, feedN4anim[19,,n]
  
  feedP4meat[1,,n] = (((feedP4anim[1,,n] + feedP4anim[10,,n]) + feedP4anim[11,,n]) + feedP4anim[13,,n]) + feedP4anim[15,,n]
  feedP4meat[2,,n] = ((feedP4anim[2,,n] + feedP4anim[12,,n]) + feedP4anim[14,,n]) + feedP4anim[16,,n]
  feedP4meat[3,,n] = feedP4anim[3,,n] + feedP4anim[4,,n]
  feedP4meat[4,,n] = feedP4anim[17,,n] #sheep, feedP4anim(17,,n]
  feedP4meat[5,,n] = feedP4anim[18,,n] #horses...maybe make 0 also?
  feedP4meat[6,,n] = feedP4anim[5,,n]
  feedP4meat[7,,n] = feedP4anim[7,,n] + feedP4anim[8,,n]
  feedP4meat[8,,n] = feedP4anim[6,,n] + feedP4anim[9,,n]
  feedP4meat[9,,n] = feedP4anim[19,,n] #goats, feedP4anim[19,,n]
  
  Psupp4meat[1,n] = (((Psupp4anim[1,n] + Psupp4anim[10,n]) + Psupp4anim[11,n]) + Psupp4anim[13,n]) + Psupp4anim[15,n]
  Psupp4meat[2,n] = ((Psupp4anim[2,n] + Psupp4anim[12,n]) + Psupp4anim[14,n]) + Psupp4anim[16,n]
  Psupp4meat[3,n] = Psupp4anim[3,n] + Psupp4anim[4,n]
  Psupp4meat[4,n] = Psupp4anim[17,n] #sheep
  Psupp4meat[5,n] = Psupp4anim[18,n] #horses...maybe make 0 also?
  Psupp4meat[6,n] = Psupp4anim[5,n]
  Psupp4meat[7,n] = Psupp4anim[7,n] + Psupp4anim[8,n]
  Psupp4meat[8,n] = Psupp4anim[6,n] + Psupp4anim[9,n]
  Psupp4meat[9,n] = Psupp4anim[19,n] #goats
  
  for(i in 1:n_meats){
    if(totmeat[n,i]>0){
      feedNpermeat[i,,n] = feedN4meat[i,,n] / totmeat[n,i] # normalized by meat production
      animNreqpermeat[i,n] = animNreqmeat[i,n] / totmeat[n,i] # kg anim req / kg meat
      feedPpermeat[i,,n] = feedP4meat[i,,n] / totmeat[n,i] # normalized by meat production
      animPreqpermeat[i,n] = animPreqmeat[i,n] / totmeat[n,i] # kg anim req / kg meat
    }else{ # avoid Inf and NaN results
      feedNpermeat[i,,n] = 0
      animNreqpermeat[i,n] = 0
      feedPpermeat[i,,n] = 0
      animPreqpermeat[i,n] = 0
    }
  }
  
  feedNpermeat[5,,n] = 0  # horses. redundant?
  feedPpermeat[5,,n] = 0  # horses. redundant?
  
  for(i in 1:n_crops){
    feedpermeat[,i,n] = feedNpermeat[,i,n] / NperC[i] # feed per kg meat
    #feedpermeat[,i,n] = feedPpermeat[,i,n] / PperC[i] # feed per kg meat ->>Check?
  }
  
  feedpermeat[5,,n] = 0 # redundant?
  animNreqpermeat[5,n] = 0 # redundant?
  animPreqpermeat[5,n] = 0 # redundant?
  
  for(i in 1:n_meats){
    # kg crop for feed to produce all meat of each type
    totfeed4meat[i,,n] = feedpermeat[i,,n] * totmeat[n,i]
  }
  
  #zero for sheep, horses, and goats
  totfeed4meat[4,,n]=0
  totfeed4meat[5,,n]=0 # redundant?
  totfeed4meat[9,,n]=0
  
  #(4.15.13) adjust the C4animN using the calculated feed N for animals in
  #order to balance animal N requirements with feed intake
  chngC4animN[,n] = t(colSums(feedN4anim[,,n]))/t(colSums(C4animN[,,n]))
  chngC4animP[,n] = t(colSums(feedP4anim[,,n]))/t(colSums(C4animP[,,n]))
  
  for(i in 1:length(chngC4animN[,1])){
    if(is.nan(chngC4animN[i,n])||is.infinite(chngC4animN[i,n])){ #if it's NaN, the numerator is zero. if it's infinity, the denominator is zero.
      chngC4animN[i,n]=0 #in both of these cases, the adjustment factor should be zero (CHECK w/ Chris)
    }
    C4animNadj[,i,n] = C4animN[,i,n]*chngC4animN[i,n]
  }
  
  for(i in 1:length(chngC4animP[,1])){
    if(is.nan(chngC4animP[i,n])||is.infinite(chngC4animP[i,n])){ #if it's NaN, the numerator is zero. if it's infinity, the denominator is zero.
      chngC4animP[i,n]=0 #in both of these cases, the adjustment factor should be zero (CHECK w/ Chris)
    }
    C4animPadj[,i,n] = C4animP[,i,n]*chngC4animP[i,n]
  }
  
  for(i in 1:n_crops){
    fertNpermeat[,i,n] = feedpermeat[,i,n] * unitfertNC[i,n]
    fertNperanim[,i,n] = feedperanim[,i,n] * unitfertNC[i,n]
    fertNanimtot[,i,n] = feed4anim[,i,n] * unitfertNC[i,n]
    fertPpermeat[,i,n] = feedpermeat[,i,n] * unitfertPC[i,n]
    fertPperanim[,i,n] = feedperanim[,i,n] * unitfertPC[i,n]
    fertPanimtot[,i,n] = feed4anim[,i,n] * unitfertPC[i,n]
    N2O100permeat[,i,n] = feedpermeat[,i,n] * cropdata[i,14] #2007 values are actually 2002 GHG values, need to pull 2007 data?
    CH4100permeat[,i,n] = feedpermeat[,i,n] * cropdata[i,15] #2007 values are actually 2002 GHG values, need to pull 2007 data?
    N2O20permeat[,i,n] = feedpermeat[,i,n] * cropdata[i,17] #2007 values are actually 2002 GHG values, need to pull 2007 data?
    CH420permeat[,i,n] = feedpermeat[,i,n] * cropdata[i,18]
    LUforfeedpermeat[,i,n] = feedpermeat[,i,n] * cropdata[i,16] #check these cropdata numbers
    fixNanimtot[,i,n] = feed4anim[,i,n] * unitfixNC[i,n]
    fixNmeattot[,i,n] = totfeed4meat[,i,n] * unitfixNC[i,n]
    fixNpermeat[,i,n] = feedpermeat[,i,n] * unitfixNC[i,n]
  }
  
  # NH3 and manure N per meat produced
  NH3meat[,1,n] = (((kgNH3[,1,n] + kgNH3[,10,n]) + kgNH3[,11,n]) + kgNH3[,13,n]) + kgNH3[,15,n]
  NH3meat[,2,n] = ((kgNH3[,2,n] + kgNH3[,12,n]) + kgNH3[,14,n]) + kgNH3[,16,n]
  NH3meat[,3,n] = kgNH3[,3,n] + kgNH3[,4,n]
  NH3meat[,4,n] = 0
  NH3meat[,5,n] = 0
  NH3meat[,6,n] = kgNH3[,5,n]
  NH3meat[,7,n] = kgNH3[,7,n] + kgNH3[,8,n]
  NH3meat[,8,n] = kgNH3[,6,n] + kgNH3[,9,n]
  NH3meat[,9,n] = 0
  
  for(i in 1:length(totmeat[n,])){
    if(totmeat[n,i]>0){
      NH3permeat[n,] = sum(NH3meat[i,,n]) / totmeat[n,i] # kg N from NH3 emissions per kg of meat by animal
    }else{ # avoid inf
      NH3permeat[n,] = 0
    }
  }
  NH3permeat[n,5] = 0
  for(i in 1:length(meatdata[n,])){
    if(meatdata[i,3]>0){ #if the edible portion is > 0
      NH3perMkcal[i,n] = t(NH3permeat[n,i]) / (meatdata[i,7])
      NH3perMprot[i,n] = t(NH3permeat[n,i]) / (meatdata[i,z] / 1000) #per kilogram of protein
    }else{ # avoid inf
      NH3perMkcal[i,n] = 0
    }
  }
  NH3perMkcal[5,n] = 0 #horses
  NH3perMprot[5,n] = 0 #horses
  
  manureNmeat[,1,n] = (((kgmanureN[,1,n] + kgmanureN[,10,n]) + kgmanureN[,11,n]) + kgmanureN[,13,n]) + kgmanureN[,15,n]
  manureNmeat[,2,n] = ((kgmanureN[,2,n] + kgmanureN[,12,n]) + kgmanureN[,14,n]) + kgmanureN[,16,n]
  manureNmeat[,3,n] = kgmanureN[,3,n] + kgmanureN[,4,n]
  manureNmeat[,4,n] = 0
  manureNmeat[,5,n] = 0
  manureNmeat[,6,n] = kgmanureN[,5,n]
  manureNmeat[,7,n] = kgmanureN[,7,n] + kgmanureN[,8,n]
  manureNmeat[,8,n] = kgmanureN[,6,n] + kgmanureN[,9,n]
  manureNmeat[,9,n] = 0
  manureNmeattot[n,] = colSums(manureNmeat[,,n])
  
  manurePmeat[,1,n] = (((kgmanureP[,1,n] + kgmanureP[,10,n]) + kgmanureP[,11,n]) + kgmanureP[,13,n]) + kgmanureP[,15,n]
  manurePmeat[,2,n] = ((kgmanureP[,2,n] + kgmanureP[,12,n]) + kgmanureP[,14,n]) + kgmanureP[,16,n]
  manurePmeat[,3,n] = kgmanureP[,3,n] + kgmanureP[,4,n]
  manurePmeat[,4,n] = 0
  manurePmeat[,5,n] = 0
  manurePmeat[,6,n] = kgmanureP[,5,n]
  manurePmeat[,7,n] = kgmanureP[,7,n] + kgmanureP[,8,n]
  manurePmeat[,8,n] = kgmanureP[,6,n] + kgmanureP[,9,n]
  manurePmeat[,9,n] = 0
  manurePmeattot[n,] = colSums(manurePmeat[,,n])
  
  for(i in 1:length(totmeat[n,])){
    if(totmeat[n,i]>0){
      manureNpermeat[n,i] = sum(manureNmeat[,i,n]) / totmeat[n,i] #kg N in manure per kg of meat by animal
      manurePpermeat[n,i] = sum(manurePmeat[,i,n]) / totmeat[n,i] #kg P in manure per kg of meat by animal
      Psupp_permeat[i,n] = Psupp4meat[i,n] / totmeat[n,i] #kg P supplement per kg meat product
    }else{
      manureNpermeat[n,i] = 0
      manurePpermeat[n,i] = 0
      Psupp_permeat[i,n] = 0
    }
  }
  manureNpermeat[n,5] = 0 #horses
  manurePpermeat[n,5] = 0 #horses
  
  # calc manure per kcal and g protein
  for(i in 1:n_meats){
    if(meatdata[i,3]>0){ #if edible portion is > 0
      manureNperkcal[i,n] = (manureNpermeat[n,i] / meatdata[i,7]) # (kg manure N/kg meat) *(kg meat/kcal)= kg manure/kcal in 1 kg meat
      manureNperprot[i,n] = (manureNpermeat[n,i] / (meatdata[i,z] / 1000)) # (kg manure N per kg meat/kg prot per kg meat)  
      manurePperkcal[i,n] = (manurePpermeat[n,i] / meatdata[i,7]) # (kg manure P/kg meat) *(kg meat/kcal)= kg manure/kcal in 1 kg meat
      manurePperprot[i,n] = (manurePpermeat[n,i] / (meatdata[i,z] / 1000)) # (kg manure P per kg meat/kg prot per kg meat) 
      Psupp_perkcal[i,n] = Psupp_permeat[i,n] / meatdata[i,7]
      Psupp_perprot[i,n] = Psupp_permeat[i,n] / (meatdata[i,z] / 1000)
    }else{
      manureNperkcal[i,n] = 0
      manureNperprot[i,n] = 0
      manurePperkcal[i,n] = 0
      manurePperprot[i,n] = 0
      Psupp_perkcal[i,n] = 0
      Psupp_perprot[i,n] = 0
    }
  }
  manureNperkcal[5,n] = 0 #horses
  manureNperprot[5,n] = 0 #horses
  manurePperkcal[5,n] = 0 #horses
  manurePperprot[5,n] = 0 #horses
}