# NEEAshedsdyn.R
## This script sorts the 450 NANI Toolbox watersheds into 144 NEEA watersheds

#allocate matrix space
CkgwswE_orig = array(0,c(n_ws_NEEA,n_crops,nyrs))  # variable to hold the NANI values that correspond to
# the watersheds in the NEEA database 
# wE = includes exported quantity
# 4.3.15 added 3 columns to accommodate new
# feeds from etoh production
etohL_orig = array(0,c(n_ws_NEEA,nyrs))
CkgwE = array(0,c(n_ws_tbx,n_crops,nyrs))         # 4.3.15 added 3 columns to accommodate new
# feeds from etoh production
etohL = array(0,c(n_ws_tbx,nyrs))
areaws = array(0,c(n_ws_NEEA,1))
NEEAws_NANI = array(0,c(n_ws_NEEA,15,nyrs))        # variable to hold the NANI values that correspond to the watersheds in the NEEA database
NEEAws_NAPI = array(0,c(n_ws_NEEA,7,nyrs))        # variable to hold the NAPI values that correspond to the watersheds in the NEEA database
popws = array(0,c(n_ws_NEEA,nyrs))            # variable to hold populations col1, 2002 col2 2007
noanimwsdyn = array(0,c(n_ws_NEEA,n_anims,nyrs))   # variable to hold the NANI values that correspond to the watersheds in the NEEA database
kgmanureNrecws = array(0,c(n_ws_NEEA,n_anims,nyrs)) #***no 2007 data for this yet****    
kgmanurePrecws = array(0,c(n_ws_NEEA,n_anims,nyrs)) #***no 2007 data for this yet****  
  
for(i in 1:nyrs){
  CkgwE[,,i] = array(area,c(n_ws_tbx,19))*cropprod[,,i] # calculates kg of crop in each watershed
  etohL[,i] = array(area,c(n_ws_tbx))*etohproddensws[,i] # calculates effective L of ethanol in each watershed
  for(w in 1:n_ws_tbx){
    n=wsNum[w,1]
    if(i==1){ # only calculate the area once -- the area will otherwise end up being area*nyrs for each watershed
      areaws[n,] = areaws[n,] + area[w,]
    }
    NEEAws_NANI[n,,i] = NEEAws_NANI[n,,i] + NANIdata[w,,i]*area[w]
    NEEAws_NAPI[n,,i] = NEEAws_NAPI[n,,i] + NAPIdata[w,,i]*area[w]
    noanimwsdyn[n,,i] = noanimwsdyn[n,,i] + noanimdyn[w,,i] #doesn't need to be area weighted since these are total # of animals
    CkgwswE_orig[n,,i] = CkgwswE_orig[n,,i] + CkgwE[w,,i]
    etohL_orig[n,i] = etohL_orig[n,i] + etohL[w,i]
    if(i==1){ # only calculate the pop once? why don't we have population data for each year??
      popws[n,] = popws[n,] + population[w,]*area[w]
    }
    kgmanureNrecws[n,,i] = kgmanureNrecws[n,,i] + kgmanureNrec450[w,,i]
    kgmanurePrecws[n,,i] = kgmanurePrecws[n,,i] + kgmanurePrec450[w,,i]
  }
}

## Calculate per-km^2 NANI and NAPI components from NEEAws_NANI and areaws
newNANIws = array(0,c(n_ws_NEEA,15,nyrs))
totNANIws = array(0,c(n_ws_NEEA,15,nyrs))
newNAPIws = array(0,c(n_ws_NEEA,7,nyrs))
totNAPIws = array(0,c(n_ws_NEEA,7,nyrs))

for(n in 1:nyrs){
  newNANIws[,,n] = NEEAws_NANI[,,n]/array(areaws,c(length(areaws),length(NEEAws_NANI[1,,n]))) 
  totNANIws[,,n] = newNANIws[,,n]*array(areaws,c(length(areaws),length(NEEAws_NANI[1,,n]))) #total kg N values for NANI values
  newNAPIws[,,n] = NEEAws_NAPI[,,n]/array(areaws,c(length(areaws),length(NEEAws_NAPI[1,,n]))) 
  totNAPIws[,,n] = newNAPIws[,,n]*array(areaws,c(length(areaws),length(NEEAws_NAPI[1,,n]))) #total kg P values for NANI values
}