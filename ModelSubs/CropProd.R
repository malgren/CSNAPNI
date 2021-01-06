# CropProd.R
# This script calculates crop production and export vectors

#allocate space to matrices
CkgwswE = array(0,c(n_ws_NEEA,n_crops,nyrs))
Ckgws = array(0,c(n_ws_NEEA,n_crops,nyrs)) # domestically available total (i.e., does not include exported quantities)
sumCkgwE = array(0,c(n_crops,nyrs))
sumCkgwsnoE = array(0,c(n_crops,nyrs))
cornexp = array(0,c(1,nyrs))
chngEC = array(0,c(n_crops,nyrs))          
expamt = array(0,c(n_crops,nyrs)) # percent crop exported in each year
Cexpkg = array(0,c(n_ws_NEEA,n_crops,nyrs))
CNexpkg = array(0,c(n_ws_NEEA,n_crops,nyrs))
CNkgwswE = array(0,c(n_crops,nyrs))
totCNexpkg = array(0,c(n_ws_NEEA,nyrs)) # total crop N exported from each watershed
CPexpkg = array(0,c(n_ws_NEEA,n_crops,nyrs))
CPkgwswE = array(0,c(n_crops,nyrs))
totCPexpkg = array(0,c(n_ws_NEEA,nyrs)) # total crop P exported from each watershed

for(i in 1:nyrs){
  for(n in 1:n_crops){
    CkgwswE[,n,i] = CkgwswE_orig[,n,i]
  }
  sumCkgwE[,i] = colSums(CkgwswE[,,i]) #total crop production with exports
  sumcornprodnoetoh = colSums(cornprodnoetoh) #corn production totals without the removal of corn for etoh and coproducts
  cornexp[i] = sumcornprodnoetoh[i] * cornuse[10,i] #calculate "better" estimate of corn exports
  exports[1,i] = cornexp[i]  #updates the corn export values with "better" data
  for(n in 1:n_crops){
      sumCkgwsnoE[n,i] = sumCkgwE[n,i] - exports[n,i] #kg crop minus exported quantities
  }
  chngEC[,i] = sumCkgwsnoE[,i] / sumCkgwE[,i]  # percent of crop not exported
  for(n in 1:n_crops){
      if(sumCkgwsnoE[n,i]==0){
          chngEC[n,i]=0  # prevent NaN
      }
      Ckgws[,n,i] = CkgwswE[,n,i] * chngEC[n,i] # crops available for domestic use for corn,
                                                        # this value represents the
                                                        # amount of corn N available
                                                        # in the US, incl corn for
                                                        # ethanol, sugar, oil, etc.
  }
  expamt[,i] = array(1,c(n_crops,1)) - chngEC[,i]      # percent of crop exported each year
}

for(i in 1:nyrs){
    Cexpkg[,,i] = CkgwswE[,,i] %*% diag(expamt[,i]) #kg of crop exported "by watershed" assume 
                                                        # equal contribution from each,
                                                        # these are calculated on the
                                                        # amount of crop following the
}

Nin_crops = cropdata[,1] * cropdata[,2] # percent N in each crop (% DM) * (% N in DM)
NperC=Nin_crops

Pin_crops = cropdata[,1] * cropdata[,3] # percent P in each crop (% DM) * (% P in DM)
PperC=Pin_crops

for(i in 1:nyrs){
    CNexpkg[,,i] = Cexpkg[,,i] %*% diag(NperC)
    CPexpkg[,,i] = Cexpkg[,,i] %*% diag(PperC)
    
    # N in total crop production
    CNkgwswE[,i] = t(colSums(CkgwswE[,,i])) * NperC # includes exports, but not the initial "waste/loss"
    totCNexpkg[,i] = rowSums(CNexpkg[,,i],2) # total crop N exported from each watershed
    
    # P in total crop production
    CPkgwswE[,i] = t(colSums(CkgwswE[,,i])) * PperC # includes exports, but not the initial "waste/loss"
    totCPexpkg[,i] = rowSums(CPexpkg[,,i]) # total crop P exported from each watershed
}