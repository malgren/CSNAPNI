## DETERMINE HUMAN N and P REQUIREMENTS

if(print_tags == 1){
  print("ModelSubs/hmn_reqs.R")
}

#allocate matrix space
totnoanimws = array(0,c(nyrs,n_anims))      # total number of animals of each type in each year?
tothmnNreqs = array(0,c(n_ws_NEEA,nyrs))     # total kg N required assuming available is consumed
tothmnfoodPreqs = array(0,c(n_ws_NEEA,nyrs))     # total kg P required assuming available is consumed
tothmndetPreqs = array(0,c(n_ws_NEEA,nyrs))     # total kg P required assuming available is consumed
hmnNreqs = array(0,c(n_ws_NEEA,nyrs))         # total kg N per km2 version for FF calcs
hmnfoodPreqs = array(0,c(n_ws_NEEA,nyrs))         # total kg P per km2 version for FF calcs
hmndetPreqs = array(0,c(n_ws_NEEA,nyrs))         # total kg P per km2 version

prot_per_hmn_per_day = c(121,123,123,120,120) #grams of protein available per person per day in the US (1997,2002,2007,2010,2010)
#from https://www.ers.usda.gov/data-products/food-availability-per-capita-data-system/
#no data post-2010

#annual human N consumption (kg)
N_per_prot = 0.16  #(Han and Allen 2008) N consumption
Nperhmn=prot_per_hmn_per_day*N_per_prot*365/1000 #(in kg/person/year)

#annual human P consumption (kg)
P_per_prot = 0.013 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4361095/#:~:text=The%20direct%20relationship%20between%20protein,protein%20%5B4%2C%205%5D.
foodPperhmn=prot_per_hmn_per_day*P_per_prot*365/1000 #(in kg/person/year)
detPperhmn=c(0.62,0.62,0.62,0.43,0.43) #(in kg/person/year)

for(i in 1:nyrs){
  totnoanimws[i,] = colSums(noanimwsdyn[,,i]) #total number of each type of animal?
  tothmnNreqs[,i] = popws[,i]*Nperhmn[i] # total kg N required assuming available is consumed (not eaten, necessarily--this includes food waste)
  hmnNreqs[,i] = tothmnNreqs[,i]/areaws #per km2 version for FF calcs
  tothmnfoodPreqs[,i] = popws[,i]*foodPperhmn[i] # total kg P required assuming available is consumed (not eaten, necessarily--this includes food waste)
  tothmndetPreqs[,i] = popws[,i]*detPperhmn[i] # halved in 2012 and 2017 because of phosphate ban in 17 us states
  hmnfoodPreqs[,i] = tothmnfoodPreqs[,i]/areaws #per km2 version for FF calcs
  hmndetPreqs[,i] = tothmndetPreqs[,i]/areaws #per km2 version
}