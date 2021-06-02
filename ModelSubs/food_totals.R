#food_totals.R
#calculate meat/animal and crop N and P contents 
#and quantities of crops for humans and animals

if(print_tags == 1){
  print("ModelSubs/food_totals.R")
}

##Variables for livestock, not adjusted for trade
kganim = array(0,c(n_ws_NEEA,n_anims,nyrs))        # live weight
kgmeat_old = array(0,c(n_ws_NEEA,n_anims,nyrs))    # edible weight, calculated using assumptions in NANI, 
#ended up using direct reports from USDA
#to reduce uncertainty
kgNmeat_old = array(0,c(n_ws_NEEA,n_anims,nyrs))   # N in edible weight
kgNH3 = array(0,c(n_ws_NEEA,n_anims,nyrs))         # kg N-NH3 from various livestock
kgmanureN = array(0,c(n_ws_NEEA,n_anims,nyrs))      # N in manure from livestock
kgmanureP = array(0,c(n_ws_NEEA,n_anims,nyrs))      # P in manure from livestock
kganimNreqs = array(0,c(n_ws_NEEA,n_anims,nyrs))   # N required for each animal (kg)
kganimPreqs = array(0,c(n_ws_NEEA,n_anims,nyrs))   # P required for each animal (kg)
sumkganimNreqs = array(0,c(n_ws_NEEA,nyrs))
sumkganimPreqs = array(0,c(n_ws_NEEA,nyrs))
animN = array(0,c(n_ws_NEEA,n_anims,nyrs))
animP = array(0,c(n_ws_NEEA,n_anims,nyrs))

##Variables for crops
CNprod = array(0,c(n_ws_NEEA,n_crops,nyrs))        # N in crops produced (kg)
CPprod = array(0,c(n_ws_NEEA,n_crops,nyrs))        # P in crops produced (kg)
C4human = array(0,c(n_ws_NEEA,n_crops,nyrs))       # amount of crop for human food, kg
C4humanN = array(0,c(n_ws_NEEA,n_crops,nyrs))      # amount of N in crops for humans, kg N
C4humanP = array(0,c(n_ws_NEEA,n_crops,nyrs))      # amount of P in crops for humans, kg P
C4animkg = array(0,c(n_ws_NEEA,n_crops,nyrs))      # amount of crops for animals (kg)
C4animN = array(0,c(n_ws_NEEA,n_crops,nyrs))       # amount of N in crops for animals
C4animP = array(0,c(n_ws_NEEA,n_crops,nyrs))       # amount of P in crops for animals
sumC4animkg = array(0,c(n_crops,nyrs))
sumCkgws = array(0,c(nyrs,n_crops))                      # total kg of each crop produced
sumC4human = array(0,c(nyrs,n_crops))                    # total kg of each crop consumed by humans


for(n in 1:nyrs){
  for(j in 1:n_anims){
    kgNH3[,j,n] = noanimwsdyn[,j,n] * animdatadyn[j,12] #kg N/year
    kgmanureN[,j,n] = noanimwsdyn[,j,n] * animdatadyn[j,10] 
    kgmanureP[,j,n] = noanimwsdyn[,j,n] * animdatadyn[j,11] 
    kganimNreqs[,j,n] = noanimwsdyn[,j,n] * animdatadyn[j,8] #in kg N/year
    kganimPreqs[,j,n] = noanimwsdyn[,j,n] * animdatadyn[j,9] #in kg P/year
    animN[,j,n] = kganimNreqs[,j,n] - kgmanureN[,j,n] # approx N in animal products using orig assumption
    # this assumption doesn't work for P because P is prevalent outside of proteins (e.g., in teeth and bones), and animN and animP are used in the FF calculation
    #animP[,j,n] = kganimPreqs[,j,n] - kgmanureP[,j,n] # approx P in animal bodies using orig assumption
    kganim[,j,n] = noanimwsdyn[,j,n] * animdatadyn[j,14] #kg of liveweight per animals assumed to be slaughtered per watershed
    kgmeat_old[,j,n] = kganim[,j,n] * animdatadyn[j,15] # kg animal * # edible = edible portion,compare to USDA meat production
    kgNmeat_old[,j,n] = kgmeat_old[,j,n] * animdatadyn[j,16] #not replicating this calculation for P
    animP[,j,n] = kgmeat_old[,j,n] * animdatadyn[j,16]*1/16 # approx P in animal bodies
  }
  
  for(k in 1:n_crops){
    CNprod[,k,n] = Ckgws[,k,n] * cropdata[k,1] * cropdata[k,2] # kg N in crops, remember that Ckgws does not include exported quantity
    C4humanN[,k,n] = CNprod[,k,n] * cropdata[k,4]
    C4animN[,k,n] = CNprod[,k,n] * cropdata[k,5]
    CPprod[,k,n] = Ckgws[,k,n] * cropdata[k,1] * cropdata[k,3] # kg N in crops remember that Ckgws02 does not include exported quantity
    C4humanP[,k,n] = CPprod[,k,n] * cropdata[k,4]
    C4animP[,k,n] = CPprod[,k,n] * cropdata[k,5]
    C4human[,k,n] = Ckgws[,k,n] * cropdata[k,4]    # doesn't include exports
    C4animkg[,k,n] = Ckgws[,k,n] * cropdata[k,5]   # doesn't include  ] of bulk crops, but does include feed for meat exports
  }
  
  sumC4animkg[,n] = t(colSums(C4animkg[,,n]))
  sumCkgws[n,] = colSums(Ckgws[,,n])                    # total kg of each crop produced
  sumC4human[n,] = colSums(C4human[,,n])                # total kg of each crop consumed by humans
  sumkganimNreqs[,n] = rowSums(kganimNreqs[,,n])
  sumkganimPreqs[,n] = rowSums(kganimPreqs[,,n])
}