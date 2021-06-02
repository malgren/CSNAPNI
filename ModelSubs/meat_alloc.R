#meat_alloc.R
##uses the average annual populations of 19 animal types to estimate
##production quantities of beef, pork, milk, eggs, chicken, and turkey

if(print_tags == 1){
  print("ModelSubs/meat_alloc.R")
}

#allocate matrix space
kgmeatdyn = array(0, c(n_ws_NEEA, n_anims, nyrs))
kgmeat = array(0, c(n_ws_NEEA, n_meats, nyrs))
meatN = array(0, c(n_meats, nyrs))
meatP = array(0, c(n_meats, nyrs))

for (n in 1:nyrs) {
  #implied N and P in meat using NANI and NAPI's N and P content assumptions
  meatN[, n] = meatprod[, n] * meatdata[, 4]
  if(protassump==1){
    meatP[, n] = meatprod[, n] * meatdata[, 5] #use Costello 2015 protein content assumptions
  }else if(protassump==2){
    meatP[, n] = meatprod[, n] * meatdata[, 6] #use de Vries' protein content assumptions
  }
  for (i in 1:n_ws_NEEA) {
    # beef
    kgmeatdyn[i, 1, n] = (noanimwsdyn[i, 1, n] / totnoanimws[n, 1]) * meatprod[1, n]
    kgmeat[i, 1, n] = (noanimwsdyn[i, 1, n] / totnoanimws[n, 1]) * meatprod[1, n]
    
    # dairy
    kgmeatdyn[i, 2, n] = (noanimwsdyn[i, 2, n] / totnoanimws[n, 2]) * meatprod[2, n]
    kgmeat[i, 2, n] = (noanimwsdyn[i, 2, n] / totnoanimws[n, 2]) * meatprod[2, n]
    
    # hogs for breeding
    kgmeatdyn[i, 3, n] = 0
    
    # hogs for slaughter
    kgmeatdyn[i, 4, n] = (noanimwsdyn[i, 4, n] / totnoanimws[n, 4]) * meatprod[3, n]
    kgmeat[i, 3, n] = (noanimwsdyn[i, 4, n] / totnoanimws[n, 4]) * meatprod[3, n]
    
    # chicken layers
    kgmeatdyn[i, 5, n] = (noanimwsdyn[i, 5, n] / totnoanimws[n, 5]) * meatprod[6, n]
    kgmeat[i, 6, n] = (noanimwsdyn[i, 5, n] / totnoanimws[n, 5]) * meatprod[6, n]
    
    # breeding turkeys
    kgmeatdyn[i, 6, n] = 0
    
    # chicken pullets
    kgmeatdyn[i, 7, n] = 0
    
    # chicken broilers
    kgmeatdyn[i, 8, n] = (noanimwsdyn[i, 8, n] / totnoanimws[n, 8]) * meatprod[7, n]
    kgmeat[i, 7, n] = (noanimwsdyn[i, 8, n] / totnoanimws[n, 8]) * meatprod[7, n]
    
    # slaughter turkeys
    kgmeatdyn[i, 9, n] = (noanimwsdyn[i, 9, n] / totnoanimws[n, 9]) * meatprod[8, n]
    kgmeat[i, 8, n] = (noanimwsdyn[i, 9, n] / totnoanimws[n, 9]) * meatprod[8, n]
    
    # beef breeding herd
    kgmeatdyn[i, 10, n] = 0
    
    # beef calves
    kgmeatdyn[i, 11, n] = 0 #put veal in?
    
    # dairy calves
    kgmeatdyn[i, 12, n] = 0
    
    # beef heifers
    kgmeatdyn[i, 13, n] = 0
    
    # dairy heifers
    kgmeatdyn[i, 14, n] = 0
    
    # beef stockers
    kgmeatdyn[i, 15, n] = 0
    
    # dairy stockers
    kgmeatdyn[i, 16, n] = 0
    
    # sheep
    kgmeatdyn[i, 17, n] = (noanimwsdyn[i, 17, n] / totnoanimws[n, 17]) * meatprod[4, n]
    kgmeat[i, 4, n] = (noanimwsdyn[i, 17, n] / totnoanimws[n, 17]) * meatprod[4, n]
    
    #horses
    kgmeatdyn[i, 18, n] = 0
    kgmeat[i, 5, n] = 0 # assume no horses raised for meat
    
    #goats
    kgmeatdyn[i, 19, n] = (noanimwsdyn[i, 19, n] / totnoanimws[n, 19]) * meatprod[9, n]
    kgmeat[i, 9, n] = (noanimwsdyn[i, 19, n] / totnoanimws[n, 19]) * meatprod[9, n]
  }
}
