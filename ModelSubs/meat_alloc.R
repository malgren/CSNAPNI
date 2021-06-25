#meat_alloc.R
##uses the average annual populations of 19 animal types to estimate
##production quantities of beef, pork, milk, eggs, chicken, and turkey

if(print_tags == 1){
  print("ModelSubs/meat_alloc.R")
}

#allocate matrix space
kgmeatdyn = array(0, c(n_ws_NEEA, n_anims, nyrs))
kgmeatcntydyn = array(0, c(n_cnty, n_anims, nyrs))
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
    # beef
    kgmeatdyn[, 1, n] = (noanimwsdyn[, 1, n] / totnoanimws[n, 1]) * meatprod[1, n]
    kgmeatcntydyn[,1,n] = (noanimdyncty[,1,n] / totnoanimws[n, 1]) * meatprod[1, n]
    kgmeat[, 1, n] = (noanimwsdyn[, 1, n] / totnoanimws[n, 1]) * meatprod[1, n]
    
    # dairy
    kgmeatdyn[, 2, n] = (noanimwsdyn[, 2, n] / totnoanimws[n, 2]) * meatprod[2, n]
    kgmeatcntydyn[,2,n] = (noanimdyncty[,2,n] / totnoanimws[n, 2]) * meatprod[2, n]
    kgmeat[, 2, n] = (noanimwsdyn[, 2, n] / totnoanimws[n, 2]) * meatprod[2, n]
    
    
    # hogs for breeding
    kgmeatdyn[, 3, n] = 0
    kgmeatcntydyn[,3,n] = 0
    
    # hogs for slaughter
    kgmeatdyn[, 4, n] = (noanimwsdyn[, 4, n] / totnoanimws[n, 4]) * meatprod[3, n]
    kgmeatcntydyn[,4,n] = (noanimdyncty[,4,n] / totnoanimws[n, 4]) * meatprod[3, n]
    kgmeat[, 3, n] = (noanimwsdyn[, 4, n] / totnoanimws[n, 4]) * meatprod[3, n]
    
    # chicken layers
    kgmeatdyn[, 5, n] = (noanimwsdyn[, 5, n] / totnoanimws[n, 5]) * meatprod[6, n]
    kgmeatcntydyn[,5,n] = (noanimdyncty[,5,n] / totnoanimws[n, 5]) * meatprod[6, n]
    kgmeat[, 6, n] = (noanimwsdyn[, 5, n] / totnoanimws[n, 5]) * meatprod[6, n]
    
    # breeding turkeys
    kgmeatdyn[, 6, n] = 0
    kgmeatcntydyn[,6,n] = 0
    
    # chicken pullets
    kgmeatdyn[, 7, n] = 0
    kgmeatcntydyn[,7,n] = 0
    
    # chicken broilers
    kgmeatdyn[, 8, n] = (noanimwsdyn[, 8, n] / totnoanimws[n, 8]) * meatprod[7, n]
    kgmeatcntydyn[,8,n] = (noanimdyncty[,8,n] / totnoanimws[n, 8]) * meatprod[7, n]
    kgmeat[, 7, n] = (noanimwsdyn[, 8, n] / totnoanimws[n, 8]) * meatprod[7, n]
    
    # slaughter turkeys
    kgmeatdyn[, 9, n] = (noanimwsdyn[, 9, n] / totnoanimws[n, 9]) * meatprod[8, n]
    kgmeatcntydyn[,9,n] = (noanimdyncty[,9,n] / totnoanimws[n, 9]) * meatprod[8, n]
    kgmeat[, 8, n] = (noanimwsdyn[, 9, n] / totnoanimws[n, 9]) * meatprod[8, n]
    
    # beef breeding herd
    kgmeatdyn[, 10, n] = 0
    kgmeatcntydyn[,10,n] = 0
    
    # beef calves
    kgmeatdyn[, 11, n] = 0 #put veal in?
    kgmeatcntydyn[,11,n] = 0
    
    # dairy calves
    kgmeatdyn[, 12, n] = 0
    kgmeatcntydyn[,12,n] = 0
    
    # beef heifers
    kgmeatdyn[, 13, n] = 0
    kgmeatcntydyn[,13,n] = 0
    
    # dairy heifers
    kgmeatdyn[, 14, n] = 0
    kgmeatcntydyn[,14,n] = 0
    
    # beef stockers
    kgmeatdyn[, 15, n] = 0
    kgmeatcntydyn[,15,n] = 0
    
    # dairy stockers
    kgmeatdyn[, 16, n] = 0
    kgmeatcntydyn[,16,n] = 0
    
    # sheep
    kgmeatdyn[, 17, n] = (noanimwsdyn[, 17, n] / totnoanimws[n, 17]) * meatprod[4, n]
    kgmeatcntydyn[,17,n] = (noanimdyncty[,17,n] / totnoanimws[n, 17]) * meatprod[4, n]
    kgmeat[, 4, n] = (noanimwsdyn[, 17, n] / totnoanimws[n, 17]) * meatprod[4, n]
    
    #horses
    kgmeatdyn[, 18, n] = 0
    kgmeatcntydyn[,18,n] = 0
    kgmeat[, 5, n] = 0 # assume no horses raised for meat
    
    #goats
    kgmeatdyn[, 19, n] = (noanimwsdyn[, 19, n] / totnoanimws[n, 19]) * meatprod[9, n]
    kgmeatcntydyn[,19,n] = (noanimdyncty[,19,n] / totnoanimws[n, 19]) * meatprod[9, n]
    kgmeat[, 9, n] = (noanimwsdyn[, 19, n] / totnoanimws[n, 19]) * meatprod[9, n]
}
