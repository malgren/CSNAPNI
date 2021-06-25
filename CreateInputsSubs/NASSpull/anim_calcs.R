#anim_calcs
#dynamic calculation methods derived from Kellogg et al 2000, without conversion to 1000 lb "animal units"

if(print_tags == 1){
  print("CreateInputsSubs/NASSpull/anim_calcs.R")
}

#(1) fattened cattle (uses data from `CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`)
#(2) milk cows (uses data from `CATTLE, COWS, MILK - INVENTORY`)
#(3) hogs for breeding, (uses data from 'HOGS, BREEDING - INVENTORY')
#(4) hogs for slaughter, (uses data from 'HOGS - INVENTORY', 'HOGS, BREEDING - INVENTORY','HOGS - SALES, MEASURED IN HEAD')
#(5) chicken layers, (uses data from 'CHICKENS, LAYERS - INVENTORY')
#(6) breeding turkeys, (uses data from 'TURKEYS - INVENTORY','TURKEYS - SALES, MEASURED IN HEAD')
#(7) chicken pullets, (uses data from 'CHICKENS, PULLETS, REPLACEMENT - INVENTORY',"CHICKENS, PULLETS, REPLACEMENT - SALES, MEASURED IN HEAD")
#(8) chicken broilers, (uses data from 'CHICKENS, BROILERS - INVENTORY',"CHICKENS, BROILERS - SALES, MEASURED IN HEAD")
#(9) slaughter turkeys, (uses data from 'TURKEYS - INVENTORY','TURKEYS - SALES, MEASURED IN HEAD')
#(10) beef breeding herd, (uses data from `CATTLE, COWS, BEEF - INVENTORY`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(11) beef calves, (uses data from `CATTLE, COWS, BEEF - INVENTORY`,'CATTLE, CALVES - SALES, MEASURED IN HEAD')
#(12) dairy calves,  (uses data from `CATTLE, COWS, MILK - INVENTORY`,'CATTLE, CALVES - SALES, MEASURED IN HEAD')
#(13) beef heifers, (uses data from `CATTLE, COWS, BEEF - INVENTORY`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(14) dairy heifers, (uses data from `CATTLE, COWS, MILK - INVENTORY`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(15) beef stockers, (uses data from `CATTLE, COWS, BEEF - INVENTORY`,`CATTLE, GE 500 LBS - SALES, MEASURED IN HEAD`,`CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(16) dairy stockers, (uses data from `CATTLE, COWS, MILK - INVENTORY`,`CATTLE, GE 500 LBS - SALES, MEASURED IN HEAD`,`CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(17) sheep, (uses data from 'SHEEP, INCL LAMBS - INVENTORY')
#(18) horses, (uses data from 'EQUINE, HORSES & PONIES - INVENTORY')
#(19) goats (uses data from 'GOATS, ANGORA - INVENTORY', 'GOATS, MILK - INVENTORY')

###beef and dairy
#(1) fattened cattle (uses data from `CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`)
fattened_cattle_sold_list = grep("CATTLE_ON_FEED_SALES", county_commodities_list, value = TRUE)
fattened_cattle_sold = array(0,c(length(NASS_County[,1]),length(fattened_cattle_sold_list)))
for(i in 1:length(fattened_cattle_sold_list)){
  fattened_cattle_sold[,i] = as.numeric(NASS_County[, fattened_cattle_sold_list[i]])
}
#dynamic calculation
fattened_cattle_cycles = 2.5  #146 days on the farm, 2 full cycles per year
fattened_cattle = fattened_cattle_sold/fattened_cattle_cycles

#(2) milk cows (uses data from `CATTLE, COWS, MILK - INVENTORY`)
milk_cows_list = grep("CATTLE_COWS_MILK_INV", county_commodities_list, value = TRUE)
milk_cows = array(0,c(length(NASS_County[,1]),length(milk_cows_list)))
for(i in 1:length(milk_cows_list)){
  milk_cows[,i] = as.numeric(NASS_County[, milk_cows_list[i]])
}

beef_cows_list = grep("CATTLE_COWS_BEEF_INV", county_commodities_list, value = TRUE)
other_cattle_list = grep("EXCL_COWS", county_commodities_list, value = TRUE)
cattle_sold_list = grep("CATTLE_GE_500_LBS_SALES", county_commodities_list, value = TRUE)
calves_sold_list = grep("CATTLE_CALVES_SALES", county_commodities_list, value = TRUE)
beef_cows = array(0,c(length(NASS_County[,1]),length(beef_cows_list)))
other_cattle = array(0,c(length(NASS_County[,1]),length(other_cattle_list)))
cattle_sold = array(0,c(length(NASS_County[,1]),length(cattle_sold_list)))
calves_sold = array(0,c(length(NASS_County[,1]),length(calves_sold_list)))
for(i in 1:length(beef_cows_list)){
  beef_cows[,i] = as.numeric(NASS_County[, beef_cows_list[i]])
  other_cattle[,i] = as.numeric(NASS_County[, other_cattle_list[i]])
  cattle_sold[,i] = as.numeric(NASS_County[, cattle_sold_list[i]])
  calves_sold[,i] = as.numeric(NASS_County[, calves_sold_list[i]])
}
heifers = 0.48 * other_cattle
steers_and_bulls = other_cattle - heifers
expected_bulls = 0.05 * beef_cows
dim = dim(expected_bulls)
bulls = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    bulls[i,j] = min(expected_bulls[i,j], steers_and_bulls[i,j])
  }
}

#(10) beef breeding herd
beef_breeding_herd = beef_cows + bulls

#(11) beef calves, (12) dairy calves, #(13) beef heifers, #(14) dairy heifers
#dynamic calculation
calves_cycles = 2.43     #150 days on farm
heifer_cycles = 2.43    #150 days on farm
expected_beef_calves = 0.82 * beef_cows
expected_dairy_calves = 0.65 * milk_cows
expected_beef_heifers = 0.15 * beef_cows
expected_dairy_heifers = 0.2 * milk_cows
dim = dim(expected_beef_calves)
beef_calves_purchased_and_sold = array(0,c(dim[1],dim[2]))
dairy_calves_purchased_and_sold = array(0,c(dim[1],dim[2]))
beef_calves = array(0,c(dim[1],dim[2]))
dairy_calves = array(0,c(dim[1],dim[2]))
beef_replacement_herd_heifers = array(0,c(dim[1],dim[2]))
dairy_replacement_herd_heifers = array(0,c(dim[1],dim[2]))
beef_heifers_avg = array(0,c(dim[1],dim[2]))
dairy_heifers_avg = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    #if there are beef cows but no milk cows
    if(beef_cows[i,j]>0 & milk_cows[i,j]==0){
      beef_calves_purchased_and_sold[i,j] = calves_sold[i,j] - expected_beef_calves[i,j]
      beef_calves[i,j] = expected_beef_calves[i,j]/calves_cycles
      + beef_calves_purchased_and_sold[i,j]/calves_cycles*(calves_cycles-1)/calves_cycles
      beef_replacement_herd_heifers[i,j] = min(expected_beef_heifers[i,j], heifers[i,j])
      beef_heifers_avg[i,j] = beef_replacement_herd_heifers[i,j]/heifer_cycles
      #else if there are milk cows but no beef cows
    }else if(beef_cows[i,j]==0 & milk_cows[i,j]>0){
      dairy_calves_purchased_and_sold[i,j] = calves_sold[i,j] - expected_dairy_calves[i,j]
      dairy_calves[i,j] = expected_dairy_calves[i,j]/calves_cycles 
      + dairy_calves_purchased_and_sold[i,j]/calves_cycles*(calves_cycles-1)/calves_cycles
      dairy_replacement_herd_heifers[i,j] = min(expected_dairy_heifers[i,j], heifers[i,j])
      dairy_heifers_avg[i,j] = dairy_replacement_herd_heifers[i,j]/heifer_cycles
      #else if there are both beef cows and milk cows
    }else if(beef_cows[i,j]>0 & milk_cows[i,j]>0){
      dairy_calves[i,j] = expected_dairy_calves[i,j]/calves_cycles 
      beef_calves_purchased_and_sold[i,j] = calves_sold[i,j] - expected_beef_calves[i,j] - expected_dairy_calves[i,j]
      beef_calves[i,j] = expected_beef_calves[i,j]/calves_cycles
      + beef_calves_purchased_and_sold[i,j]/calves_cycles*(calves_cycles-1)/calves_cycles
      dairy_replacement_herd_heifers[i,j] = min(expected_dairy_heifers[i,j], heifers[i,j])
      beef_replacement_herd_heifers[i,j] = min(expected_beef_heifers[i,j], heifers[i,j] - dairy_replacement_herd_heifers[i,j])
      beef_heifers_avg[i,j] = beef_replacement_herd_heifers[i,j]/heifer_cycles
      dairy_heifers_avg[i,j] = dairy_replacement_herd_heifers[i,j]/heifer_cycles
    }
  }
}

#(15) beef stockers, (16) dairy stockers
#dynamic calculation
stocker_cycles = 1.825 #200 days on farm
beef_stockers_sold = array(0,c(dim[1],dim[2]))
beef_stockers_inventory = array(0,c(dim[1],dim[2]))
dairy_stockers_sold = array(0,c(dim[1],dim[2]))
dairy_stockers_inventory = array(0,c(dim[1],dim[2]))
beef_stockers_purchased_and_sold = array(0,c(dim[1],dim[2]))
dairy_stockers_purchased_and_sold = array(0,c(dim[1],dim[2]))
beef_stockers_avg = array(0,c(dim[1],dim[2]))
dairy_stockers_avg = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    #if there are beef cows but no milk cows
    if(beef_cows[i,j]>0 & milk_cows[i,j]==0){
      beef_stockers_sold[i,j] = cattle_sold[i,j] - fattened_cattle_sold[i,j] - beef_replacement_herd_heifers[i,j]
      beef_stockers_inventory[i,j] = heifers[i,j] - beef_replacement_herd_heifers[i,j] + steers_and_bulls[i,j] - bulls[i,j]
      #else if there are milk cows but no beef cows
    }else if(beef_cows[i,j]==0 & milk_cows[i,j]>0){
      dairy_stockers_sold[i,j] = cattle_sold[i,j] - fattened_cattle_sold[i,j] - dairy_replacement_herd_heifers[i,j]
      dairy_stockers_inventory[i,j] = heifers[i,j] - dairy_replacement_herd_heifers[i,j] + steers_and_bulls[i,j]
      dairy_stockers_purchased_and_sold[i,j] = dairy_stockers_sold[i,j] + dairy_stockers_inventory[i,j] - expected_dairy_calves[i,j]
      if(dairy_stockers_purchased_and_sold[i,j]<0){
        dairy_stockers_avg[i,j] = (dairy_stockers_sold[i,j] + dairy_stockers_inventory[i,j])/stocker_cycles
      }else{
        dairy_stockers_avg[i,j] = expected_dairy_calves[i,j]/stocker_cycles + dairy_stockers_purchased_and_sold[i,j]/stocker_cycles*(stocker_cycles-1)/stocker_cycles
      }
      #else if there are both beef cows and milk cows or neither
    }else{
      beef_stockers_sold[i,j] = cattle_sold[i,j] - fattened_cattle_sold[i,j] - beef_replacement_herd_heifers[i,j] - dairy_replacement_herd_heifers[i,j]
      beef_stockers_inventory[i,j] = heifers[i,j] - beef_replacement_herd_heifers[i,j] - dairy_replacement_herd_heifers[i,j] + steers_and_bulls[i,j] - bulls[i,j]
    }
    #if there are beef cows
    if(beef_cows[i,j]>0){
      beef_stockers_purchased_and_sold[i,j] = beef_stockers_sold[i,j] + beef_stockers_inventory[i,j] - expected_beef_calves[i,j]
      if(beef_stockers_purchased_and_sold[i,j]<0){
        beef_stockers_avg[i,j] = (beef_stockers_sold[i,j] + beef_stockers_inventory[i,j])/stocker_cycles
      }else{
        beef_stockers_avg[i,j] = expected_beef_calves[i,j]/stocker_cycles + beef_stockers_purchased_and_sold[i,j]/stocker_cycles*(stocker_cycles-1)/stocker_cycles
      }
    }else{
      beef_stockers_avg[i,j] = beef_stockers_sold[i,j] + beef_stockers_inventory[i,j]*(stocker_cycles-1)/stocker_cycles
    }
  }
}

#pigs
#(3) hogs for breeding, (4) hogs for slaughter
hogs_and_pigs_list = grep("HOGS_INV", county_commodities_list, value = TRUE)
hogs_for_breeding_list = grep("HOGS_BREEDING_INV", county_commodities_list, value = TRUE)
hogs_sales_list = grep("HOGS_SALES", county_commodities_list, value = TRUE)
hogs_and_pigs = array(0,c(length(NASS_County[,1]),length(hogs_and_pigs_list)))
hogs_for_breeding = array(0,c(length(NASS_County[,1]),length(hogs_for_breeding_list)))
hogs_sales = array(0,c(length(NASS_County[,1]),length(hogs_sales_list)))
for(i in 1:length(hogs_and_pigs_list)){
  hogs_and_pigs[,i] = as.numeric(NASS_County[, hogs_and_pigs_list[i]])
  hogs_for_breeding[,i] = as.numeric(NASS_County[, hogs_for_breeding_list[i]])
  hogs_sales[,i] = as.numeric(NASS_County[, hogs_sales_list[i]])
}
#dynamic calculation
hogs_for_slaughter_cycles = 2   #182 days on farm
hogs_for_slaughter_inv = hogs_and_pigs - hogs_for_breeding
hogs_for_slaughter_sales = hogs_sales*0.75
dim = dim(hogs_for_slaughter_inv)
hogs_for_slaughter = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    if(hogs_for_slaughter_inv[i,j]<=0){
      hogs_for_slaughter[i,j] = hogs_for_slaughter_sales[i,j]/hogs_for_slaughter_cycles
    }else if(hogs_for_slaughter_sales[i,j]==0){
      hogs_for_slaughter[i,j] = hogs_for_slaughter_inv[i,j]/hogs_for_slaughter_cycles*(hogs_for_slaughter_cycles-1)/(hogs_for_slaughter_cycles)
    }else{
      hogs_for_slaughter[i,j] = hogs_for_slaughter_inv[i,j]/hogs_for_slaughter_cycles + 
        hogs_for_slaughter_sales[i,j]/hogs_for_slaughter_cycles*(hogs_for_slaughter_cycles-1)/(hogs_for_slaughter_cycles)
    }
  }
}

#chicken
#(5) chicken layers, (7) chicken pullets, (8) chicken broilers
layers_list = grep("CHICKENS_LAYERS_INV", county_commodities_list, value = TRUE)
pullets_inv_list = grep("CHICKENS_PULLETS_REPLACEMENT_INV", county_commodities_list, value = TRUE)
pullets_sales_list = grep("CHICKENS_PULLETS_REPLACEMENT_SALES", county_commodities_list, value = TRUE)
broilers_inv_list = grep("CHICKENS_BROILERS_INV", county_commodities_list, value = TRUE)
broilers_sales_list = grep("CHICKENS_BROILERS_SALES", county_commodities_list, value = TRUE)
layers = array(0,c(length(NASS_County[,1]),length(layers_list)))
pullets_inv = array(0,c(length(NASS_County[,1]),length(pullets_inv_list)))
pullets_sales = array(0,c(length(NASS_County[,1]),length(pullets_sales_list)))
broilers_inv = array(0,c(length(NASS_County[,1]),length(broilers_inv_list)))
broilers_sales = array(0,c(length(NASS_County[,1]),length(broilers_sales_list)))
for(i in 1:length(layers_list)){
  layers[,i] = as.numeric(NASS_County[, layers_list[i]])
  pullets_inv[,i] = as.numeric(NASS_County[, pullets_inv_list[i]])
  pullets_sales[,i] = as.numeric(NASS_County[, pullets_sales_list[i]])
  broilers_inv[,i] = as.numeric(NASS_County[, broilers_inv_list[i]])
  broilers_sales[,i] = as.numeric(NASS_County[, broilers_sales_list[i]])
}
#dynamic calculation
pullets_cycles = 2.25 #162 days on farm
dim = dim(pullets_inv)
pullets = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    if(pullets_inv[i,j]==0){
      pullets[i,j] = pullets_sales[i,j]/pullets_cycles
    }else if(pullets_sales[i,j]==0){
      pullets[i,j] = pullets_inv[i,j]/pullets_cycles*(pullets_cycles-1)/(pullets_cycles)
    }else{
      pullets[i,j] = pullets_inv[i,j]/pullets_cycles +
        pullets_sales[i,j]/pullets_cycles*(pullets_cycles-1)/(pullets_cycles)
    }
  }
}
broilers_cycles = 6 #60 days on farm
dim = dim(broilers_inv)
broilers = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    if(broilers_inv[i,j]==0){
      broilers[i,j] = broilers_sales[i,j]/broilers_cycles
    }else if(broilers_sales[i,j]==0){
      broilers[i,j] = broilers_inv[i,j]/broilers_cycles*(broilers_cycles-1)/(broilers_cycles)
    }else{
      broilers[i,j] = broilers_inv[i,j]/broilers_cycles + 
        broilers_sales[i,j]/broilers_cycles*(broilers_cycles-1)/(broilers_cycles)
    }
  }
}


#turkeys
#(6) breeding turkeys, (9) slaughter turkeys
turkeys_list = grep("TURKEYS_INVENTORY", county_commodities_list, value = TRUE)
turkeys_sold_list = grep("TURKEYS_SALES", county_commodities_list, value = TRUE)
turkeys = array(0,c(length(NASS_County[,1]),length(turkeys_list)))
turkeys_sold = array(0,c(length(NASS_County[,1]),length(turkeys_sold_list)))
for(i in 1:length(turkeys_list)){
  turkeys[,i] = as.numeric(NASS_County[, turkeys_list[i]])
  turkeys_sold[,i] = as.numeric(NASS_County[, turkeys_sold_list[i]])
}
#dynamic calculation
breeding_turkeys_inv = turkeys*0.05
breeding_turkeys_sold = turkeys_sold*0.03
breeding_turkeys = breeding_turkeys_inv
dim = dim(breeding_turkeys_inv)
breeding_turkeys = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    if(breeding_turkeys_inv[i,j]==0){
      breeding_turkeys[i,j] = breeding_turkeys_sold[i,j]/2
    }else{
      breeding_turkeys[i,j] = breeding_turkeys_inv[i,j]
    }
  }
}
slaughter_turkeys_inv = turkeys*0.95
slaughter_turkeys_sold = turkeys_sold*0.97
slaughter_turkeys_cycles = 2    #182 days on farm
dim = dim(slaughter_turkeys_inv)
slaughter_turkeys = array(0,c(dim[1],dim[2]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    if(slaughter_turkeys_inv[i,j]==0){
      slaughter_turkeys[i,j] = slaughter_turkeys_sold[i,j]/slaughter_turkeys_cycles
    }else if(slaughter_turkeys_sold[i,j]==0){
      slaughter_turkeys[i,j] = slaughter_turkeys_inv[i,j]/slaughter_turkeys_cycles*(slaughter_turkeys_cycles-1)/(slaughter_turkeys_cycles)
    }else{
      slaughter_turkeys[i,j] = slaughter_turkeys_inv[i,j]/slaughter_turkeys_cycles + 
        (slaughter_turkeys_sold[i,j]/slaughter_turkeys_cycles)*(slaughter_turkeys_cycles-1)/(slaughter_turkeys_cycles)
    }
  }
}

#(17) sheep
sheep_list = grep("SHEEP", county_commodities_list, value = TRUE)
sheep = array(0,c(length(NASS_County[,1]),length(sheep_list)))
for(i in 1:length(sheep_list)){
  sheep[,i] = as.numeric(NASS_County[, sheep_list[i]])
}

#(18) horses
horses_list = grep("EQUINE", county_commodities_list, value = TRUE)
horses = array(0,c(length(NASS_County[,1]),length(horses_list)))
for(i in 1:length(horses_list)){
  horses[,i] = as.numeric(NASS_County[, horses_list[i]])
}

#(19) goats
goats_angora_list = grep("GOATS_ANGORA", county_commodities_list, value = TRUE)
goats_milk_list = grep("GOATS_MILK", county_commodities_list, value = TRUE)
goats_angora = array(0,c(length(NASS_County[,1]),length(goats_angora_list)))
goats_milk = array(0,c(length(NASS_County[,1]),length(goats_milk_list)))
for(i in 1:length(goats_angora_list)){
  goats_angora[,i] = as.numeric(NASS_County[, goats_angora_list[i]])
  goats_milk[,i] = as.numeric(NASS_County[, goats_milk_list[i]])
}
goats = goats_angora + goats_milk

#sums
anim_sums=c(colSums(fattened_cattle),colSums(milk_cows),colSums(hogs_for_breeding),colSums(hogs_for_slaughter),colSums(layers),
            colSums(breeding_turkeys), colSums(pullets),colSums(broilers),colSums(slaughter_turkeys),colSums(beef_breeding_herd),
            colSums(beef_calves),colSums(dairy_calves),colSums(beef_heifers_avg),colSums(dairy_heifers_avg),
            colSums(beef_stockers_avg),colSums(dairy_stockers_avg),colSums(sheep),colSums(horses),colSums(goats))
anim_sums_array=t(array(anim_sums,c(length(year),19)))

NASS_anim_sums = data.frame(anim_sums_array)

#full county level animal average inventories for all years
anim_avg_inv_array = array(0,c(length(NASS_County[,1]),19,length(year)))
for(n in 1:(length(year))){
  anim_avg_inv = c(fattened_cattle[,n],milk_cows[,n],hogs_for_breeding[,n],hogs_for_slaughter[,n],layers[,n],
                   breeding_turkeys[,n],pullets[,n],broilers[,n],slaughter_turkeys[,n],beef_breeding_herd[,n],
                   beef_calves[,n],dairy_calves[,n],beef_heifers_avg[,n],dairy_heifers_avg[,n],
                   beef_stockers_avg[,n],dairy_stockers_avg[,n],sheep[,n],horses[,n],goats[,n])
  anim_avg_inv_array[,,n]=array(anim_avg_inv,c(length(NASS_County[,1]),19))

  #write data files
  write_name = paste("InputFiles/NASSanim",year[n],".txt",sep = "")
  write.table(anim_avg_inv_array[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}

#write keys
write_name = paste("InputFileKeys/NASSanimkey.txt",sep = "")
write.table(c("annual average population, estimated using dynamic animal population averaging methods from Kellog et al 2000", 
                  "cols: [1]fattened_cattle, [2]milk_cows, [3]hogs_for_breeding, [4]hogs_for_slaughter, [5]layers,
                  [6]breeding_turkeys, [7]pullets, [8]broilers, [9]slaughter_turkeys, [10]beef_breeding_herd,
                  [11]beef_calves, [12]dairy_calves, [13]beef_heifers, [14]dairy_heifers,
                  [15]beef_stockers, [16]dairy_stockers, [17]sheep, [18]horses, [19]goats", 
                  "rows: US counties (IDs listed below)", cnty_IDs_NANI_NAPI[,1]), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

