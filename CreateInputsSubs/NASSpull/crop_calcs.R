#crop_calcs.r
# use USDA Census data to calculate
# harvest (in km^2) and production (in kg) for
# (1) corn grain, (need 'CORN, GRAIN - ACRES HARVESTED', 'CORN, GRAIN - PRODUCTION, MEASURED IN BU')
# (2) corn silage, (need 'CORN, SILAGE - ACRES HARVESTED', 'CORN, SILAGE - PRODUCTION, MEASURED IN TONS')
# (3) wheat, (need 'WHEAT - ACRES HARVESTED', 'WHEAT - PRODUCTION, MEASURED IN BU')
# (4) oats, (need 'OATS - ACRES HARVESTED', 'OATS - PRODUCTION, MEASURED IN BU')
# (5) barley, (need 'BARLEY - ACRES HARVESTED', 'BARLEY - PRODUCTION, MEASURED IN BU')
# (6) sorghum grain, (need 'SORGHUM, GRAIN - ACRES HARVESTED', 'SORGHUM, GRAIN - PRODUCTION, MEASURED IN BU')
# (7) sorghum silage, (need 'SORGHUM, SILAGE - ACRES HARVESTED', 'SORGHUM, SILAGE - PRODUCTION, MEASURED IN TONS')
# (8) potatoes, (need 'POTATOES - ACRES HARVESTED', 'POTATOES - PRODUCTION, MEASURED IN CWT')
# (9) rye, (need 'RYE - ACRES HARVESTED', 'RYE - PRODUCTION, MEASURED IN BU')
# (10) alfalfa hay, (need 'HAY, ALFALFA - ACRES HARVESTED', 'HAY, ALFALFA - PRODUCTION, MEASURED IN TONS')
# (11) other hay, ('HAY - ACRES HARVESTED','HAY - PRODUCTION, MEASURED IN TONS')
# (12) soybeans, ('SOYBEANS - ACRES HARVESTED', 'SOYBEANS - PRODUCTION, MEASURED IN BU')
# (13) cropland, ('ag land, pastureland - acres' - ('ag land, pastureland, (excl cropland & woodland) - acres' + 'ag land, woodland - acres'))
# (14) noncropland, ('ag land, pastureland, (excl cropland & woodland) - acres' + 'ag land, woodland - acres')
# (15) rice, ('RICE - ACRES HARVESTED', 'RICE - PRODUCTION, MEASURED IN CWT')
# (16) peanuts ('PEANUTS - ACRES HARVESTED','PEANUTS - PRODUCTION, MEASURED IN LB')

#conversion factors
bushelperton_corn = 39.368 #bushels / (metric)ton
bushelperton_sorghum = 39.368 #bushels / (metric)ton
bushelperton_barley = 45.9296 #bushels / (metric)ton
bushelperton_wheat = 36.7437 #bushels / (metric)ton
bushelperton_soybeans = 36.7437 #bushels / (metric)ton
bushelperton_oats = 64.8420 #bushels / (metric)ton
bushelperton_rye = 39.3680 #bushels / (metric)ton
lbsperkg = 2.20462 #lbs/kg
km2peracre = 0.00405 #km2/acre
literspergal = 3.78541 #liters/gallon

# (1) corn grain, (need 'CORN, GRAIN - ACRES HARVESTED', 'CORN, GRAIN - PRODUCTION, MEASURED IN BU')
corn_grain_area_list = grep("CORN_GRAIN_ACRES", county_commodities_list, value = TRUE)
corn_grain_areas = array(0,c(length(NASS_County[,1]),length(corn_grain_area_list)))
for(i in 1:length(corn_grain_area_list)){
  corn_grain_areas[,i] = as.numeric(NASS_County[, corn_grain_area_list[i]])*km2peracre
}
corn_grain_prod_list = grep("CORN_GRAIN_P", county_commodities_list, value = TRUE)
corn_grain_prod = array(0,c(length(NASS_County[,1]),length(corn_grain_prod_list)))
for(i in 1:length(corn_grain_prod_list)){
  corn_grain_prod[,i] = as.numeric(NASS_County[, corn_grain_prod_list[i]])/bushelperton_corn*1000
}

# (2) corn silage, (need 'CORN, SILAGE - ACRES HARVESTED', 'CORN, SILAGE - PRODUCTION, MEASURED IN TONS')
corn_silage_area_list = grep("CORN_SILAGE_ACRES", county_commodities_list, value = TRUE)
corn_silage_areas = array(0,c(length(NASS_County[,1]),length(corn_silage_area_list)))
for(i in 1:length(corn_silage_area_list)){
  corn_silage_areas[,i] = as.numeric(NASS_County[, corn_silage_area_list[i]])*km2peracre
}
corn_silage_prod_list = grep("CORN_SILAGE_P", county_commodities_list, value = TRUE)
corn_silage_prod = array(0,c(length(NASS_County[,1]),length(corn_silage_prod_list)))
for(i in 1:length(corn_silage_prod_list)){
  corn_silage_prod[,i] = as.numeric(NASS_County[, corn_silage_prod_list[i]])*1000
}

# (3) wheat, (need 'WHEAT - ACRES HARVESTED', 'WHEAT - PRODUCTION, MEASURED IN BU')
wheat_area_list = grep("WHEAT_ACRES", county_commodities_list, value = TRUE)
wheat_areas = array(0,c(length(NASS_County[,1]),length(wheat_area_list)))
for(i in 1:length(corn_grain_area_list)){
  wheat_areas[,i] = as.numeric(NASS_County[, wheat_area_list[i]])*km2peracre
}
wheat_prod_list = grep("WHEAT_P", county_commodities_list, value = TRUE)
wheat_prod = array(0,c(length(NASS_County[,1]),length(wheat_prod_list)))
for(i in 1:length(corn_grain_prod_list)){
  wheat_prod[,i] = as.numeric(NASS_County[, wheat_prod_list[i]])/bushelperton_wheat*1000
}

# (4) oats, (need 'OATS - ACRES HARVESTED', 'OATS - PRODUCTION, MEASURED IN BU')
oats_area_list = grep("OATS_ACRES", county_commodities_list, value = TRUE)
oats_areas = array(0,c(length(NASS_County[,1]),length(oats_area_list)))
for(i in 1:length(oats_area_list)){
  oats_areas[,i] = as.numeric(NASS_County[, oats_area_list[i]])*km2peracre
}
oats_prod_list = grep("OATS_P", county_commodities_list, value = TRUE)
oats_prod = array(0,c(length(NASS_County[,1]),length(oats_prod_list)))
for(i in 1:length(oats_prod_list)){
  oats_prod[,i] = as.numeric(NASS_County[, oats_prod_list[i]])/bushelperton_oats*1000
}

# (5) barley, (need 'BARLEY - ACRES HARVESTED', 'BARLEY - PRODUCTION, MEASURED IN BU')
barley_area_list = grep("BARLEY_ACRES", county_commodities_list, value = TRUE)
barley_areas = array(0,c(length(NASS_County[,1]),length(barley_area_list)))
for(i in 1:length(barley_area_list)){
  barley_areas[,i] = as.numeric(NASS_County[, barley_area_list[i]])*km2peracre
}
barley_prod_list = grep("BARLEY_P", county_commodities_list, value = TRUE)
barley_prod = array(0,c(length(NASS_County[,1]),length(barley_prod_list)))
for(i in 1:length(barley_prod_list)){
  barley_prod[,i] = as.numeric(NASS_County[, barley_prod_list[i]])/bushelperton_barley*1000
}

# (6) sorghum grain, (need 'SORGHUM, GRAIN - ACRES HARVESTED', 'SORGHUM, GRAIN - PRODUCTION, MEASURED IN BU')
sorghum_grain_area_list = grep("SORGHUM_GRAIN_ACRES", county_commodities_list, value = TRUE)
sorghum_grain_areas = array(0,c(length(NASS_County[,1]),length(sorghum_grain_area_list)))
for(i in 1:length(sorghum_grain_area_list)){
  sorghum_grain_areas[,i] = as.numeric(NASS_County[, sorghum_grain_area_list[i]])*km2peracre
}
sorghum_grain_prod_list = grep("SORGHUM_GRAIN_P", county_commodities_list, value = TRUE)
sorghum_grain_prod = array(0,c(length(NASS_County[,1]),length(sorghum_grain_prod_list)))
for(i in 1:length(sorghum_grain_prod_list)){
  sorghum_grain_prod[,i] = as.numeric(NASS_County[, sorghum_grain_prod_list[i]])/bushelperton_sorghum*1000
}

# (7) sorghum silage, (need 'SORGHUM, SILAGE - ACRES HARVESTED', 'SORGHUM, SILAGE - PRODUCTION, MEASURED IN TONS')
sorghum_silage_area_list = grep("SORGHUM_SILAGE_ACRES", county_commodities_list, value = TRUE)
sorghum_silage_areas = array(0,c(length(NASS_County[,1]),length(sorghum_silage_area_list)))
for(i in 1:length(sorghum_silage_area_list)){
  sorghum_silage_areas[,i] = as.numeric(NASS_County[, sorghum_silage_area_list[i]])*km2peracre
}
sorghum_silage_prod_list = grep("SORGHUM_SILAGE_P", county_commodities_list, value = TRUE)
sorghum_silage_prod = array(0,c(length(NASS_County[,1]),length(sorghum_silage_prod_list)))
for(i in 1:length(sorghum_silage_prod_list)){
  sorghum_silage_prod[,i] = as.numeric(NASS_County[, sorghum_silage_prod_list[i]])*1000
}

# (8) potatoes, (need 'POTATOES - ACRES HARVESTED', 'POTATOES - PRODUCTION, MEASURED IN CWT')
potatoes_area_list = grep("POTATOES_ACRES", county_commodities_list, value = TRUE)
potatoes_areas = array(0,c(length(NASS_County[,1]),length(potatoes_area_list)))
for(i in 1:length(potatoes_area_list)){
  potatoes_areas[,i] = as.numeric(NASS_County[, potatoes_area_list[i]])*km2peracre
}
potatoes_prod_list = grep("POTATOES_P", county_commodities_list, value = TRUE)
potatoes_prod = array(0,c(length(NASS_County[,1]),length(potatoes_prod_list)))
for(i in 1:length(potatoes_prod_list)){
  potatoes_prod[,i] = as.numeric(NASS_County[, potatoes_prod_list[i]])*100/lbsperkg
}

# (9) rye, (need 'RYE - ACRES HARVESTED', 'RYE - PRODUCTION, MEASURED IN BU')=
rye_area_list = grep("RYE_ACRES", county_commodities_list, value = TRUE)
rye_areas = array(0,c(length(NASS_County[,1]),length(rye_area_list)))
for(i in 1:length(rye_area_list)){
  rye_areas[,i] = as.numeric(NASS_County[, rye_area_list[i]])*km2peracre
}
rye_prod_list = grep("RYE_P", county_commodities_list, value = TRUE)
rye_prod = array(0,c(length(NASS_County[,1]),length(rye_prod_list)))
for(i in 1:length(rye_prod_list)){
  rye_prod[,i] = as.numeric(NASS_County[, rye_prod_list[i]])/bushelperton_rye*1000
}

# (10) alfalfa hay, (need 'HAY, ALFALFA - ACRES HARVESTED', 'HAY, ALFALFA - PRODUCTION, MEASURED IN TONS')
alfalfa_hay_area_list = grep("HAY_ALFALFA_ACRES", county_commodities_list, value = TRUE)
alfalfa_hay_areas = array(0,c(length(NASS_County[,1]),length(alfalfa_hay_area_list)))
for(i in 1:length(alfalfa_hay_area_list)){
  alfalfa_hay_areas[,i] = as.numeric(NASS_County[, alfalfa_hay_area_list[i]])*km2peracre
}
alfalfa_hay_prod_list = grep("HAY_ALFALFA_P", county_commodities_list, value = TRUE)
alfalfa_hay_prod = array(0,c(length(NASS_County[,1]),length(alfalfa_hay_prod_list)))
for(i in 1:length(alfalfa_hay_prod_list)){
  alfalfa_hay_prod[,i] = as.numeric(NASS_County[, alfalfa_hay_prod_list[i]])*1000
}

# (11) other hay, ('HAY - ACRES HARVESTED','HAY - PRODUCTION, MEASURED IN TONS')
hay_area_list = grep("HAY_ACRES", county_commodities_list, value = TRUE)
other_hay_areas = array(0,c(length(NASS_County[,1]),length(hay_area_list)))
for(i in 1:length(hay_area_list)){
  other_hay_areas[,i] = (as.numeric(NASS_County[, hay_area_list[i]])-as.numeric(NASS_County[, alfalfa_hay_area_list[i]]))*km2peracre
}
hay_prod_list = grep("HAY_P", county_commodities_list, value = TRUE)
other_hay_prod = array(0,c(length(NASS_County[,1]),length(hay_prod_list)))
for(i in 1:length(hay_prod_list)){
  other_hay_prod[,i] = (as.numeric(NASS_County[, hay_prod_list[i]])-as.numeric(NASS_County[, alfalfa_hay_prod_list[i]]))*1000
}

# (12) soybeans, ('SOYBEANS - ACRES HARVESTED', 'SOYBEANS - PRODUCTION, MEASURED IN BU')
soybeans_area_list = grep("SOYBEANS_ACRES", county_commodities_list, value = TRUE)
soybeans_areas = array(0,c(length(NASS_County[,1]),length(soybeans_area_list)))
for(i in 1:length(soybeans_area_list)){
  soybeans_areas[,i] = as.numeric(NASS_County[, soybeans_area_list[i]])*km2peracre
}
soybeans_prod_list = grep("SOYBEANS_P", county_commodities_list, value = TRUE)
soybeans_prod = array(0,c(length(NASS_County[,1]),length(soybeans_prod_list)))
for(i in 1:length(soybeans_prod_list)){
  soybeans_prod[,i] = as.numeric(NASS_County[, soybeans_prod_list[i]])/bushelperton_soybeans*1000
}

# (13) cropland pasture, (14) noncropland pasture
pasture_area_list = grep("ag_land_pastureland_acres", county_commodities_list, value = TRUE)
cropland_pasture_area_list = grep("ag_land_cropland_pastured_only", county_commodities_list, value = TRUE)
all_pastureland_areas = array(0,c(length(NASS_County[,1]),length(year)))
cropland_pasture_areas = array(0,c(length(NASS_County[,1]),length(year)))
noncropland_pasture_areas = array(0,c(length(NASS_County[,1]),length(year)))
for(i in 1:(length(pasture_area_list))){
  all_pastureland_areas[,i] = as.numeric(NASS_County[, pasture_area_list[i]])*km2peracre
  cropland_pasture_areas[,i] = as.numeric(NASS_County[, cropland_pasture_area_list[i]])*km2peracre
  noncropland_pasture_areas[,i] = all_pastureland_areas[,i] - cropland_pasture_areas[,i]
}
cp_yield = 112000 #yield factor from NANI/NAPI Toolbox data, kg per km2
ncp_yield = 56000 #yield factor from NANI/NAPI Toolbox data
cropland_pasture_prod = cp_yield*cropland_pasture_areas
noncropland_pasture_prod = ncp_yield*noncropland_pasture_areas

# (15) rice, ('RICE - ACRES HARVESTED', 'RICE - PRODUCTION, MEASURED IN CWT')
rice_area_list = grep("RICE_ACRES", county_commodities_list, value = TRUE)
rice_areas = array(0,c(length(NASS_County[,1]),length(rice_area_list)))
for(i in 1:length(rice_area_list)){
  rice_areas[,i] = as.numeric(NASS_County[, rice_area_list[i]])*km2peracre
}
rice_prod_list = grep("RICE_P", county_commodities_list, value = TRUE)
rice_prod = array(0,c(length(NASS_County[,1]),length(rice_prod_list)))
for(i in 1:length(rice_prod_list)){
  rice_prod[,i] = as.numeric(NASS_County[, rice_prod_list[i]])*100/lbsperkg
}

# (16) peanuts ('PEANUTS - ACRES HARVESTED','PEANUTS - PRODUCTION, MEASURED IN LB')
peanuts_area_list = grep("PEANUTS_ACRES", county_commodities_list, value = TRUE)
peanuts_areas = array(0,c(length(NASS_County[,1]),length(peanuts_area_list)))
for(i in 1:length(peanuts_area_list)){
  peanuts_areas[,i] = as.numeric(NASS_County[, peanuts_area_list[i]])*km2peracre
}
peanuts_prod_list = grep("PEANUTS_P", county_commodities_list, value = TRUE)
peanuts_prod = array(0,c(length(NASS_County[,1]),length(peanuts_prod_list)))
for(i in 1:length(peanuts_prod_list)){
  peanuts_prod[,i] = as.numeric(NASS_County[, peanuts_prod_list[i]])/lbsperkg
}


area_sums=c(colSums(corn_grain_areas),colSums(corn_silage_areas),colSums(wheat_areas),colSums(oats_areas),colSums(barley_areas),
       colSums(sorghum_grain_areas),colSums(sorghum_silage_areas),colSums(potatoes_areas),colSums(rye_areas),
       colSums(alfalfa_hay_areas),colSums(other_hay_areas),colSums(soybeans_areas),colSums(cropland_pasture_areas),
       colSums(noncropland_pasture_areas),colSums(rice_areas),colSums(peanuts_areas))
area_sums_array=t(array(area_sums,c(length(year),16)))

prod_sums=c(colSums(corn_grain_prod),colSums(corn_silage_prod),colSums(wheat_prod),colSums(oats_prod),colSums(barley_prod),
            colSums(sorghum_grain_prod),colSums(sorghum_silage_prod),colSums(potatoes_prod),colSums(rye_prod),
            colSums(alfalfa_hay_prod),colSums(other_hay_prod),colSums(soybeans_prod),colSums(cropland_pasture_prod),
            colSums(noncropland_pasture_prod),colSums(rice_prod),colSums(peanuts_prod))
prod_sums_array=t(array(prod_sums,c(length(year),16)))

NASS_area_sums = data.frame(area_sums_array)
NASS_prod_sums = data.frame(prod_sums_array)

#full county-level crop data for all years
areas_array = array(0,c(length(NASS_County[,1]),16,length(year))) #crop harvested areas, in km2
prod_array = array(0,c(length(NASS_County[,1]),16,length(year))) #crop production, in kg
for(n in 1:(length(year))){
  areas = c(corn_grain_areas[,n],corn_silage_areas[,n],wheat_areas[,n],oats_areas[,n],barley_areas[,n],
           sorghum_grain_areas[,n],sorghum_silage_areas[,n],potatoes_areas[,n],rye_areas[,n],
           alfalfa_hay_areas[,n],other_hay_areas[,n],soybeans_areas[,n],cropland_pasture_areas[,n],
           noncropland_pasture_areas[,n],rice_areas[,n],peanuts_areas[,n])
  areas_array[,,n]=array(areas,c(length(NASS_County[,1]),16))
  prod = c(corn_grain_prod[,n],corn_silage_prod[,n],wheat_prod[,n],oats_prod[,n],barley_prod[,n],
            sorghum_grain_prod[,n],sorghum_silage_prod[,n],potatoes_prod[,n],rye_prod[,n],
            alfalfa_hay_prod[,n],other_hay_prod[,n],soybeans_prod[,n],cropland_pasture_prod[,n],
            noncropland_pasture_prod[,n],rice_prod[,n],peanuts_prod[,n])
  prod_array[,,n]=array(prod,c(length(NASS_County[,1]),16))
  
  #write data files
  write_name = paste("InputFiles/NASScropareas",year[n],".txt",sep = "")
  write.table(areas_array[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles/NASScropprod",year[n],".txt",sep = "")
  write.table(prod_array[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}

#write keys
write_name = paste("InputFileKeys/NASScropareas&prodkey.txt",sep = "")
write.table(c("areas in km2, production in kg", "cols: [1] corn_grain, [2] corn_silage, [3] wheat_areas, [4] oats_areas, [5] barley_areas,
           [6] sorghum_grain, [7] sorghum_silage, [8] potatoes, [9] rye,
           [10] alfalfa_hay, [11] other_hay, [12] soybeans, [13] cropland_pasture,
           [14] noncropland_pasture, [15] rice, [16] peanuts", "rows: US counties (IDs listed below)", cnty_IDs_NANI_NAPI[,1]), file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
