#harvestedarea_data.r
#This script uses the county-level crop production data (3111 counties from the continental US) for 1997-2017
#from NANI_NAPI_NASS_pull.r
#uses "areas_array"
#and aggregates it to crop-specific totals for each year (which we may not want to do in the future)
#and allocates virtual areas to ethanol and coproducts

cropareacty = array(0,c(n_cnty,n_crops,length(import_yrs)))
cropareacty[,1:(n_crops-3),] = areas_array
cropareaws=array(0,c(n_ws_tbx,n_crops,length(import_yrs)))
cornareanoetoh=array(0,c(n_cnty,length(import_yrs)))
yr_col=array(0,c(length(import_yrs),1))
for(n in 1:length(import_yrs)){
  # calc amounts of etoh coproducts
  cropareacty[,(n_crops-2),n]=cornuse[5,n]*cropareacty[,1,n]*to_FC_wetmill[alloc_method]*wetmill_CGF
  cropareacty[,(n_crops-1),n]=cornuse[5,n]*cropareacty[,1,n]*to_FC_wetmill[alloc_method]*wetmill_CGM
  cropareacty[,(n_crops),n]=cornuse[6,n]*cropareacty[,1,n]*to_FC_drymill[alloc_method]
  # calc new corn area
  cornareanoetoh[,n] = drop(cropareacty[,1,n])
  cropareacty[,1,n] = cornareanoetoh[,n]*(1-(cornuse[5,n]+cornuse[6,n])) #proportion of corn not allocated to fuel ethanol production
  #solve NaN issue
  for(m in 1:16){
   ind=which(is.na(cropareacty[,m,n]) %in% 1)
   if(length(ind)>0){
     for(i in 1:length(ind)){
       cropareacty[ind[i],m,n]=0
     }
    }
  }
cropareaws[,,n]=t(cnty_ws)%*%cropareacty[,,n]
}
croparea=colSums(cropareaws)
cornareanoetohsum=colSums(cornareanoetoh)
etoh_landuse = cornuse[5,]*cornareanoetohsum*(1-to_FC_wetmill[alloc_method])+cornuse[6,]*cornareanoetohsum*(1-to_FC_drymill[alloc_method]) #ag land use for etoh

#write files
write_name = paste("InputFiles/corntotareaharvested.txt")
write.table(cornareanoetohsum, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles/cropareaharvested.txt")
write.table(croparea, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = paste("InputFiles/etoh_landuse_harvestedarea.txt")
write.table(etoh_landuse, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write key
write_name = paste("InputFileKeys/cropareaharvested_key.txt")
cropareaharvested_key = array(" ", c(n_crops+1,length(import_yrs)+1))
cropareaharvested_key[1,] = c(" ", import_yrs) #column headings
cropareaharvested_key[,1]=c("crop", cropname) #row headings
write.table(cropareaharvested_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
