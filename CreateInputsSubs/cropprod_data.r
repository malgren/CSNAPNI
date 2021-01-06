#cropprod_data.r
#This script uses the county-level crop production data (3111 counties from the continental US) for 1997-2017
#from NANI_NAPI_NASS_pull.r
#uses "prod_array"

cropname=array("",c(n_crops))
cropname[1] = 'corn for grain'
cropname[2] = 'corn for silage'
cropname[3] = 'wheat'
cropname[4] = 'oats'
cropname[5] = 'barley'
cropname[6] = 'sorghum for grain'
cropname[7] = 'sorghum for silage'
cropname[8] = 'potatoes'
cropname[9] = 'rye'
cropname[10] = 'alfalfa hay'
cropname[11] = 'other hay'
cropname[12] = 'soybeans'
cropname[13] = 'cropland pasture'
cropname[14] = 'noncropland pasture'
cropname[15] = 'rice'
cropname[16] = 'peanuts'
cropname[17] = 'CGF'
cropname[18] = 'CGM'
cropname[19] = 'DGS'

# allocate space to matrices
cropprodcnty = array(0,c(n_cnty,length(cropname),length(import_yrs)))
cropprodcnty[,1:(n_crops-3),] = prod_array
etohprodcnty = array(0,c(n_cnty,length(import_yrs)))
cropprodws = array(0,c(n_ws_tbx,length(cropname),length(import_yrs)))
etohprodws = array(0,c(n_ws_tbx,length(import_yrs)))
cropproddensws = array(0,c(n_ws_tbx,length(cropname),length(import_yrs)))
etohproddensws = array(0,c(n_ws_tbx,length(import_yrs)))
cornprodnoetoh=array(0,c(n_cnty,length(import_yrs)))

for(n in 1:(length(import_yrs))){ 
  # build a matrix of extracted data
  for(j in 1:n_cnty){ #rows (counties)
    # calc amounts of etoh coproducts
    cropprodcnty[j,(n_crops-2),n] = cornuse[5,n]*cropprodcnty[j,1,n]*CGF_from_corn #CGF (the CGF produced by corn reported by the USDA in "alcohol for fuel" that is not DDGS)
    cropprodcnty[j,(n_crops-1),n] = cornuse[5,n]*cropprodcnty[j,1,n]*CGM_from_corn #CGM (the CGM produced by corn reported by the USDA in "alcohol for fuel" that is not DDGS)
    cropprodcnty[j,n_crops,n] = cornuse[6,n]*cropprodcnty[j,1,n]*DGS_from_corn #DGS (the DDGS produced by corn from ethanol plants, as reported by the USDA)
    etohprodcnty[j,n] = (cornuse[5,n]+cornuse[6,n])*cropprodcnty[j,1,n]*etoh_from_corn[n] #liters of etoh from corn for etoh, assumption that everry county contributes equally to corn for ethanol
    # calc new corn total
    cornprodnoetoh[j,n] = drop(cropprodcnty[j,1,n])
    cropprodcnty[j,1,n] = cornprodnoetoh[j,n]*(1-(cornuse[5,n]+cornuse[6,n])) #proportion of corn not allocated to fuel ethanol production
    #pastures: "take half, leave half"
    cropprodcnty[j,13:14,n] = cropprodcnty[j,13:14,n]/2
  }
  # watershed crop production
  cropprodws[,,n] = t(cnty_ws)%*%cropprodcnty[,,n]
  etohprodws[,n] = t(cnty_ws)%*%etohprodcnty[,n]
  for(i in 1:(length(cropname))){ #columns (crops)
    cropproddensws[,i,n] = cropprodws[,i,n]/area
  }
  etohproddensws[,n] = etohprodws[,n]/area
  
  #write data files
  ##crop production
  write_name = paste("InputFiles/cropprod",run_yrs[n],".txt",sep = "")
  write.table(cropproddensws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  ##total corn production
  write_name = "InputFiles/cornprodnoetoh.txt"
  write.table(cornprodnoetoh, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
##ethanol production
write_name = paste("InputFiles/etohproddensws.txt",sep = "")
write.table(etohproddensws, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)

#write keys
##crop production
write_name = "InputFileKeys/cropprod_key.txt"
cropprod_key = array(" ", c(n_ws_tbx+1,length(cropname)+1))
cropprod_key[1,]=c(" ", cropname) #column headings
cropprod_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(cropprod_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
cropprodtotal = colSums(cropprodcnty) #total crop prod in each year
##total corn production
write_name = "InputFileKeys/cornprodnoetoh_key.txt"
cornprodnoetoh_key = array(" ", c(n_cnty+1,length(import_yrs)+1))
cornprodnoetoh_key[1,]=c(" ", import_yrs) #column headings
cornprodnoetoh_key[,1]=c("total_corn", 1:n_cnty) #row headings
##ethanol production
write_name = "InputFileKeys/etohproddensws_key.txt"
etohproddensws_key = array(" ", c(n_ws_tbx+1,length(import_yrs)+1))
etohproddensws_key[1,]=c(" ", import_yrs) #column headings
etohproddensws_key[,1]=c("L/km^2", 1:n_ws_tbx) #row headings
write.table(etohproddensws_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)