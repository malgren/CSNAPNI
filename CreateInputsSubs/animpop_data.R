# animpop_data.r
## This script pulls county animal population data (3111 counties from the continental US) for 1997-2017
#from NANI_NAPI_NASS_pull.r
#uses "anim_avg_inv_array"

if(print_tags == 1){
  print("CreateInputsSubs/animpop_data.R")
}


# Specify number and names of crop categories
n_animtyp = 19 #number of crops to import
animtyp=array("",c(n_crops))
animtyp[1] = 'fattened cattle'
animtyp[2] = 'milk cows'
animtyp[3] = 'hogs for breeding'
animtyp[4] = 'hogs for slaughter'
animtyp[5] = 'chicken layers'
animtyp[6] = 'breeding turkeys'
animtyp[7] = 'chicken pullets'
animtyp[8] = 'chicken broilers'
animtyp[9] = 'slaughter turkeys'
animtyp[10] = 'beef breeding herd'
animtyp[11] = 'beef calves'
animtyp[12] = 'dairy calves'
animtyp[13] = 'beef heifers'
animtyp[14] = 'dairy heifers'
animtyp[15] = 'beef stockers'
animtyp[16] = 'dairy stockers'
animtyp[17] = 'sheep'
animtyp[18] = 'horses'
animtyp[19] = 'goats'

# allocate space to matrices
animpopcnty = array(0,c(n_cnty,length(animtyp),length(import_yrs)))
animpopcnty = anim_avg_inv_array
animpopws = array(0,c(n_ws_tbx,length(animtyp),length(import_yrs)))
for(n in 1:length(import_yrs)){
  # watershed crop production
  animpopws[,,n] = t(cnty_ws)%*%animpopcnty[,,n]
  #write data files
  ##animal population
  write_name = paste("InputFiles/noanimdyn",run_yrs[n],".txt",sep = "")
  write.table(animpopws[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
  write_name = paste("InputFiles/noanimdyncty",run_yrs[n],".txt",sep = "")
  write.table(animpopcnty[,,n], file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
}
#write key
##animal population
write_name = "InputFileKeys/noanimdyn_key.txt"
animpopws_key = array(" ", c(n_ws_tbx+1,length(animtyp)+1))
animpopws_key[1,]=c(" ", animtyp) #column headings
animpopws_key[,1]=c("ws_num", 1:n_ws_tbx) #row headings
write.table(animpopws_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
write_name = "InputFileKeys/noanimdyncty_key.txt"
animpopcnty_key = array(" ", c(n_cnty+1,length(animtyp)+1))
animpopcnty_key[1,]=c(" ", animtyp) #column headings
animpopcnty_key[,1]=c("ws_num", 1:n_cnty) #row headings
write.table(animpopcnty_key, file = write_name, sep = " ", row.names = FALSE, col.names = FALSE)
animpoptotal = drop(colSums(animpopcnty)) #total animal populations in each year