# Settings.R
## This file contains all user-adjustable input parameters for CSNAPNI.

#Regenerate input files? 0=no, 1=yes
get_new_data=0
print(paste("Input files regenerated (0=no, 1=yes): ", get_new_data),quote=FALSE)

##input file creation settings
  ##allocation method
  alloc_methods = c("output masses", "energy content", "market value", "input mass (i.e., corn starch content)", "no allocation to ethanol","no allocation to feed coproducts")
  alloc_method = 4
  print(paste("Ethanol and coproduct allocation method: ", alloc_methods[alloc_method]),quote=FALSE)

#protein assumptions
protassump=1
if(protassump==1){
  print(paste("Using Costello's protein assumptions for meat."),quote=FALSE)
}else if(protassump==2){
  print(paste("Using DeVries' protein assumptions for meat."),quote=FALSE)
}

#data years to load
run_yrs = c("97","02","07","12","17") #last two digits of data years to import
nyrs = length(run_yrs)
print(paste("Running for the following year(s): "), quote=FALSE)
print(paste(run_yrs), quote=FALSE)

ws_name = "Miss_Atch"
ws = c(22,25) ##Missippi River and Atchafalaya Watersheds
#ws = c(52:60) ##Chesapeake Bay Watersheds (Patuxent,Potomac,Rappahannock,
#York,James,Chester,Choptank,Tangier/Pokomoke,Chesapeake Bay Mainstream)

                      
