# Settings.R
## This file contains all user-adjustable input parameters for CSNAPNI.
#test
#Print Troubleshooting Tags
print_tags = 0
if(print_tags == 1){
  print("Config/Settings.R")
}

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

#animal P intake
#animdatadyn = t(array(scan("InputFiles/animdatadyn_min.txt"), c(23,19))) #minimum P intakes
animdatadyn = t(array(scan("InputFiles/animdatadyn_max.txt"), c(23,19))) #maximum P intakes
#animdatadyn = t(array(scan("InputFiles/animdatadyn_Pdigest.txt"), c(23,19))) #digestibility-reduced P intakes and P excreted for swine and poultry

#data years to load
run_yrs = c("97","02","07","12","17") #last two digits of data years to import
nyrs = length(run_yrs)
print(paste("Running for the following year(s): "), quote=FALSE)
print(paste(run_yrs), quote=FALSE)

ws_name = "Miss_Atch"
ws = c(22,25) ##Missippi River and Atchafalaya Watersheds
#ws = c(52:60) ##Chesapeake Bay Watersheds (Patuxent,Potomac,Rappahannock,
#York,James,Chester,Choptank,Tangier/Pokomoke,Chesapeake Bay Mainstream)

                      
