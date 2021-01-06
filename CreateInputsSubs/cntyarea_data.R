#This script pulls county areas (3111 counties from the continental US) from the NANI Accounting Tool V3.1.0
read_file = "RawData/NANI_Accounting_Tool_V3.1.0.xlsm"
read_sheet = "Ag_Census_Crops_12"  #several sheets contain the area (km2) data, so I just picked one
data = as.matrix(read_excel(read_file, sheet = read_sheet))
#Get county areas
areakm2_cnty = as.numeric(data[2:3112,4])