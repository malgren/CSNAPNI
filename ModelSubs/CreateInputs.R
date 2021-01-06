# CreateInputs.R
## This file generates input files needed to run NANI and NAPI by loading and formatting data from various sources.
## All the source files are in the "Raw Data" folder.

# Start the clock!
ptm <- proc.time()

# * = still needs to be updated for 2017

source("CreateInputsSubs/constants.r")

#####################################################################################
##County area data (from NANI Accounting Tool V3.1.0)
source("CreateInputsSubs/cntyarea_data.r")

#####################################################################################
##Corn disappearance proportions (from USDA ERS DATA)
source("CreateInputsSubs/cornuse_data.r")

#####################################################################################
##County population data (from NANI Accounting Tool V3.1.0, original source: US Census)
source("CreateInputsSubs/population_data.r") # *using 2012 for 2017 currently

#####################################################################################
##Census of agriculture data (crop harvested areas and production, animal average populations) 
#(from NASS QuickStats database, original source: USDA Ag Census)
source("CreateInputsSubs/NASSpull/NANI_NAPI_NASS_pull.r") #check cropprod, animpop, and harvestedarea scripts
source("CreateInputsSubs/cropprod_data.r")
source("CreateInputsSubs/animpop_data.r")
source("CreateInputsSubs/harvestedarea_data.r")

#####################################################################################
##Crop export data (from USDA NASS and GATS databases)
source("CreateInputsSubs/export_data.r")

#####################################################################################
##Crop allocation to animals (methods from Costello 2015)
source("CreateInputsSubs/cropalloc_model.r")

#####################################################################################
##P diet supplementation to animals (NEW methods)
source("CreateInputsSubs/P_diet_supp.r")

#####################################################################################
##NANI totals from the VB model (from NANI Accounting Tool V3.1.0)
source("CreateInputsSubs/NANItotals_vbmodel.r") # *used data from 2012 for 2017, (2002 for 2012 and 2017 for atm N dep)

#####################################################################################
##NAPI totals from the VB model (from NAPI Accounting Tool V3.1.0)
source("CreateInputsSubs/NAPItotals_vbmodel.r") # *used data from 2012 for 2017

#####################################################################################
##Meat production and emissions data (from original commodity-specific NANI for 2002)
source("CreateInputsSubs/meat_data.r")
#add Mtrade data

#####################################################################################
##Ethanol production data (from USDA ERS)
source("CreateInputsSubs/etoh_data.r")

#####################################################################################
##Crop fertilization rate data (from USDA ERS and NASS, and MN extension recommendations)
source("CreateInputsSubs/fertrates_data.r")

#####################################################################################
##Food waste data (from original commodity-specific NANI for 2002)
#source("foodwaste_data.r") # *
#still needs to be scripted for years other than 02 and 07

# Stop the clock
runtime = proc.time() - ptm
runtime_msg = paste("input files created in", runtime, "seconds", sep = " ")
print(runtime_msg[3])