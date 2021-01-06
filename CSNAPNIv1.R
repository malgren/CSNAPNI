#CSNAPNIv1.R

#settings
source("Config/Settings.R")

#pull data from raw data files
if (get_new_data == 1){
  source("ModelSubs/CreateInputs.R")
}
source("ModelSubs/LoadData.R")

##Calculate recoverable manure for NANI/NAPI Toolbox Watersheds
source("ModelSubs/manure.R")

##Sort crop, animal, and manure production, NANI and NAPI data, human populations, and areas into NEEA watersheds
source("ModelSubs/NEEAshedsdyn.R")

## DETERMINE HUMAN N and P REQUIREMENTS
source("ModelSubs/hmn_reqs.R")

## CALCULATE CROP PRODUCTION SUBROUTINE
source("ModelSubs/CropProd.R")

## CALCULATE MEAT PRODUCTION
source("ModelSubs/meat_alloc.R") #creates kgmeat

## ANIMAL AND CROPS AS FOOD
source("ModelSubs/food_totals.R")

## CROP, ETHANOL, and CROPRODUCT FERTILIZER AND FIXATION
source("ModelSubs/Cprodfertfix.R")

## MEAT FERT, FIX, NH3, N2O, CH4, LU, and MANURE
source("ModelSubs/Mprodimpacts.R")

## IMPACTS PER NUTRITIONAL UNIT
source("ModelSubs/pernutrition.R")

## calculates domestic meat availability given trade, also calculates N lost due to food waste SUBROUTINE
#Temporarily commented-out - *this code has not bee converted to R and updated for additional years yet*
#meattrade

## BUILD FINAL NANI/NAPI MATRICES
source("ModelSubs/NPinputs_aggregate.R")

## WRITE OUTPUT DATA TO TEXT FILES
source("ModelSubs/write_outputs.R")