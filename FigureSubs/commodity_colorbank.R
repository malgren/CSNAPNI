#commodity_colorbank.R
## This script assigns colors to data categories. It was originally just for commodities, but now it's also for the broader NANI and NAPI contributions

#NANI/NAPI
atmdep = rgb(200/260, 220/260, 254/260) #bluish
biofix = rgb(150/260, 200/260, 10/260) #greenish
agfert = rgb(0.9,0.223,0.180) #redish
nff = rgb(160/260, 39/260, 185/260) #purple, net food and feed
nagfert = rgb(255/260, 255/260, 130/260) #yellow
nffert = rgb(209/260, 165/260, 138/260) #tan, non food fertilizer
det = rgb(255/260, 140/260, 0/260) #orange
P_diet_supp = "#ADCCF6" #light blue

#NANI
NANI_colors = c(atmdep,biofix,agfert,nff,nagfert,nffert)

#NAPI
NAPI_colors = c(agfert,nff,nagfert,nffert,det,P_diet_supp)

#crop commodities
corn = rgb(255/260, 46/260, 67/260) #corn
csilage = rgb(224/300, 67/300, 117/300) #corn silage
wheat = rgb(225/260, 245/260, 254/260) #wheat
oats = rgb(156/260, 39/260, 176/260) #oats
barley = rgb(240/240, 120/240, 146/240) #barley
sorghum = rgb(25/260, 118/260, 210/260) #sorghum
ssilage = rgb(144/260, 202/260, 249/260) #sorghum silage
potatoes = rgb(255/260, 255/260, 141/260) #potatoes
rye = rgb(199/260, 0/260, 57/260) #rye
ahay = rgb(214/260, 183/260, 92/260) #alfalfa hay
nahay = rgb(158/260, 124/260, 22/260) #non alfalfa
soy = rgb(255/260, 182/260, 38/260) #soybeans
cpasture = rgb(127/260, 81/260, 52/260) #cropland pasture
ncpasture = rgb(209/260, 165/260, 138/260) #non cropland
rice = rgb(239/260, 235/260, 233/260) #rice
peanuts = rgb(109/260, 76/260, 65/260) #peanuts
CGF = rgb(244/260, 115/260, 106/260) #CGF
CGM = rgb(244/260, 147/260, 141/260) #CGM
DGS = rgb(244/260, 185/260, 181/260) #DGS
etoh = rgb(247/260, 29/260, 127/260) #ethanol

crop_colors = c(corn,csilage,wheat,oats,barley,sorghum,ssilage,potatoes,rye,ahay,nahay,soy,cpasture,ncpasture,rice,peanuts,CGF,CGM,DGS,etoh)

