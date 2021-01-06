#fig_setup.R
source("FigureSubs/commodity_colorbank.r")
year_labels = c(1997,2002,2007,2012,2017)
crop_labels = c("corn grain","corn silage","wheat","oats","barley","sorghum grain","sorghum silage",
                "potatoes","rye","alfalfa hay","other hay","soybeans","cropland pasture","noncropland pasture",
                "rice","peanuts","CGF","CGM","DGS")
meat_labels = c("beef","dairy","swine","sheep","horse","layers","broilers",
                "turkey","goats")
corn_labels = c("Corn not used for ethanol","Corn for dry milling","Corn for wet milling")
NANI_labels = c("Atm N Dep", "Fix N", "Fert N", "Net Food and Feed Imports")