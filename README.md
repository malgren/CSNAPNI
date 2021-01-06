# CSNAPNI
The CSNAPNI model is designed to estimate nutrient inputs (real and embodied) to specific products, including crops, corn ethanol, and animal products, 
in addition to total nutrient inputs at the county and watershed scale for the conterminous United States (US).

To run the model, download the repository, then open Config/Settings.R.

In Config/Settings.R, ensure that the variable "get_new_data" is set to 1. This will prompt model input files to be generated from raw data on the first run.

Open CSNAPNIv1.R or CSNAPNIv1.Rmd in R Studio.

Use CSNAPNIv1.R to generate all output files quickly, and to enable model variables to be probed from within R.

Use CSNAPNIv1.Rmd to walk through model functionality, inputs, and outputs. Click “Knit” to generate the HTML model guide.

Some tabled outputs will be saved in the “OutputFiles” folder after the model is run.
