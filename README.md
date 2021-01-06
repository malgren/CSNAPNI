# CSNAPNI
The CSNAPNI model is designed to estimate nutrient inputs (real and embodied) to specific products, including crops, corn ethanol,
and animal products, in addition to total nutrient inputs at the county and watershed scale for the conterminous United States (US).

To run the model, download the repository, unzip it, then open Config/Settings.R.

In Config/Settings.R, set the working directory to the file path corresponding to the repository folder, *CSNAPNI-master*.
 * One way to do this is to open README.md, go up to the menu bar in RStudio, and click "Session>>Set Working Directory>>To Source File Location." 
The correct setwd() command can now be copied from the RStudio Console.

Open CSNAPNIv1.R or CSNAPNIv1.Rmd in R Studio.

Run CSNAPNIv1.R to generate all output files quickly, and to enable model variables to be probed from within R.

Knit CSNAPNIv1.Rmd to generate the HTML model guide that will walk you through model functionality, inputs, and outputs.

Some tabled outputs will be saved in the “OutputFiles” folder after the model is run.
