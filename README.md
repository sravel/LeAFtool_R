# LeAFtool: Lesion Area Finding tool

## About this package

Research on plant leaf diseases requires the acquisition of quantitative data to characterize the symptoms caused by different pathogens. These symptoms are frequently lesions that are differentiated from the leaf blade by their color and texture. Among the variables used to characterize the impact of a disease, the most relevant are the number of lesions per unit of leaf area, the area and the shape of the lesions. Since visual measurements are only possible on small numbers of images, it is necessary to use computerized image analysis procedures.

Existing procedures can partially meet the needs but are not always adapted to the particularities of the images obtained in the laboratory. From a scanned image under laboratory conditions containing several leaves of plants showing symptoms of a known disease, the algorithm developed makes it possible to obtain for each sheet of the image the number and the characteristics of surface and shape. lesions.

The method used is based on a supervised classification of the pixels characterized by the intensities of the red, green, blue channels. The learning set, made from a reference image, contains samples of the three classes: background, limb and lesions, each class can be subdivided into subclasses to improve the accuracy of the detection. Several methods of supervised classification can be used (discriminant factorial analysis, neural networks, machine vector support ...). Noise filtering is done using basic morphological operations. The code is developed under the R software, the image processing operations using the EBImage package.

The LeAFtool (Lesion Area Finding tool) is the created R package. The tool can be used in command line mode, or GUI mode via the Shiny package.
For the learning game and the analysis the same options are available whatever the mode used. The interface contains an editing part of the results. It allows the editing of lesions (suppression of false detection), or to filter the information according to the maximum surface, the shape (round, elongated), ...

The tools are being developed and a first functional version is available. The first tests carried out on 7 patho-systems showed promising results and similar to manual (visual) expertise. We will also improve the portability between different OS and see how to implement it on a shiny server.

## Installation
### Regular installation

  * Main Program: Please copy and paste the following command to R console. 
  * Upgrading R and Rstudio to the latest version (R >= 3.4, Rstudio > 1.0.0) is strongly recommended. 

```
# Dependecies that needs to be manually installed.
# You may need to paste the following code line by line 
# and choose if previously installed packages should be updated (recommended).

# list of packages required
list.of.packages <- c("RCurl","shiny","shinythemes","shinydashboard","shinyFiles","shinyBS","shinyjs", "DT","EBImage","MASS","lattice",
                      "foreach","future","doParallel","shinyFeedback","colourpicker","shinyhelper", "shinyjqui", "rstudioapi", "ggplot2")
#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# Install LeAFtool
install_github("sravel/LeAFtool")

```

#### Running LeAFtool with GUI

  * To run the application LeAFtool
```
library(LeAFtool)
runLeAFtool()
```


## User manual

See here: https://rawgit.com/qinzhu/PIVOT/master/inst/app/www/manual_file.html 

## Troubleshooting

## Citation

