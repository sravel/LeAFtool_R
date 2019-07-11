![LeAFtool Logo](/inst/app/www/LeAFtool-long.png)

## Table of Contents
<!-- TOC depthFrom:2 depthTo:3 withLinks:1 updateOnSave:1 orderedList:0 -->
- [About this package](#about-this-package)
- [Installation](#installation)
- [Running LeAFtool with GUI](#running-leaftool-with-gui)
- [Running LeAFtool without GUI (call direct function)](#running-leaftool-without-gui-call-direct-function)
	- [Training](#training)
	- [Analysis](#analysis)
- [Troubleshooting](#troubleshooting)
- [Citation](#citation)
- [License](#license)
- [Other](#other)

<!-- /TOC -->


## About this package

Research on plant leaf diseases requires the acquisition of quantitative data to characterize the symptoms caused by different pathogens. These symptoms are frequently lesions that are differentiated from the leaf blade by their color and texture. Among the variables used to characterize the impact of a disease, the most relevant are the number of lesions per unit of leaf area, the area and the shape of the lesions. Since visual measurements are only possible on small numbers of images, it is necessary to use computerized image analysis procedures.

Existing procedures can partially meet the needs but are not always adapted to the particularities of the images obtained in the laboratory. From a scanned image under laboratory conditions containing several leaves of plants showing symptoms of a known disease, the algorithm developed makes it possible to obtain for each sheet of the image the number and the characteristics of surface and shape. lesions.

The method used is based on a supervised classification of the pixels characterized by the intensities of the red, green, blue channels. The learning set, made from a reference image, contains samples of the three classes: background, limb and lesions, each class can be subdivided into subclasses to improve the accuracy of the detection. Several methods of supervised classification can be used (discriminant factorial analysis, neural networks, machine vector support ...). Noise filtering is done using basic morphological operations. The code is developed under the R software, the image processing operations using the EBImage package.

The LeAFtool (Lesion Area Finding tool) is the created R package. The tool can be used in command line mode, or GUI mode via the Shiny package.
For the learning game and the analysis the same options are available whatever the mode used. The interface contains an editing part of the results. It allows the editing of lesions (suppression of false detection), or to filter the information according to the maximum surface, the shape (round, elongated), ...

The tools are being developed and a first functional version is available. The first tests carried out on 7 patho-systems showed promising results and similar to manual (visual) expertise. We will also improve the portability between different OS and see how to implement it on a shiny server.

## Installation

  * Main Program: Please copy and paste the following command to R console.
  * Upgrading R and Rstudio to the latest version (R >= 3.5, Rstudio > 1.0.0) is strongly recommended.

``` ruby
#### Install or update LeAFtool
library(devtools)
install_github("sravel/LeAFtool")

```

## Running LeAFtool with GUI

* To run the application LeAFtool
```ruby
library(LeAFtool)
runLeAFtool()
```

## Running LeAFtool without GUI (call direct function)

### Training

Compute and saves on disk the parameters of the training set
Training input folder must include sub-folders:
- limb
- background
- lesion

This sub-folder can contain either image files or sub-folders containing different groups of image files

The function return the confusion matrix and error rate.

```ruby
library(LeAFtool)
pathTraining <- '../Exemple1/learning/' ## FOR all OS (Linux Mac Windows)
pathTraining <- '..\\Exemple1\\learning' ## FOR windows only
training(pathTraining,
         method = "lda",
         transform = NULL,
         colormodel = "rgb"
        )
```
* __pathTraining__	The path of the folder containing sampled images for training. This folder must contain at least 3 sub-folders with name 'background', 'limb' and 'lesion'.
* __method__	Method of discrimainant analysis: "lda" (default) or "qda"
* __transform__	 transformation before analysis (e.g. sqrt) # not avail on GUI
* __colormodel__	 Model of color for the analysis: "rgb" (default) or "hsv"

```ruby
#### Examples
pathTraining <- '/media/sebastien/LaAFtool/exemples/exemple1/learning'
confusionMatrix <- training(pathTraining)
training(pathTraining, transform=function(x) log1p(x),colormodel='rgb', method='svm')
training(pathTraining, colormodel='hsv', method='lda')
training(pathTraining, transform=function(x) (sin(pi*(x-0.5))+1)/2, method='qda')
training(pathTraining, transform=function(x) asin(2*x-1)/pi+0.5)
training(pathTraining, transform=log1p)
```

### Analysis

Analyse an image or a set of images.
Analysis step can use many ram on parallel mode.

The function return a dataframe with file name, exit status and message if error.

```ruby
library(LeAFtool)
analyseImages(pathTraining, pathResult, pathImages, fileImage = NA,
  leafAreaMin = 1000, leafBorder = 5, lesionBorder = 3,
  lesionAreaMin = 10, lesionAreaMax = 120000,
  lesionEccentricityMin = 0, lesionEccentricityMax = 1,
  lesionColorBorder = "#0000FF11", lesionColorBodies = "#FE8E0000",
  blurDiameter = 0, outPosition = "right", parallelThreadsNum = 1)
```

* __pathTraining__	The path of the directory containing the sampled images used for training. After the training step, this directory contains the parameters of the training set.
* __pathResult__	The path of the directory where to store the result files (created by the function if it does not exist).
* __pathImages__	The path of the directory containing the images to analyse.
* __fileImage__	A character vector containg the fils names of the images to analyse in pathImages (NA to analyse all the images in pathImages).
* __leafAreaMin__	The minimum area of a leaf (in pixels) Default:1000.
* __leafBorder__	The diameter of the brush (in pixels) used to erode the leafBorder Default:5.
* __lesionBorder__	The diameter of the brush (in pixels) used to erode the lesionBorder Default:3.
* __lesionAreaMin__	The minimum area of a lesion (in pixels) Default:10.
* __lesionAreaMax__	The maximum area of a lesion (in pixels) Default:120000.
* __lesionEccentricityMin__	The minimum eccentricity of a lesion Default:0.
* __lesionEccentricityMax__	The maximum eccentricity of a lesion Default:1.
* __lesionColorBorder__	hexadecimal code for output fill color for lesion in the output image Default:#0000FF (blue).
* __lesionColorBodies__	hexadecimal code for output bodies color for lesion in the output image Default:#FE8E0000 (transparent).
* __blurDiameter__	The diameter of the brush (in pixels) used to blur the image (0 for no blur) Default:0)'.
* __outPosition__	join origale and color lesion image at right or buttom Default:right)'.
* __parallelThreadsNum__	number of thread use, 1 thread analysis 1 image if >= 2 Default:1)'.

```ruby
#### Examples
dataframeExitStatus <- analyseImages(pathTraining = "../exemple1/learning",
             pathResult = "../exemple1/results",
             pathImages = "../exemple1/samples",
             parallelThreadsNum = 8
             )

analyseImages(pathTraining = "../exemple1/learning",
              pathResult = "../exemple1/results",
              pathImages = "../exemple1/samples",
              leafAreaMin = 600,
              leafBorder = 130,
              parallelThreadsNum = 22)
```
<!--
## User manual

See here: https://docs.google.com/document/d/1lFr8_08TGJps5lcSbY_AimstFnf0AfuOX7tY1MfkDv8/edit?usp=sharing
-->

## Troubleshooting

#### install on linux

if install *devtools* fail please check you have the library:
```ruby
sudo apt install libxml2-dev libcurl4-openssl-dev libssl-dev -y
```

if install *LeAFtool* fail please check you have the library:
```ruby
sudo apt install libtiff5-dev libfftw3-dev -y
```

## Citation
The paper is currently in prep.

## License

LGPL-3 | file LICENSE

## Other

#### Poster

![LeAFtool poster](/inst/app/www/posterLeaftool-JOBIM2019.png)
