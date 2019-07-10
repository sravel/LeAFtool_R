![LeAFtool Logo](/inst/app/www/LeAFtool-long.png)

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
  * Upgrading R and Rstudio to the latest version (R >= 3.5, Rstudio > 1.0.0) is strongly recommended.

```
# Install or update LeAFtool
library(devtools)
install_github("sravel/LeAFtool")

```

## Running LeAFtool with GUI

* To run the application LeAFtool
```
library(LeAFtool)
runLeAFtool()
```

## Running LeAFtool without GUI (call direct function)

### Training

The function return the confusion matrix and error rate.

```
library(LeAFtool)
training(pathTraining, method = "lda", transform = NULL,
  colormodel = "rgb")
```
* pathTraining	The path of the folder containing sampled images for training. This folder must contain at least 3 sub-folders with name 'background', 'limb' and 'lesion'.
* method	Method of discrimainant analysis: "lda" (default) or "qda"
* transform	 transformation before analysis (e.g. sqrt) # not avail on GUI
* colormodel	 Model of color for the analysis: "rgb" (default) or "hsv"

```
## Examples

pathTraining <- '/media/sebastien/LaAFtool/exemples/exemple1/learning'
confusionMatrix <- training(pathTraining)
training(pathTraining, transform=function(x) log1p(x),colormodel='rgb', method='svm')
training(pathTraining, colormodel='hsv', method='lda')
training(pathTraining, transform=function(x) (sin(pi*(x-0.5))+1)/2, method='qda')
training(pathTraining, transform=function(x) asin(2*x-1)/pi+0.5)
training(pathTraining, transform=log1p)
```

### Analysis

The function return a dataframe with file name, exit status and message if error.

```
library(LeAFtool)
analyseImages(pathTraining, pathResult, pathImages, fileImage = NA,
  leafAreaMin = 1000, leafBorder = 5, lesionBorder = 3,
  lesionAreaMin = 10, lesionAreaMax = 120000,
  lesionEccentricityMin = 0, lesionEccentricityMax = 1,
  lesionColorBorder = "#0000FF11", lesionColorBodies = "#FE8E0000",
  blurDiameter = 0, outPosition = "right", parallelThreadsNum = 1)
```
* pathTraining	The path of the directory containing the sampled images used for training. After the training step, this directory contains the parameters of the training set.
* pathResult	The path of the directory where to store the result files (created by the function if it does not exist).
* pathImages	The path of the directory containing the images to analyse.
* fileImage	A character vector containg the fils names of the images to analyse in pathImages (NA to analyse all the images in pathImages).
* leafAreaMin	The minimum area of a leaf (in pixels) Default:1000.
* leafBorder	The diameter of the brush (in pixels) used to erode the leafBorder Default:5.
* lesionBorder	The diameter of the brush (in pixels) used to erode the lesionBorder Default:3.
* lesionAreaMin	The minimum area of a lesion (in pixels) Default:10.
* lesionAreaMax	The maximum area of a lesion (in pixels) Default:120000.
* lesionEccentricityMin	The minimum eccentricity of a lesion Default:0.
* lesionEccentricityMax	The maximum eccentricity of a lesion Default:1.
* lesionColorBorder	hexadecimal code for output fill color for lesion in the output image Default:#0000FF (blue).
* lesionColorBodies	hexadecimal code for output bodies color for lesion in the output image Default:#FE8E0000 (transparent).
* blurDiameter	The diameter of the brush (in pixels) used to blur the image (0 for no blur) Default:0)'.
* outPosition	join origale and color lesion image at right or buttom Default:right)'.
* parallelThreadsNum	number of thread use, 1 thread analysis 1 image if >= 2 Default:1)'.

```
## Examples

dataframeExitStatus <- analyseImages(pathTraining = "/media/sebastien/LaAFtool/exemples/exemple1/learning",
             pathResult = "/media/sebastien/LaAFtool/exemples/exemple1/results",
             pathImages = "/media/sebastien/LaAFtool/exemples/exemple1/samples", parallelThreadsNum=22)

dataframeExitStatus <- analyseImages(pathTraining = "/media/sebastien/LaAFtool/exemples/exemple1/learning",
              pathResult = "/media/sebastien/LaAFtool/exemples/exemple1/results",
              pathImages = "/media/sebastien/LaAFtool/exemples/exemple1/samples",
              leafAreaMin = 600,
              leafBorder = 130,
              parallelThreadsNum = 22)
```

## User manual

See here: https://docs.google.com/document/d/1lFr8_08TGJps5lcSbY_AimstFnf0AfuOX7tY1MfkDv8/edit?usp=sharing

## Troubleshooting

#### install on linux

if install *devtools* fail please check you have the library:
```
sudo apt install libxml2-dev libcurl4-openssl-dev libssl-dev -y
R
install.package("devtools")
```

if install *LeAFtool* fail please check you have the library:
```
sudo apt install libtiff5-dev libfftw3-dev -y
```

## Citation

## Other

<object data="https://github.com/sravel/LeAFtool/blob/master/inst/app/www/posterLeAFtool-JOBIM2019-final.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="https://github.com/sravel/LeAFtool/blob/master/inst/app/www/posterLeAFtool-JOBIM2019-final.pdf">
        <p>This browser does not support PDFs. Please download the PDF to view it: <a href="https://github.com/sravel/LeAFtool/blob/master/inst/app/www/posterLeAFtool-JOBIM2019-final.pdf">Download PDF</a>.</p>
    </embed>
</object>
