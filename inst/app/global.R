#!/usr/bin/Rscript

#####################################################################################################
#
# Copyright 2019 CIRAD-INRA
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/> or
# write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.
#
# You should have received a copy of the CeCILL-C license with this program.
# If not see <http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.txt>
#
# Intellectual property belongs to CIRAD and South Green developpement plateform
# Version 0.1.0 written by Sebastien RAVEL, François BONNOT, Sajid ALI, FOURNIER Elisabeth
#####################################################################################################
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

# list of packages required
list.of.packages <- c("RCurl","shiny","shinydashboard","shinyFiles","shinyjs", "DT","EBImage","MASS","lattice",
                      "foreach","doParallel","shinyFeedback","colourpicker","shinyhelper", "shinyjqui", "ggplot2","ParallelLogger")


#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

#Case of EBImage
if (!require('EBImage')) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("EBImage")
}

#Load all library
library(shiny, quietly = TRUE, warn.conflicts = FALSE)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
library(shinyFiles, quietly = TRUE, warn.conflicts = FALSE)
library(DT, quietly = TRUE, warn.conflicts = FALSE)
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE)
library(shinyFeedback, quietly = TRUE, warn.conflicts = FALSE)
library(shinyhelper, quietly = TRUE, warn.conflicts = FALSE)
library(colourpicker, quietly = TRUE, warn.conflicts = FALSE)
library(shinyjqui, quietly = TRUE, warn.conflicts = FALSE)

# calibration
library(EBImage)
library(lattice)
library(MASS)
library(ggplot2)

# analysis
library(future)
library(foreach)
library(doParallel)
library(ParallelLogger, quietly = TRUE, warn.conflicts = FALSE)

plan(multiprocess)

############################################
## Global functions
############################################

# to only 2 digits after dot
options(digits=2)

# auto-detect number of core on computer
max_no_cores <- as.numeric(max(1, detectCores() - 2))

# All reactive values for program
rv <<- reactiveValues(
                        # for calibration
                        exitStatusCal = -1, messCal = NULL, errCal = NULL,
                        dirTraining = NULL, outTrainingTable = NULL,
                        outTrainingCSV = NULL, plotFileTraining = NULL,
#                        # for analysis
                        dirSamples = NULL,
                        dirSamplesOut = NULL,

                        exitStatusAna = -1,
                        codeValidationInt = 1,
                        rmScanLine = FALSE,

                        leaf_min_size = 1000,
                        leaf_border_size = 1,
                        lesion_min_size = 1,
                        lesion_max_size = 120000,
                        lesion_border_size = 1,
                        lesion_color_border = "blue",
                        lesion_color_bodies = "#FE8E0000",
                        rmEdge = FALSE,
                        rmEccentric = FALSE,
                        lesion_eccentricMin = 0,
                        lesion_eccentricMax = 1,
                        active_blur = FALSE,

                        # both
                        parallelMode = FALSE,
                        parallelThreadsNum = max_no_cores,
                        blur_value = 0,
#                        logfilename = "log.txt",

                        # edit
                        dirInResult = NULL,
                        loadImageEdit = NULL,
                        missing = NULL,
                        plotCexColor = "green",
                        selectedRows = 0
                        )

# function derive from shinyFiles to load Home on linux and home for MACOS
getOwnVolume <- function (exclude=NULL){
  osSystem <- Sys.info()["sysname"]
  if (osSystem == "Darwin") {
    disk <- list.files("/Volumes/", full.names = T)
    names(disk) <- disk
    home <- c(home = "~")
    volumes <- c(home, disk)
    volumes <- home
  }
  else if (osSystem == "Linux") {
    volumes <- c(root = "/")
    home <- c(home = "~")
    media <- list.files("/media", full.names = T)
    names(media) <- media
    volumes <- c(home, media, volumes)
  }
  else if (osSystem == "Windows") {
    volumes <- system("wmic logicaldisk get Caption", intern = T)
    volumes <- sub(" *\\r$", "", volumes)
    keep <- !tolower(volumes) %in% c("caption", "")
    volumes <- volumes[keep]
    volNames <- system("wmic logicaldisk get VolumeName",
                       intern = T)
    volNames <- sub(" *\\r$", "", volNames)
    volNames <- volNames[keep]
    volNames <- paste0(volNames, ifelse(volNames == "",
                                        "", " "))
    volNames <- paste0(volNames, "(", volumes, ")")
    names(volumes) <- volNames
  }
  else {
    stop("unsupported OS")
  }
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% exclude]
  }
  volumes
}

# list of volumes acces to load data
allVolumesAvail <- getOwnVolume()

# return odd number >
returnOdd <- function(value){
  if(value%%2==0){
    return(value+1)
  }else{
    return(value)
  }
}

# no color for hide ploint of plot on edit mode
noColor <- rgb(red = 0, green = 0, blue = 1, alpha = 0)


existDirTraining <- function(dirTraining){
  list(
    dirlimb = dir.exists(paste(dirTraining,"/limb", sep = .Platform$file.sep)),
    dirBackground = dir.exists(paste(dirTraining,"/background", sep = .Platform$file.sep)),
    dirLesion = dir.exists(paste(dirTraining,"/lesion", sep = .Platform$file.sep))
  )
}
