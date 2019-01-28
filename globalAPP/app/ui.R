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
#If not see <http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.txt>
#
# Intellectual property belongs to CIRAD and South Green developpement plateform
# Version 0.1.0 written by Sebastien RAVEL, François BONNOT, Sajid ALI, FOURNIER Elisabeth
#####################################################################################################

# list of packages required
list.of.packages <- c("shiny","shinythemes","shinydashboard","shinyFiles","shinyBS","shinyjs", "DT","EBImage","MASS","lattice",
                      "foreach","future","doParallel","shinyFeedback","colourpicker","shinyhelper", "shinyjqui", "rstudioapi")


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
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyFiles)
library(shinyBS)
library(DT)
library(shinyjs)
library(shinyFeedback)
library(shinyhelper)
library(colourpicker)
library(shinyjqui)

# calibration
library(EBImage)
library(lattice)
library(MASS)

# analysis
library(foreach)
library(future)
#library(doSNOW)
library(doParallel)

set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path
  setwd(dirname(current_path ))
  currentFilePath <<-getwd()
}
set_wd()

# create log file
logfilename <<- paste0(currentFilePath,"/debug.txt")
# remove previous log (not working if multiple instance on same path)
unlink(logfilename)

############################################
## Shiny dashboard start
############################################

# ### creating custom logo object
#logo_blue_gradient <- shinyDashboardLogoDIY(

#   badgeText = "alpha"
#  ,boldText = "ALAMA"
#  ,mainText = "Automatic Lesion Analysis Measure Area"
#  ,textSize = 16
#  ,badgeTextColor = "white"
#  ,badgeTextSize = 3
#  ,badgeBackColor = "#00E574"
#  ,badgeBorderRadius = 3

#)

#### creating custom theme object
#theme_blue_gradient <- shinyDashboardThemeDIY(

#  ### general
#  appFontFamily = "Arial"
#  ,appFontColor = "rgb(0,0,0)"
#  ,bodyBackColor = "rgb(248,248,248)"

#  ### header
#  ,logoBackColor = "#007F40"

##  ,headerButtonBackColor = "rgb(238,238,238)"
#  ,headerButtonBackColor = "rgb(0,127,64)"
#  ,headerButtonIconColor = "rgb(238,238,238)"
#  ,headerButtonBackColorHover = "rgb(210,210,210)"
#  ,headerButtonIconColorHover = "rgb(0,0,0)"

##  ,headerBackColor = "rgb(238,238,238)"
#  ,headerBackColor = "rgb(0,127,64)"
#  ,headerBoxShadowColor = "#aaaaaa"
#  ,headerBoxShadowSize = "0px 0px 0px"

#  ### sidebar
#  ,sidebarBackColor = cssGradientThreeColors(
#    direction = "down"
#    ,colorStart = "rgb(0,127,64)"
#    ,colorMiddle = "rgb(0,255,129)"
#    ,colorEnd = "rgb(0,64,32)"
#    ,colorStartPos = 0
#    ,colorMiddlePos = 60
#    ,colorEndPos = 100
#  )
#  ,sidebarPadding = 0
#  ,sidebarMenuBackColor = "transparent"
#  ,sidebarMenuPadding = 0
#  ,sidebarMenuBorderRadius = 0

#  ,sidebarShadowRadius = "3px 5px 5px"
#  ,sidebarShadowColor = "#aaaaaa"

#  ,sidebarUserTextColor = "rgb(0,127,64)"

#  ,sidebarSearchBackColor = "rgb(0,127,64)"
#  ,sidebarSearchIconColor = "rgb(153,153,153)"
#  ,sidebarSearchBorderColor = "rgb(0,127,64)"

#  ,sidebarTabTextColor = "rgb(255,255,255)"
#  ,sidebarTabTextSize = 13
#  ,sidebarTabBorderStyle = "none none solid none"
#  ,sidebarTabBorderColor = "rgb(35,106,135)"
#  ,sidebarTabBorderWidth = 1

#  ,sidebarTabBackColorSelected = cssGradientThreeColors(
#    direction = "down"
#    ,colorStart = "#00E574"
#    ,colorMiddle = "rgb(0,229,116)"
#    ,colorEnd = "rgb(0,127,64)"
#    ,colorStartPos = 0
#    ,colorMiddlePos = 60
#    ,colorEndPos = 100
#  )
#  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
#  ,sidebarTabRadiusSelected = "10px 10px 10px 10px"

#  ,sidebarTabBackColorHover = cssGradientThreeColors(
#    direction = "down"
#    ,colorStart = "rgb(0,209,106)"
#    ,colorMiddle = "rgb(0,229,116)"
#    ,colorEnd = "rgb(0,127,64)"
#    ,colorStartPos = 0
#    ,colorMiddlePos = 60
#    ,colorEndPos = 100
#  )
#  ,sidebarTabTextColorHover = "rgb(50,50,50)"
#  ,sidebarTabBorderStyleHover = "none none solid none"
#  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
#  ,sidebarTabBorderWidthHover = 1
#  ,sidebarTabRadiusHover = "10px 10px 10px 10px"

#  ### boxes
#  ,boxBackColor = "rgb(255,255,255)"
#  ,boxBorderRadius = 5
#  ,boxShadowSize = "0px 1px 1px"
#  ,boxShadowColor = "rgba(0,0,0,.1)"
#  ,boxTitleSize = 16
#  ,boxDefaultColor = "rgb(210,214,220)"
#  ,boxPrimaryColor = "rgba(44,222,235,1)"
#  ,boxSuccessColor = "rgba(0,255,213,1)"
#  ,boxWarningColor = "rgb(244,156,104)"
#  ,boxDangerColor = "rgb(255,88,55)"

#  ,tabBoxTabColor = "rgb(255,255,255)"
#  ,tabBoxTabTextSize = 14
#  ,tabBoxTabTextColor = "rgb(0,0,0)"
#  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
#  ,tabBoxBackColor = "rgb(255,255,255)"
#  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
#  ,tabBoxBorderRadius = 5

#  ### inputs
##  ,buttonBackColor = "rgb(245,245,245)"
#  ,buttonBackColor = cssGradientThreeColors(
#    direction = "down"
#    ,colorStart = "rgb(0,229,116)"
#    ,colorMiddle = "rgb(0,229,116)"
#    ,colorEnd = "rgb(0,64,32)"
#    ,colorStartPos = 0
#    ,colorMiddlePos = 80
#    ,colorEndPos = 100
#  )
#  ,buttonTextColor = "rgb(0,0,0)"
#  ,buttonBorderColor = "rgb(200,200,200)"
#  ,buttonBorderRadius = 8

##  ,buttonBackColorHover = "rgb(235,235,235)"
#  ,buttonBackColorHover = cssGradientThreeColors(
#    direction = "down"
#    ,colorStart = "rgba(44,222,235,1)"
#    ,colorMiddle = "rgba(44,222,235,1)"
#    ,colorEnd = "rgba(0,255,213,1)"
#    ,colorStartPos = 0
#    ,colorMiddlePos = 60
#    ,colorEndPos = 100
#  )
#  ,buttonTextColorHover = "rgb(100,100,100)"
#  ,buttonBorderColorHover = "rgb(200,200,200)"

#  ,textboxBackColor = "rgb(255,255,255)"
#  ,textboxBorderColor = "rgb(200,200,200)"
#  ,textboxBorderRadius = 5
#  ,textboxBackColorSelect = "rgb(245,245,245)"
#  ,textboxBorderColorSelect = "rgb(200,200,200)"

#  ### tables
#  ,tableBackColor = "rgb(255,255,255)"
#  ,tableBorderColor = "rgb(240,240,240)"
#  ,tableBorderTopSize = 1
#  ,tableBorderRowSize = 1

#)

# add header
#header <- dashboardHeader(title = "ALAMA: Automatic Lesion Analysis Measure Area", titleWidth = 500)
#header <- dashboardHeader(title = logo_blue_gradient, titleWidth = 500)
header <- dashboardHeader(title = img(src = "logolong2.png", class = 'img-responsive'), titleWidth = 400)

# Sidebar for acces to tab
sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Home", tabName = "tabHome", icon = icon("home")),
    menuItem("Calibration", tabName = "tabCalibration", icon = icon("balance-scale")),
    menuItem("Analysis", tabName = "tabAnalysis", icon = icon("pagelines")),
    menuItem("Edit", tabName = "tabEdit", icon = icon("edit")),
    menuItem("Debug", tabName = "tabDebug", icon = icon("dashboard"))
  )
  # actionButton('close', "Close", class = "btn btn-danger",onclick = "setTimeout(function(){window.close();},500);")
)

# Boby page
body <- dashboardBody(
  includeCSS('www/stylesLesion.css'),
  useShinyjs(),
  useShinyFeedback(),
  ## Theme already build:
   ### changing theme
#  shinyDashboardThemes(
#    theme = "poor_mans_flatly"
#  ),
  ## Own theme
#  theme_blue_gradient,

  # Loading message
  hidden(
    tags$div(
      id = "loading-content",
      tags$h2("Loading...")
    )
  ),

  tabItems(
    # add tab for Home
    source(file.path("ui_code", "tabHomeUI.R"), local = TRUE, chdir = TRUE)$value,

    # add tab for calibration
    source(file.path("ui_code", "tabCalibrationUI.R"), local = TRUE, chdir = TRUE)$value,

    # add tab for analysis
    source(file.path("ui_code", "tabAnalysisUI.R"), local = TRUE, chdir = TRUE)$value,

    # add tab for edit
    source(file.path("ui_code", "tabEditUI.R"), local = TRUE, chdir = TRUE)$value,

    # other tab
    tabItem(
      tabName = "tabDebug",
      h1("DEBUG"),
      verbatimTextOutput("debug"),
#      conditionalPanel(
#        condition = 'input.runButtonAnalysis',
#        box(
#          title = "LOG", status = "info",solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
#          actionButton('actu', "actualize", class = "btn ")
#          verbatimTextOutput('log', placeholder = TRUE)
#        )
#      ),
      shinythemes::themeSelector()  # <--- Add this somewhere in the UI
    )
  )
)

# run UI
shinyUI(
  dashboardPage(title="ALAMA", skin = "yellow", header, sidebar, body)
)
