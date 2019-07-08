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



#set_wd <- function() {
#  library(rstudioapi) # make sure you have it installed
#  current_path <- getActiveDocumentContext()$path
#  setwd(dirname(current_path ))
#  currentFilePath <<-getwd()
#}
#set_wd()


############################################
## Shiny dashboard start
############################################

# add header
#header <- dashboardHeader(title = "ALAMA: Automatic Lesion Analysis Measure Area", titleWidth = 500
header <- dashboardHeader(title = img(src = "LeAFtool-long.png", class = 'img-responsive'), titleWidth = 400)

# Sidebar for acces to tab
sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Home", tabName = "tabHome", icon = icon("home")),
    menuItem("Training", tabName = "tabTraining", icon = icon("balance-scale")),
    menuItem("Analysis", tabName = "tabAnalysis", icon = icon("pagelines")),
    menuItem("Edit", tabName = "tabEdit", icon = icon("edit")),
#    menuItem("Debug", tabName = "tabDebug", icon = icon("dashboard")),
    menuItem("LOG", tabName = "tabLOG", icon = icon("file-code"))
  )
  # actionButton('close', "Close", class = "btn btn-danger",onclick = "setTimeout(function(){window.close();},500);")
)

# Boby page
body <- dashboardBody(
  includeCSS('www/stylesLesion.css'),
  useShinyjs(),
  useShinyFeedback(),
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

    # add tab for Training
    source(file.path("ui_code", "tabTrainingUI.R"), local = TRUE, chdir = TRUE)$value,

    # add tab for analysis
    source(file.path("ui_code", "tabAnalysisUI.R"), local = TRUE, chdir = TRUE)$value,

    # add tab for edit
    source(file.path("ui_code", "tabEditUI.R"), local = TRUE, chdir = TRUE)$value,

#    # other tab
#    tabItem(
#      tabName = "tabDebug",
#      h1("DEBUG"),
#      verbatimTextOutput("debug")
#    ),
    # other tab
    tabItem(
      tabName = "tabLOG",
      h1("LOG"),
      fluidRow(
        column(1,
              selectInput("level", label = "Level", choices = "INFO", selected = "INFO"),
              selectInput("thread", label = "Thread", choices = "1"),
              selectInput("package", label = "Package", choices = "packages")
              ),
        column(11,
              verbatimTextOutput("logFilePath"),
              dataTableOutput("logTable")
              )
      )
    )
  )
)

# run UI
shinyUI(
  dashboardPage(title="LeAFtool", skin = "yellow", header, sidebar, body)
)
