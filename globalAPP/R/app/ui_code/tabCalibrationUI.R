#####################################################################################################
#
# Copyright 2018 CIRAD-INRA
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

tabItem(
  # Tab for calibration input/output
  tabName = "tabCalibration",
  fluidRow(
    box(
      title = "Calibration Input", status = "primary",solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(4,
             tags$div(class = "calibrationTXT", "Calibration input directory must include sub-folders:",  tags$br(),
                      tags$ul(
                        tags$li("limb"),
                        tags$li("background"),
                        tags$li("lesion")
                      )
             )
      ),
      column(8,
             shinyDirButton(id = 'dirCalibration', label = 'Select Data Folder', title = 'Please select a folder', FALSE, class = "btn-info"),
             bsPopover(id = "dirCalibration", "Select Input folder", "the input folder must have sub-folders", trigger="hover", options = NULL),
#             conditionalPanel(
#               condition = "output.codeAna == 0", br(),
               verbatimTextOutput("dirCalibration", placeholder = FALSE),
               actionButton("runButton", "run calibration!")
#             )
      )
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "output.codeAna == 0",br(),
      box(
        title = "ERROR", status = "danger",solidHeader = TRUE,
        uiOutput("err"),
        verbatimTextOutput("codeAna")
      )
    )
  ),
  fluidRow(
#    conditionalPanel(
#      condition = "output.codeAna == 1",
      box(
        title = "Calibration output", status = "success",solidHeader = TRUE, width = 12,

        fluidRow(
          column(4,
                 verbatimTextOutput("mess",placeholder = FALSE)
          )
        ),
        fluidRow(
          column(4,
                 plotOutput("img")
          ),
          column(8,
                 tableOutput("table")
          )
        )
      )
#    )
  )
)
