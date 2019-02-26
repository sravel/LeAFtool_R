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

tabItem(
  # Tab for calibration input/output
  tabName = "tabCalibration",

  # BOX input
  fluidRow(
    box(
      title = "Calibration Input", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 6,
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
        fluidRow( class = "spaceRow",
          shinyDirButton(id = 'dirCalibration', label = 'Select Data Folder', title = 'Please select a folder', FALSE, class = "btn-default") %>%
            helper(icon = "question",
                  type = "markdown",
                  content = "dirCalibration")
        ),
        fluidRow(
          verbatimTextOutput("dirCalibration", placeholder = FALSE)
        )
      )
    )
  ),
  # BOX ERROR
  fluidRow(
    conditionalPanel(
      condition = "output.codeAna == 0",br(),
      box(
        title = "ERROR", status = "danger",solidHeader = TRUE,
        uiOutput("err")
      )
    )
  ),
  # BOX RESULT
  fluidRow(
    conditionalPanel(
      condition = "output.codeAna == 1",
      box(
        title = "Calibration output", status = "success",solidHeader = TRUE, width = 12,

        fluidRow( class = "spaceRow",
          column(8,
                tags$div(class = "infoDiv", "File Rdata build and mandatory for analysis images:",
                 verbatimTextOutput("mess",placeholder = FALSE)
                )
          )
        ),
        fluidRow( class = "spaceRow",
          column(12,
            tags$div(class = "infoDiv","ACP build with pixel calibration:",
                 plotOutput("img1_2", click = "img1_2_zoom_cal", width = "100%", height = "100%")
            )
          )
        ),
        fluidRow( class = "spaceRow",
          column(12,
            tags$div(class = "infoDiv","ACP build with pixel calibration:",
                 plotOutput("img1_3", click = "img1_3_zoom_cal", width = "100%", height = "100%")
            )
          )
        ),
        fluidRow( class = "spaceRow",
          column(12,
            tags$div(class = "infoDiv","ACP build with pixel calibration:",
                 plotOutput("img2_3", click = "img2_3_zoom_cal", width = "100%", height = "100%")
            )
          )
        ),
        fluidRow( class = "spaceRow",
          column(12,
            tags$div(class = "infoDiv","Table ...?:",
                 tableOutput("table")
            )
          )
        )
      )
    )
  )
)
