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
  tabName = "tabAnalysis",
  fluidRow(
    box(
      title = "Analysis Input", status = "primary",solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(width = 5,
             fluidRow( class = "spaceRow",
               shinyDirButton(id = 'dirInSamples', label = 'Select images folder', title = 'Please select a folder', FALSE, class = "btn-info"),
               verbatimTextOutput("dirSamples", placeholder = TRUE)

             ),
             fluidRow( class = "spaceRow",
               shinyDirButton(id = 'dirOut', label = 'Select output results folder', title = 'Please select a folder', FALSE, class = "btn-info"),
               verbatimTextOutput("dirOutAnalysis", placeholder = TRUE)
             ),
             fluidRow( class = "spaceRow",
               shinyFilesButton('files', label='Load Rdata build in Calibration', title='Please select Rdata file', multiple=T, class = "btn-info"),
               verbatimTextOutput('fileRData', placeholder = TRUE),

#               conditionalPanel(
#                 condition = "input.dirSamples && input.dirOut && input.fileRData && output.codeValidationInt == 1", br(),
                 actionButton("runButtonAnalysis", "Run Analysis!")
#               )
             )
      ),
      column(width = 2,
             h4("Leaf parameters:"),
             numericInput("leaf_min_size", "Leaf min size:", value = 1000, min=1,  width = "150px"),
             numericInput("leaf_border_size", "Leaf border size:", value = 3, min=1, width = "150px")
             # verbatimTextOutput("value")

      ),
      column(width = 2,offset = 0,
             h4("Lesion parameters:"),
             numericInput("lesion_min_size", "Lesion min size:", value = 10, min=1, width = "150px"),
             numericInput("lesion_max_size", "Lesion max size:", value = 1000, min=1000, width = "150px"),
             numericInput("lesion_border_size", "Lesion border size:", value = 3, min=1, width = "150px"),
#             selectInput("lesion_color_boundaries", label = p("Lesions color boundaries"),
#                         choices = colors(),
#                         selected = 0, width = "100px"),
#             selectInput("lesion_color_bodies", label = p("Lesions color bodies"),
#                         choices = colors(),
#                         selected = 0, width = "100px"),
            colourInput("lesion_color_boundaries",  label = p("Lesions color boundaries"), value = "green",
                        palette = "limited", allowedCols = NULL,
                        allowTransparent = FALSE, returnName = TRUE),

            colourInput("lesion_color_bodies",  label = p("Lesions color bodies"), value = "red",
                        palette = "limited", allowedCols = NULL,
                        allowTransparent = FALSE, returnName = TRUE)
      )
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = 'output.codeValidationInt==0',
      box(
        title = "Warning", status = "warning",solidHeader = TRUE,
        uiOutput("warning")
      )
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "output.analysisFinish==1",
      box(
        title = "Analysis output", status = "success",solidHeader = TRUE, width = 12,
        verbatimTextOutput("analysisFinish",placeholder = FALSE),
        DT::dataTableOutput("contents")
      )
    )
  )
)
