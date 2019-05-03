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
  tabName = "tabAnalysis",

  # BOX for input
  fluidRow(
    box(
      title = "Analysis Input", status = "success",solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(width = 5,
      # DIRECTORIES Input
        fluidRow( class = "spaceRow",
          shinyDirButton(id = 'dirInSamples', label = 'Select images folder', title = 'Please select a folder', FALSE, class = "btn-default", icon = icon("leaf")) %>%
            helper(icon = "question",
                type = "markdown",
                content = "dirInSamples"),
          verbatimTextOutput("dirSamples", placeholder = TRUE)
        ),
        fluidRow( class = "spaceRow",
          shinyDirButton(id = 'dirOut', label = 'Select output results folder', title = 'Please select a folder', FALSE, class = "btn-default", icon = icon("sign-out-alt")) %>%
            helper(icon = "question",
                type = "markdown",
                content = "dirOut"),
          verbatimTextOutput("dirOutAnalysis", placeholder = TRUE)
        ),
        fluidRow( class = "spaceRow",
          shinyFilesButton('fileRDataIn', label='Load calibration file', title='Please select Rdata file', multiple=T, class = "btn-default", icon = icon("balance-scale")) %>%
            helper(icon = "question",
                type = "markdown",
                content = "fileRDataIn"),
          verbatimTextOutput('fileRData', placeholder = TRUE)
        ),
        fluidRow( class = "spaceRow",
          conditionalPanel(
            condition = "output.dirSamples && output.dirOutAnalysis && output.fileRData && output.codeValidationInt == 1",
            actionButton("runButtonAnalysis", "Run Analysis", icon = icon("play-circle "))
#            actionButton("stopButtonAnalysis",  label = "Stop Analysis", icon = icon("dashboard"))
#            verbatimTextOutput('log', placeholder = FALSE)
          )
        )
      ),
      column(width = 2,
        ### Image parameters
        h4("Image parameters:"),
        checkboxInput("active_blur", "Apply blur on image", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "blur_value"),
        conditionalPanel(
          condition = "input.active_blur",
            numericInput("blur_value", "Blur image:", value = 1 , min = 1, max = 21, step = 2, width = "150px")
        ),
        checkboxInput("outputPositionBottum", "Both images position bottum", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "outputPositionBottum"),
#        checkboxInput("rmScanLine", "Remove Scan line", value = FALSE, width = "150px") %>%
#          helper(icon = "question",
#                type = "markdown",
#                content = "rmScanLine"),
        ### Leaf parameters
        h4("Leaf parameters:"),
          numericInput("leaf_min_size", "Minimum leaf size:", value = 1000, min=1,  width = "150px") %>%
            helper(icon = "question",
                  type = "markdown",
                  content = "leaf_min_size"),
          numericInput("leaf_border_size", "Leaf border size:", value = 5, min=1, step = 2, width = "150px") %>%
            helper(icon = "question",
                  type = "markdown",
                  content = "leaf_border_size")
      ),
      column(width = 2,offset = 0,
      ### Lesion parameters
        h4("Lesions parameters:"),
        checkboxInput("rmEdge", "Remove edge", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "rmEdge"),
        checkboxInput("rmEccentric", "Remove eccentric", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "lesion_eccentric"),
        conditionalPanel(
          condition = "input.rmEccentric",
            sliderInput("lesion_eccentric_slider", label = "Eccentric size", min = 0, max = 1, value = c(0, 1), step = 0.01, width = "150px")
        ),
        numericInput("lesion_min_size", "Lesion min size:", value = 10, min=1, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "lesion_min_size"),
        numericInput("lesion_max_size", "Lesion max size:", value = 120000, min=1, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                 content = "lesion_max_size"),

        numericInput("lesion_border_size", "Lesion border size:", value = 3, min=1, step = 2, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "lesion_border_size"),
        column(width = 12, offset = 0,
          fluidRow(
            tags$label("Lesions color")
          ),
          fluidRow( class = "colorRow",
            column(width = 6, offset = 0,
              tags$label("border"),
              colourInput("lesion_color_border",  label = "", value = "blue",
                          palette = c("square", "limited"), allowedCols = NULL,
                          allowTransparent = TRUE, returnName = FALSE, showColour = "background") %>%
                helper(icon = "question",
                      type = "markdown",
                      content = "lesion_color_border")
            ),
            column(width = 6, offset = 0,
              tags$label("bodies"),
              colourInput("lesion_color_bodies",  label = "", value = "#FE8E0000",
                          palette = c("square", "limited"), allowedCols = NULL,
                          allowTransparent = TRUE, returnName = FALSE, showColour = "background") %>%
                helper(icon = "question",
                      type = "markdown",
                      content = "lesion_color_bodies")
            )
          )
        )
      ),
      column(width = 2, offset = 0,
      ### running parameters
        h4("Running Mode"),
        checkboxInput("parallelMode", "Active Parallel mode", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "parallelMode"),
        conditionalPanel(
          condition = "input.parallelMode",
            numericInput("parallelThreadsNum","Number of Threads", value = max_no_cores, min = 1, max = max_no_cores, width = "150px")  %>%
              helper(icon = "question",
                    type = "markdown",
                    content = "parallelThreadsNum")
        )
      )
    )
  ),
  # BOX ERROR
#  fluidRow(
#    conditionalPanel(
#      condition = 'output.codeValidationInt==0',
#      box(
#        title = "Warning", status = "warning",solidHeader = TRUE,
#        uiOutput("warning")
#      )
#    )
#  ),
  # BOX RESULT
  fluidRow(
    conditionalPanel(
      condition = "output.analysisFinish==1",
      box(
        title = "Analysis output", status = "success",solidHeader = TRUE, width = 12,
        #verbatimTextOutput("analysisFinish",placeholder = FALSE),
        DT::dataTableOutput("contents")
      )
    )
  )
)
