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
  # Tab for Training input/output
  tabName = "tabTraining",

  # BOX input
  fluidRow(
    box(
      title = "Training Input", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(6,
        withTags(
        div(class = "TrainingTXT",
          p('There is no constraint on the name and the number of the Training image files, the program always reads all .jpg, .jpeg .PNG and .tif files present (not case sensitive for extention).'),
          p('Training input folder must include sub-folders:'),
            ul(
              li("limb"),
              li("background"),
              li("lesion")
            )
          )
        )
      ),
      column(3,
        fluidRow( class = "spaceRow",
          shinyDirButton(id = 'dirTraining', label = 'Select Data Folder', title = 'Please select a folder', FALSE, class = "btn-success") %>%
            helper(icon = "question",
                  type = "markdown",
                  content = "dirTraining")
        ),
        fluidRow(
          verbatimTextOutput("dirTraining", placeholder = FALSE)
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
        title = "Training output", status = "success",solidHeader = TRUE, width = 12,
        fluidRow( class = "spaceRow",
          column(8,
                tags$div(class = "infoDiv", "File Rdata build and mandatory for analysis images:",
#                  verbatimTextOutput("plotALL",placeholder = FALSE),
                  verbatimTextOutput("mess",placeholder = FALSE)
                )
          )
        ),
        fluidRow( class = "spaceRow",
          column(12,
#            tags$div(class = "infoDiv","input class VS predict class:",
                 DT::dataTableOutput("table2", width = "50%")
#            )
          )
        ),
        fluidRow( class = "spaceRow",
          column(12,
            tags$div(class = "infoDiv","ACP build with pixel Training:",
                 plotOutput("img1_2", click = "img1_2_zoom_cal", width = "100%", height = "100%")
            )
          )
        ),
        conditionalPanel(
          condition = "output.plotALL == 1",
          fluidRow( class = "spaceRow",
            column(12,
                   plotOutput("img1_3", click = "img1_3_zoom_cal", width = "100%", height = "100%")
            )
          ),
          fluidRow( class = "spaceRow",
            column(12,
                   plotOutput("img2_3", click = "img2_3_zoom_cal", width = "100%", height = "100%")
            )
          )
#          fluidRow( class = "spaceRow",
#            column(8,
#                    plotlyOutput("plotly", width = "100%", height = "100%")
#            )
#          )
        )
      )
    )
  )
)
