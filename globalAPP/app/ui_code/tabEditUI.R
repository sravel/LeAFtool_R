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
  tabName = "tabEdit",
  # BOX SELECT EDIT
  box( title = "Edit Input", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
    fluidRow( class = "spaceRow",
      column(width = 5,offset = 0,
        shinyDirButton(id = 'dirInResult', label = 'Select Input results folder', title = 'Please select a folder', FALSE, class = "btn-default") %>%
          helper(icon = "question",
                type = "markdown",
                content = "dirInResult"),
        verbatimTextOutput("dirInResult", placeholder = TRUE),
        selectInput("imageEdit", "Sample image:",NULL)
        ),
      column(width = 3,offset = 0,
        checkboxInput("pchOriginal", "Add pch Original", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "pchOriginal"),
        checkboxInput("zoomOriginalCheck", "Zoom Origal Image", value = FALSE, width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "zoomOriginalCheck")
      ),
      column(width = 4, offset = 0,
        fluidRow(
          tags$label("Lesions color Edit")
        ),
        fluidRow( class = "colorRow",
          column(width = 6, offset = 0,
            tags$label("border"),
            colourpicker::colourInput("lesion_color_borderEdit",  label = "", value = "green",
                        palette = c("square", "limited"), allowedCols = NULL,
                        allowTransparent = TRUE, returnName = FALSE, showColour = "background") %>%
              helper(icon = "question",
                      type = "markdown",
                      content = "lesion_color_borderEdit")
            ),
          column(width = 6, offset = 0,
            tags$label("bodies"),
            colourpicker::colourInput("lesion_color_bodiesEdit",  label = "", value = "green",
                        palette = c("square", "limited"), allowedCols = NULL,
                        allowTransparent = TRUE, returnName = FALSE, showColour = "background") %>%
              helper(icon = "question",
                      type = "markdown",
                      content = "lesion_color_bodiesEdit")
          )
        )
      )
    )
  ),
  # BOX EDITION
  box( title = "Edit Selection", status = "success",solidHeader = TRUE, collapsible = FALSE, width = 12,
    fluidRow( class = "spaceRow",
      column(width = 4,offset = 0,
        jqui_resizable(plotOutput("plotcurrentImageOriginalEdit"))
      ),
      column(width = 4,offset = 0,
        jqui_resizable(
          plotOutput("plotcurrentImageEdit",click = "plot_click",
                    dblclick = "plot_dbclick",
                    brush = brushOpts(id = "plot_brush", delay = 100, delayType = "debounce", resetOnNew = TRUE),
                    hover = hoverOpts(id = "plot_hover", delay = 80, delayType = "debounce")
          )
        )
      ),
      column(width = 4,offset = 0,
        jqui_resizable(plotOutput("zoomcurrentImageEdit"))
      )
    ),
    # TABLES
    box( title = "Table all lesion", status = "success",solidHeader = TRUE, collapsible = TRUE, width = 12,
      fluidRow( class = "spaceRow",
        column(width = 12,offset = 0,
          div(style = 'overflow-x: scroll', DT::dataTableOutput("results", width = "100%"))
#          div(style = 'overflow-x: scroll', textOutput("coor"))
        )
      )
    ),
    box( title = "Table merge leaf lesion", status = "success",solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(width = 12,offset = 0,
        div(DT::dataTableOutput("AG", width = "100%"))
      )
    ),
    box( title = "Table all leaves lesion", status = "success",solidHeader = TRUE, collapsible = TRUE, width = 12,
      column(width = 12,offset = 0,
        div(DT::dataTableOutput("MERGE", width = "100%"))
      )
    )
  )
)
