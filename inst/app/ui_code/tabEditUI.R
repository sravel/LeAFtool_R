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
    fluidRow( #class = "spaceRow",
      column(width = 5,offset = 0,
        shinyDirButton(id = 'dirInResult', label = 'Select Input results folder', title = 'Please select a folder', FALSE, class = "btn-default") %>%
          helper(icon = "question",
                type = "markdown",
                content = "dirOut"),
        verbatimTextOutput("dirInResult", placeholder = TRUE),
        selectInput("imageEdit", "Sample image:",NULL)
        ),
      column(width = 2,offset = 0,
        checkboxInput("pchOriginal", "Add lesion number", value = FALSE, width = "150px"), #%>%
#          helper(icon = "question",
#                type = "markdown",
#                content = "pchOriginal"),
        checkboxInput("zoomOriginalCheck", "Zoom Original Image", value = FALSE, width = "150px"), #%>%
#          helper(icon = "question",
#                type = "markdown",
#                content = "zoomOriginalCheck"),
        sliderInput("zoomValueSlider", "Zoom nX", min = 2, max = 9, value = 2, step = 1, width = "150px") #%>%
#            helper(icon = "question",
#                  type = "markdown",
#                  content = "zoomValueSlider")
      ),
      column(width = 2, offset = 0,
        fluidRow(
          tags$label("Lesions color Edit")
        ),
        fluidRow( class = "colorRow",
          column(width = 6, offset = 0,
            tags$label("border"),
            colourpicker::colourInput("lesion_color_borderEdit",  label = "", value = "blue",
                        palette = c("square", "limited"), allowedCols = NULL,
                        allowTransparent = TRUE, returnName = FALSE, showColour = "background") %>%
              helper(icon = "question",
                      type = "markdown",
                      content = "lesion_color_border")
            ),
          column(width = 6, offset = 0,
            tags$label("bodies"),
            colourpicker::colourInput("lesion_color_bodiesEdit",  label = "", value = "#FE8E0000",
                        palette = c("square", "limited"), allowedCols = NULL,
                        allowTransparent = TRUE, returnName = FALSE, showColour = "background") %>%
              helper(icon = "question",
                      type = "markdown",
                      content = "lesion_color_bodies")
          )
        )
      ),
      column(width = 2, offset = 0,
        fluidRow(
          tags$label("Plot adjust")
        ),
        fluidRow( #class = "spaceRow",
          sliderInput("plotCexSize", "Plot cex size", min = 1, max = 4, value = 2, step = 0.5, width = "150px"), #%>%
#            helper(icon = "question",
#                  type = "markdown",
#                  content = "plotCexSize")
          colourpicker::colourInput("plotCexColor",  label = "Plot point color", value = "green",
                        palette = c("limited"), allowedCols = NULL,
                        allowTransparent = FALSE, returnName = FALSE, showColour = "background"),
          checkboxInput("hideRemove", "hide remove point", value = FALSE, width = "150px")
        )
      )
    )
  ),
  # BOX EDITION
  conditionalPanel(
    condition = "input.imageEdit",
    box( title = "Edit Selection", status = "success",solidHeader = TRUE, collapsible = FALSE, width = 12,
      fluidRow( class = "spaceRow",
        column(width = 4,offset = 0,
          jqui_resizable(plotOutput("plotcurrentImageOriginalEdit"))
        ),
        column(width = 4,offset = 0,
          jqui_resizable(
            plotOutput("plotcurrentImageEdit",click = "plot_click",
                      dblclick = "plot_dbclick",
                      brush = brushOpts(id = "plot_brush", delay = 10000, delayType = "debounce", resetOnNew = TRUE),
                      hover = hoverOpts(id = "plot_hover", delay = 400, delayType = "debounce")
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
  #          textOutput("coor"),
            actionButton("removeSelect", strong("Remove Select")),
            actionButton("keepSelect", strong("Keep Select")),
            actionButton("deselectAll", strong("Deselect All")),
            actionButton("removeFilter", strong("Remove filter")),
            actionButton("resetKeep", strong("Keep all")),
            actionButton("removeAll", strong("Remove all"))
#            verbatimTextOutput('info')
          )
        ),
        fluidRow( class = "spaceRow",
          column(width = 12,offset = 0,
            div(style = 'overflow-x: scroll', DT::dataTableOutput("results", width = "100%"))
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
)
