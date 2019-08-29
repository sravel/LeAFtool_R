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
  # Tab for Toolbox input/output
  tabName = "tabToolbox",

  # BOX input
  fluidRow(
    box(
      title = "Resize images", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 6,
      column(12,
        numericInput("factor", "factor to resize:", value = 2, min=1,  width = "150px") %>%
          helper(icon = "question",
                type = "markdown",
                content = "factor_resize"),

          shinyDirButton(id = 'dirInResize', label = 'Select images folder', title = 'Please select a folder', FALSE, class = "btn-default", icon = icon("compress")) %>%
          helper(icon = "question",
              type = "markdown",
              content = "dirInSamples"),
        verbatimTextOutput("dirResize", placeholder = TRUE),
        "Resize path output:",
        verbatimTextOutput("resizePathOut")
      )
    ),
    box(
      title = "Split images", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 6,
      column(12,
        fluidRow(
            column(3,
              numericInput("splitVertical", "Split Vertical:", value = 1, min=1,  width = "150px") %>%
                helper(icon = "question",
                      type = "markdown",
                      content = "splithelp")
            ),
            column(3,
              numericInput("splitHorizontal", "Split Horizontal:", value = 1, min=1,  width = "150px") %>%
                helper(icon = "question",
                      type = "markdown",
                      content = "splithelp")
            ),
            column(3,
              checkboxInput("outputNumber", "Order to numerote 'left to right'", value = FALSE, width = "150px") %>%
              helper(icon = "question",
                    type = "markdown",
                    content = "outputNumber")
            )
          ),
          shinyDirButton(id = 'dirInSplit', label = 'Select images folder', title = 'Please select a folder', FALSE, class = "btn-default", icon = icon("cut")) %>%
            helper(icon = "question",
                type = "markdown",
                content = "dirInSamples"),
          verbatimTextOutput("dirSplit", placeholder = TRUE),
          "Split path output:",
          verbatimTextOutput("splitPathOut")
      )
    )
  )
#  ),
#  fluidRow(
#    box(
#      title = "Upload data", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
#      column(6,

#        fileInput("file", "Upload Zip file", accept = ".zip"),
#        # action button to unzip the file
#        actionButton("unzip", "Unzip Files", icon = icon("file-archive")),

#        # to display the metadata of the zipped file
#        tableOutput("filedf"),

#        # to display the list of unzipped files
#        tableOutput("unzipped")
#      )
#    )
#  )
)
