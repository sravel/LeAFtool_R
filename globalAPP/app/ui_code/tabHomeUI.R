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

tabItem(# Tab for Home input/output
  tabName = "tabHome",
  fluidRow(
    box(title = "Count Lesion tools manual", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12,
      fluidRow(column( 4, offset = 0,
        tags$div( class = "home",
          "The analysis procedure consists of 2 steps::",
          tags$br(),
          tags$ul(tags$li("Calibration"),
                  tags$li("Analysis"))
        )
      )),
      fluidRow( column( width = 6, offset = 0,
          tags$div(
            class = "calibrationHome",
            tags$h2("Calibration"),
            tags$p('Important note: the calibration directory must contain the sub-directory "background", "limb" and "lesion" with these names (case-sensitive).'),
            tags$p('The current version accepts these three categories of pixels, but the calibration directory can contain more than three subdirectories since the name of the useful subdirectories is specified.'),
            tags$p('There is no constraint on the name and the number of the calibration image files (the program always reads all the files present in the useful subdirectories).'),
            tags$p('At the end of the learning phase, three files are created in the calibration directory (the first part of the name these files is the name of the directory with the extensions .png (graph of the discriminant analysis), .txt (results of the discriminant analysis), and .RData (file R containing the results of the discriminant analysis, then used by the image analysis procedure).')
          ),
          tags$div(
            tags$p("Output exemple:"),
            actionButton( "showCalibration", class = "btn-img", img(src = "calibration.png", width = '60%', height = '60%'))
          )
      ),
        column( width = 6, offset = 0,
          tags$div( class = "analysisHome",
            tags$h2("Analysis"),
            tags$p('Analysis parameters:'),
            tags$ul(
              tags$li("Samples directory: directory for storing files to be analyzed"),
              tags$li("Output directory: "),
              tags$li(".RData file: file created during the calibration phase in the calibration directory"),
              tags$li("Leaf min size: Minimum area of a leaf"),
              tags$li("Leaf border size: leaf edge thickness to be removed"),
              tags$li("Lesion min size: minimum area of a lesion"),
              tags$li("Lesion border size: thickness of lesion border to dilate / erode"),
              tags$li("Lesion color output: color of lesions in the analyzed image (0 = black, 1 = white)")
            ),
            tags$p(
              'There is no constraint on the name and the number of the sample images files.'
            ),
            tags$p('Note: Surfaces and thicknesses are given in pixels.')
          ),
          tags$div(
            tags$p("Output exemple:"),
            # tags$img(paste0("exemple.jpeg"))
            actionButton( "show", class = "btn-img", img(src = "exemple.jpeg", width = '60%', height = '60%'))
          )
        )
      )
    )
  )
)
