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

tabItem(# Tab for Home input/output
  tabName = "tabHome",
  fluidRow(
    box(title = "ALAMA 	  Automatic Lesion Analysis Measure Area manual",status="success", solidHeader = TRUE, collapsible = FALSE, width = 12,
      fluidRow(column( 12, offset = 0,
      img(src = "logo.png", class = 'img-responsive')
#        withTags(
#          div( class = "home",
##            "http://acronymcreator.net/ace.py " ,br(),
##            "LETAL      Lesion Estimate Tools Area Leaf" ,br(),
##            "         LEThAL 	  Lesion Estimate Tools Area Leaf" ,br(),
##            "         TOMALES 	  TOols Measure Area LESion" ,br(),
##            "         COLLATe 	  COunt Leaf Lesion Area Tools" ,br(),
##            "         TALLIM 	  Tools Area Lesion LImb Measure" ,br(),
##            "         CALLIsTO 	  Count Area Lesion LImb TOols" ,br(),
##            "         MATiLLA 	  MeAsure Tools Leaf Lesion Area" ,br(),
##            "         SkILLET 	  SIze Limb Lesion Estimate Tools" ,br(),
##            "         SLALOM 	  Size LeAf LesiOn Measure" ,br(),
##            "         CLARET 	  Count Lesion AREa Tools" ,br(),
##            "         ALECTO 	  Area LEsion Count TOols" ,br(),
##            "         ALAMA 	  Automatic Lesion Analysis Measure Area" ,br(),

#            "         The analysis procedure consists of 2 steps:",
#            br(),
#            ul(
#              li("Calibration"),
#              li("Analysis")
#            )
#          )
#        )
      )),
      fluidRow(
        column( width = 6, offset = 0,
          withTags(
            div( class = "calibrationHome",
              h2("Calibration"),
              p('Important note: the calibration directory must contain the sub-directory "background", "limb" and "lesion" with these names (case-sensitive).'),
              p('The current version accepts these three categories of pixels, but the calibration directory can contain more than three subdirectories since the name of the useful subdirectories is specified.'),
              p('There is no constraint on the name and the number of the calibration image files (the program always reads all the files present in the useful subdirectories).'),
              p('At the end of the learning phase, three files are created in the calibration directory (the first part of the name these files is the name of the directory with the extensions .png (graph of the discriminant analysis), .txt (results of the discriminant analysis), and .RData (file R containing the results of the discriminant analysis, then used by the image analysis procedure).')
            )
          ),
          tags$div(
            tags$p("Output exemple:"),
            actionButton( "showCalibration", class = "btn-img", img(src = "learning.jpeg", width = '60%', height = '60%'))
          )
        ),
        column( width = 6, offset = 0,
          withTags(
            div( class = "analysisHome",
              h2("Analysis"),
              p("Analysis parameters:"),
              ul(
                li("Samples directory: directory for storing files to be analyzed"),
                li("Output directory: "),
                li(".RData file: file created during the calibration phase in the calibration directory"),
                li("Leaf min size: Minimum area of a leaf"),
                li("Leaf border size: leaf edge thickness to be removed"),
                li("Lesion min size: minimum area of a lesion"),
                li("Lesion border size: thickness of lesion border to dilate / erode"),
                li("Lesion color output: color of lesions in the analyzed image (0 = black, 1 = white)")
              ),
              p("There is no constraint on the name and the number of the sample images files."),
              p("Note: Surfaces and thicknesses are given in pixels.")
            )
          ),
          tags$div(
            tags$p("Output exemple:"),
            actionButton( "show", class = "btn-img", img(src = "exemple.jpeg", width = '60%', height = '60%'))
          )
        )
      )
    )
  )
)
