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
    box(title = "LeAFtool     Lesion Area Finding tool", status="success", solidHeader = TRUE, collapsible = FALSE, width = 12,
      fluidRow(column( 12, offset = 0,
      img(src = "LeAFtool-short.png", class = 'img-responsive', style="max-width: 50%;display: block;margin-left: auto;margin-right: auto;")
      )),
      fluidRow(
        column( width = 12, offset = 0,
          withTags(
            div( class = "calibrationHome",
              h2("About the tool"),
              p('This application was created at the UMR BGPI (Unité Mixte de Recherche Biology and Genetic of Plant-Pathogen Interaction - CIRAD, INRA and Montpellier SupAgro - Montpellier, France) in order to analyse lesions on a infected leaf. An automatic measurement of lesions makes it easier to compare the pathogenicity of pathogens, whose signs and symptoms are visible. It makes possible to analyse many images in a faster way.'),
              p('The pathogenic lesions are automatically analysed and different parameters are calculated in pixels so that the number and relative area of lesions can be calculated.')
            )
          )
        )
      ),
      fluidRow(
        column( width = 4, offset = 0,
          withTags(
            div( class = "calibrationHome",
              h2("Calibration"),
              p('Calibration is the first of two steps to analyse lesions present on the leaves. It begins by creating a learning game and then, carrying out a discriminant factorial analysis.'),
              p('Important note: the calibration folder must contain the sub-folder "background", "limb" and "lesion" with these names (case-sensitive).'),
              p('The current version accepts these three categories of pixels, with  no restriction for sub-categories'),
              p('There is no constraint on the name and the number of the calibration image files, the program always reads all .jpg, .jpeg .PNG and .tif files present (not case sensitive for extention).'),
              p('At the end of the learning phase, three files are created in the calibration folder (the first part of the name these files is the name of the folder with the extensions .png (graph of the discriminant analysis), .txt (results of the discriminant analysis), and .RData (file R containing the results of the discriminant analysis, then used by the image analysis procedure).')
            )
          ),
          tags$div(
            tags$p("Input exemple:"),
            actionButton( "showInCalibration", class = "btn-img", img(src = "calibrationFolder.png", width = '60%', height = '60%'))
          ),
          tags$div(
            tags$p("Output exemple:"),
            actionButton( "showCalibration", class = "btn-img", img(src = "learning.jpeg", width = '60%', height = '60%'))
          )
        ),
        column( width = 4, offset = 0,
          withTags(
            div( class = "analysisHome",
              h2("Analysis"),
              p("Analysis parameters:"),
              ul(
                li("Samples folder: folder for storing files to be analyzed"),
                li("Output folder: "),
                li(".RData file: file created during the calibration phase in the calibration folder"),
                li("Blur image: Minimum area of a leaf"),
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
        ),
        column( width = 4, offset = 0,
          withTags(
            div( class = "analysisHome",
              h2("Edit mode"),
              p("Edition availables:"),
              ul(
                li("remove bad lesion detection (juste click)"),
                li("remove multiple bad lesions detection (brush on image)"),
                li("filter all lesions table with many values (for exemple size of lesion, excentricity, ...)"),
                li("zoom on image"),
                li("change the color of lesion on image (only on edit image not all)")
              ),
              p("Note: if you edit the tables outside LeAFtool, they may not be compatible anymore")
            )
          ),
          tags$div(
            tags$p("Exemple:"),
            actionButton( "showEdit", class = "btn-img", img(src = "editMode.png", width = '60%', height = '60%'))
          )
        )
      ),
      fluidRow(
        column( width = 12, offset = 0,
#          box(width = 12,
          withTags(
            footer(align='right',
              p(align='right',
                u("contacts :"),
                " Sébastien RAVEL - ", a(href='mailto:sebastien.ravel@cirad.fr',"sebastien.ravel@cirad.fr")
              ),
              p(align='right',
                a(href='http://umr-bgpi.cirad.fr/', img(style = 'width: 7%;', src='logo-bgpi.png')),
                a(href='https://www.cirad.fr', img(style = 'width:8%;', src='logo-cirad.png'))
              )
            )
          )
        )
      )
    )
  )
)
