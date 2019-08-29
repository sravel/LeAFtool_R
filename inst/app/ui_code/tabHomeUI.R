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
    box(collapsible = FALSE, width = 12,
      fluidRow(column( 12, offset = 0,
      img(src = "LeAFtool-short.png", class = 'img-responsive', style="max-width: 50%;display: block;margin-left: auto;margin-right: auto;")
      )),
      fluidRow(
        column( width = 12, offset = 0,
          withTags(
            div( class = "TrainingHome",
              h2("About the tool"),
              p('This application was created at the UMR BGPI (Unité Mixte de Recherche Biology and Genetic of Plant-Pathogen Interaction - CIRAD, INRA and Montpellier SupAgro - Montpellier, France) in order to analyse lesions on a infected leaf. An automatic measurement of lesions makes it easier to compare the pathogenicity of pathogens, whose signs and symptoms are visible. It makes possible to analyse many images in a faster way.'),
              p('The pathogenic lesions are automatically analysed and different parameters are calculated in pixels so that the number and relative area of lesions can be calculated.')
            )
          )
        )
      ),
      fluidRow(
        box( width = 6, offset = 0, title = "Training", status = "success", solidHeader = FALSE, collapsible = TRUE,
          withTags(
            div( class = "TrainingHome",
              p('Training is the first of two steps to analyse lesions present on the leaves. It begins by creating a training game and then, carrying out a discriminant factorial analysis.'),
              p('Important note: the Training folder must contain the sub-folder "background", "limb" and "lesion" with these names (case-sensitive).'),
              p('The current version accepts these three categories of pixels, with  no restriction for sub-categories'),
              p('There is no constraint on the name and the number of the Training image files, the program always reads all .jpg, .jpeg .PNG and .tif files present (not case sensitive for extention).')
            )
          ),
          tags$div( class = "TrainingHome",
            tags$p("Input exemple:"),
            actionButton( "showInTraining", class = "btn-img", img(src = "trainingFolder.png", width = '90%'))
          ),
          tags$div( class = "TrainingHome",
            tags$p("Output exemple:"),
            actionButton( "showTraining", class = "btn-img", img(src = "training.jpeg", width = '70%'))
          )
        ),
        box( width = 6, offset = 0, title = "Analysis", status = "success", solidHeader = FALSE, collapsible = TRUE,
          withTags(
            div( class = "analysisHome",
              p("Analysis parameters:"),
              HTML("
                <table class='display table table-condensed dataTable no-footer'>
                <thead>
                <tr>
                  <th>Name</th>
                  <th>function</th>
                </tr>
                </thead>
                <tbody>
                  <tr>
                    <td>pathTraining</td>
                    <td>The path of the directory containing the sampled images used for training. After the training step, this directory contains the parameters of the training set.</td>
                  </tr>
                  <tr>
                    <td>pathResult</td>
                    <td>The path of the directory where to store the result files.</td>
                  </tr>
                  <tr>
                    <td>pathImages</td>
                    <td>The path of the directory containing the images to analyse.</td>
                  </tr>
                  <tr>
                    <td>fileImage</td>
                    <td>The files names of the images to analyse in pathImages Default: NULL.</td>
                  </tr>
                  <tr>
                    <td>leafAreaMin</td>
                    <td>The minimum area of a leaf (in pixels) Default: 1000.</td>
                  </tr>
                  <tr>
                    <td>leafBorder</td>
                    <td>The diameter of the brush (in pixels) used to erode the leafBorder Default: 5.</td>
                  </tr>
                  <tr>
                    <td>lesionBorder</td>
                    <td>The diameter of the brush (in pixels) used to erode the lesionBorder Default: 3.</td>
                  </tr>
                  <tr>
                    <td>lesionAreaMin</td>
                    <td>The minimum area of a lesion (in pixels) Default: 10.</td>
                  </tr>
                  <tr>
                    <td>lesionAreaMax</td>
                    <td>The maximum area of a lesion (in pixels) Default: 120000.</td>
                  </tr>
                  <tr>
                    <td>lesionEccentricityMin</td>
                    <td>The minimum eccentricity of a lesion Default: 0.</td>
                  </tr>
                  <tr>
                    <td>lesionEccentricityMax</td>
                    <td>The maximum eccentricity of a lesion Default: 1.</td>
                  </tr>
                  <tr>
                    <td>lesionColorBorder</td>
                    <td>hexadecimal code for output fill color for lesion Default: #0000FF (blue).</td>
                  </tr>
                  <tr>
                    <td>lesionColorBodies</td>
                    <td>hexadecimal code for output bodies color for lesion Default: #FE8E0000 (transparent).</td>
                  </tr>
                  <tr>
                    <td>blurDiameter</td>
                    <td>The diameter of the brush (in pixels) used to blur the image (0 for no blur) Default: 0.</td>
                  </tr>
                  <tr>
                    <td>outPosition</td>
                    <td>join origale and color lesion image at right or buttom Default: right.</td>
                  </tr>
                  <tr>
                    <td>parallelThreadsNum</td>
                    <td>number of thread use, 1 thread analysis 1 image if >= 2 Default: 1.</td>
                  </tr>
                </tbody>
                </table>"
              ),
              p("There is no constraint on the name and the number of the sample images files."),
              p("Note: Surfaces and thicknesses are given in pixels.")
            )
          ),
          tags$div(
            tags$p("Output exemple:"),
            actionButton( "show", class = "btn-img", img(src = "exemple.jpeg", width = '62%'))
          )
        )
      ),
      fluidRow(
        box( width = 6, offset = 0, offset = 0, title = "Edit", status = "success", solidHeader = FALSE, collapsible = TRUE,
          withTags(
            div( class = "analysisHome",
              p("Edition availables:"),
              ul( class="leaf",
                li("remove bad lesion detection (juste click)"),
                li("remove multiple bad lesions detection (brush on image)"),
                li("filter all lesions table with many values (for exemple size of lesion, excentricity, ...)"),
                li("zoom on image"),
                li("change the color of lesion on image (only on edit image not all)")
              ),
              p("Note: if you edit the tables outside LeAFtool, they may not be compatible anymore")
            )
          ),
          tags$div( class = "TrainingHome",
            tags$p("Exemple:"),
            actionButton( "showEdit", class = "btn-img", img(src = "editMode.png", width = '90%'))
          )
        ),
        box( width = 6, offset = 0, offset = 0, title = "Toolbox", status = "success", solidHeader = FALSE, collapsible = TRUE,
          withTags(
            div( class = "analysisHome",
              p("2 functions availables:"),
              ul(class="leaf",
                li("resizeImageDirectory: Resize all images into directory in order to reduce size. The function create new directory with factor resize."),
                li("splitImages: The function split image on n horizontal / m vertical. For exemple if you want to split on 2 equal part. The function create new directory with split images.")
              ),
              tags$p("Exemple:"),
              actionButton( "showSplit", class = "btn-img", img(src = "splitExemple.png", width = '90%'))
            )
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
