#!/usr/bin/Rscript

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


fileRdata = "/media/sebastien/JJC2018/test_visilog/calibration/calibration.RData"
pathResult = "/media/sebastien/JJC2018/test_visilog/result"
pathImages = "//media/sebastien/JJC2018/test_visilog/sample"

fileRdata = "/media/sebastien/Bayer/ScriptsSEB/scripts/GUI/countLesionTools/exemples/Images/Apprentissage/Apprentissage.RData"
pathResult = "/media/sebastien/Bayer/ScriptsSEB/scripts/GUI/countLesionTools/exemples/Images/Result"
pathImages = "/media/sebastien/Bayer/ScriptsSEB/scripts/GUI/countLesionTools/exemples/Images/samples1"
onefileImage = NA
leafMinSize = 1000
leafBorderSize = 3
lesionBorderSize = 3
lesionMinSize = 10
colorLesion = 0

rm(list = ls())
gc()

runCountLesion <- function(port=NULL) {
  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
  runApp(appDir = getwd(),launch.browser = TRUE)
}

