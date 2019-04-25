#!/usr/bin/Rscript

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
# If not see <http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.txt>
# 
# Intellectual property belongs to CIRAD and South Green developpement plateform
# Version 0.1.0 written by Sebastien RAVEL, François BONNOT, Sajid ALI, FOURNIER Elisabeth
#####################################################################################################
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.



library(parallel)
max_no_cores <- as.numeric(max(1, detectCores() - 2))

runCountLesion <- function(port=NULL) {

  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
  runApp(appDir = getwd(),launch.browser = TRUE)
}