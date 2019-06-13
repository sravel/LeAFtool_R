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

options(width=160)

## packages nécessaires
library(EBImage)
library(lattice)
library(MASS)

## fonction de lecture des images d'un groupe ; retourne le data.frame des pixels
load_group <- function(g,pathTraining) {
  path_group <- paste(pathTraining,g,sep='/')
  files_group <- list.files(path_group,full.name=TRUE)
  sample <- lapply(files_group,readImage)
  ## constitution du data frame des pixels échantillonnés
  li <- lapply(sample, function(im) {
    data.frame(group=g,red=as.numeric(imageData(im)[,,1]), green=as.numeric(imageData(im)[,,2]), blue=as.numeric(imageData(im)[,,3]))
  })
  do.call(rbind, li)
}

apprentissage <- function(pathTraining,...) {
  ## Les arguments passés dans "..." doivent être (dans cet ordre) le nom (relatif) des sous-répertoires fond, limb, lésions
  ## Recherche des sous-répertoires de pathTraining
  progress$inc(2/7, detail = "Load sub-directories 2/6")
  dirs <- list.dirs(pathTraining,full.names=FALSE)[-1] ## -1 pour supprimer le premier nom (toujouts vide)

  ## vérification de l'existence des sous-répertoires passés en argument
  group <- list(...)
  if (any(is.na(match(unlist(group),dirs)))) stop("Répertoire(s) inexistant(s).")

  ## constitution du data.frame des pixels des échantillons
  progress$inc(3/7, detail = "Build dataframe with learning 3/6")
  li <- lapply(group,load_group,pathTraining)
  df2 <- do.call(rbind, li)

  ## analyse discriminante
  progress$inc(4/7, detail = "Build analysis discriminante 4/6")
  lda1 <- lda(df2[2:4], df2$group)

  ## nom commun aux 3 fichiers de sortie, identique au nom du réprtoire
  basename <- tail(strsplit(pathTraining,'/')[[1]],1)

  ## écriture du fichier texte des résultats
  progress$inc(5/7, detail = "Write output files (csv,jpeg) 5/6")
  file.txt <- paste(pathTraining,paste0(basename,".txt"),sep='/') ## fichier de sortie texte
  sink(file.txt)
  print(table(df2$group))
  print(lda1$scaling)
  df2$predict <- predict(lda1, df2[2:4])$class
  sink()

  outTrainingCSV <- paste(pathTraining,paste0(basename,"_info.csv"),sep='/') ## fichier de sortie csv
  outTrainingTable <- as.data.frame.matrix(table(df2$group, df2$predict))
  write.csv2(outTrainingTable, file = outTrainingCSV)

  ## graphe des groupes dans le plan discriminant
  plotFileTraining <- paste(pathTraining,paste0(basename,".jpeg"),sep='/') ## fichier de sortie jpeg
  df4 <- cbind(df2, as.data.frame(as.matrix(df2[2:4])%*%lda1$scaling))

  jpeg(plotFileTraining)
  print(xyplot(LD2~LD1, group=group, cex=0.8, alpha=1, pch=1, asp=1, auto.key=TRUE, data=df4))
  dev.off()

  ## sauvegarde de l'analyse
  progress$inc(6/7, detail = "Save analysis into R file 6/6")
  fileRData <- paste(pathTraining,paste0(basename,".RData"),sep='/')
  save(lda1,file=fileRData)
  return(list(code = 1, mess = fileRData,outTrainingTable = outTrainingTable, outTrainingCSV = outTrainingCSV, fileRData = fileRData,plotFileTraining = plotFileTraining))
}

## Fin de fichier
