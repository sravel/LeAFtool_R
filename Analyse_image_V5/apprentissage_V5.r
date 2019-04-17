## d?tection de l?sions sur image couleur
## phase 1 : apprentissage ? partir d'images ?chantillonn?es
## path.sample est le r?pertoire contenant les sous-r?pertoires contenant les fichiers d'?channtillons
## les trois derniers arguments de la fonction apprentissage sont (dans cer ordre)
## chaque sous-r?opertoire contient un nombre ind?termin? de fichiers d'une m?me cat?gorie (fond ou limbe ou l?sion)
## trois sous-r?pertoires contenant trois cat?gories de pixels (fond, limbe, l?sion) sont requis,
## r?pertoire path.sample peut contenir d'autres sous-r?pertoires inutilis?s


## Choisir Exemple1 ou Exemple2
path.sample <- "/media/sebastien/Bayer/ScriptsSEB/images/exemples/exemple1/learning/"

library(RColorBrewer)
library(ggplot2)
library("plot3D")
path.sample <- "/media/sebastien/Bayer/ScriptsSEB/images/exemples/musaBrut/learning/"
source("fonctions_apprentissage_V5.r")
apprentissage(path.sample,"background","limb","lesion")

path.sample <- "../Exemple2/Samples"
path.sample <- "../Exemple2/Samples2"
path.sample <- "../Exemple_Thomas/Samples"
path.sample <- "../Exemple_Mathias/Samples1"
path.sample <- "../Exemple_Mathias/Samples2"
path.sample <- "../Exemple_Mathias/Samples_feuille_riz"
path.sample <- "D:/BGPI/BecPhy/Babeth/AnalyseImages/AnalyseImagesV4/Exemple2_Stella/Samples"
path.sample <- "../Exemple_Seb_banane/Samples"
path.sample <- "../Exemple2_Stella/Samples"

apprentissage(path.sample,"fond","limbe1","lesion")
#apprentissage(path.sample,"fond","limbe","lesion_claire","lesion_fonce")
apprentissage(path.sample,"fond","limbe","lesion_noire")
#apprentissage(path.sample,"fond","limbe","lesion_blanche","lesion_noire")
#apprentissage(path.sample,"fond","limbe1","lesion1","lesion2")
apprentissage(path.sample,"fond","limbe","lesionBN")
#apprentissage(path.sample,"limbe","lesion")

apprentissage(path.sample,c("fond","fond2"),c("limbe1","limbe2"),c("lesion1","lesion2"))
apprentissage(path.sample,c("fond_clair","fond_fonce"),c("limbe_clair","limbe_fonce"),c("lesion_claire","lesion_fonce"))

apprentissage(path.sample,"background", "limb", c("lesion", "lesion2"))

## Fin de fichier

fond <- "fond"
limbe <- c("limbe")
lesion <- c("lesion")
