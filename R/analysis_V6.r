## détection de lésions sur image couleur
## phase 2 : analyse d'image

source("analysis_functions_V6.r")

## -------------------- Paramètres de l'analyse -----------------------------------
.leaf.area.min <- 1000 ## surface minimum d'une feuille
.leaf.border <- 3 ## épaisseur de bordure de feuille à supprimer
.lesion.border <- 3 ## épaisseur de bordure de lésion à dilater / éroder
.lesion.area.min <- 10 ## surface minimum d'une lésion
##surface.lesion.maxi <- 12000 ## surface maximum d'une lésion
.lesion.area.max <- 10000 ## surface maximum d'une lésion
.lesion.eccentricity.max <- 0.999 ## excentricité maximum d'une lésion
.lesion.color <-  1 ## couleur des lésions dans l'image analysée (0=noir, 1=blanc)
.blur.diameter <- 5 ## flou si valeur > 1 (valeur impaire)

## -------------------- Répertoires et fichiers Exemple_Dominique---------------------------
path.sample <- "../Exemple_Dominique/GT1 stade C recto verso/Samples" ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Dominique/GT1 stade C recto verso/Result"  ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple_Dominique/GT1 stade C recto verso/Images"   ## Répertoire de stockage des fichiers images sources
##file.image <- "IMG_5583_50.jpg"      ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Dominique---------------------------
path.sample <- "../Exemple_Dominique/GT1 stade D Recto Verso/Samples" ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Dominique/GT1 stade D Recto Verso/Result"  ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple_Dominique/GT1 stade D Recto Verso/Images"   ## Répertoire de stockage des fichiers images sources
##file.image <- "IMG_5583_50.jpg"      ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Dominique ---------------------------
path.sample <- "../Exemple_Dominique/PB317 stadeC note 0à4 recto verso/Samples" ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Dominique/PB317 stadeC note 0à4 recto verso/Result"  ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple_Dominique/PB317 stadeC note 0à4 recto verso/Images2"   ## Répertoire de stockage des fichiers images sources
##file.image <- "IMG_5583_50.jpg"      ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Dominique anthracnose face inférieure ---------------------------
path.sample <- "../Exemple_Dominique/Anthracnose/Samples/Inf"
path.result <- "../Exemple_Dominique/Anthracnose/Result/Inf"
path.image <- "../Exemple_Dominique/Anthracnose/Images_2"
file.image <- "IRCA18_ANT_score2_2_abaxial_stade C_2.jpg"

## -------------------- Répertoires et fichiers Exemple_Dominique anthracnose face supérieure ---------------------------
path.sample <- "../Exemple_Dominique/Anthracnose/Samples/Sup"
path.result <- "../Exemple_Dominique/Anthracnose/Result/Sup"
path.image <- "../Exemple_Dominique/Anthracnose/Images_2"
file.image <- "IRCA18_ANT_score2_2_adaxial_stade C_2.jpg"

## -------------------- Répertoires et fichiers Exemple_Dominique CLFD  face inférieure ---------------------------
path.sample <- "../Exemple_Dominique/CLFD/Samples/Inf"
path.result <- "../Exemple_Dominique/CLFD/Result/Inf"
path.image <- "../Exemple_Dominique/CLFD/Images_2"
file.image <- "IRCA18_CLFD_score2_1_Abaxial_2.jpg"

## -------------------- Répertoires et fichiers Exemple_Dominique CLFD  face supérieure ---------------------------
path.sample <- "../Exemple_Dominique/CLFD/Samples/Sup"
path.result <- "../Exemple_Dominique/CLFD/Result/Sup"
path.image <- "../Exemple_Dominique/CLFD/Images_2"
file.image <- "IRCA18_CLFD_score2_1_Adaxial_2.jpg"

## -------------------- Répertoires et fichiers Exemple1---------------------------
path.sample <- "../Exemple1/Samples" ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple1/Result"  ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple1/Images"   ## Répertoire de stockage des fichiers images sources
file.image <- "IMG_5583_50.jpg"      ## Fichier image source

## -------------------- Répertoires et fichiers Exemple2 --------------------------
path.sample <- "../Exemple2/Samples"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple2/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple2/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "pCR17-6-1_kitaake3.jpg" ## Fichier image source

path.sample <- "../Exemple2/Samples2"   ## Répertoire de stockage des fichiers échantillons
file.image <- "Mock1.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Thomas --------------------------
path.sample <- "../Exemple_Thomas/Samples"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Thomas/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple_Thomas/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "010_repdo5_dpi40_4.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Mathias --------------------------
path.sample <- "../Exemple_Mathias/Samples1"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Mathias/Result1"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)

path.sample <- "../Exemple_Mathias/Samples2"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Mathias/Result2"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)

path.image <- "D:/BGPI/BecPhy/Babeth/AnalyseImages/Exemples/Mathias/Images"
##path.image <- "../Exemple_Mathias/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "démarche1.jpg" ## Fichier image source
file.image <- "feuille_riz.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Seb_banane --------------------------
path.sample <- "../Exemple_Seb_banane/Samples2"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Seb_banane/Result2"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "D:/BGPI/BecPhy/Babeth/AnalyseImages/Exemples/Seb_banane/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "008_repdo5_dpi40_1.jpg" ## Fichier image source
file.image <- "18_cuba5_dpi42_2.jpg" ## Fichier image source
file.image <- "18_cuba5_dpi42_4.jpg" ## Fichier image source
file.image <- "046_repdo5_dpi40_4.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Seb_riz --------------------------
path.sample <- "../Exemple_Seb_riz/Samples"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Seb_riz/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple_Seb_riz/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "Mock1.jpg" ## Fichier image source
file.image <- "Chi_P01_1_V2.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Stella --------------------------
.path.sample <- "../Exemple2_Stella/Samples"   ## Répertoire de stockage des fichiers échantillons
.path.result <- "../Exemple2_Stella/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
.path.image <-  "D:/BGPI/BecPhy/Babeth/AnalyseImages/Exemples/Stella/Images"     ## Répertoire de stockage des fichiers images sources
.file.image <- "pCR17-6-1_kitaake3.jpg" ## Fichier image source
file.image <- "pCR17-6-1_kitaake_2.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Babeth --------------------------
path.sample <- "../Exemple_Babeth/apprentissage"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Babeth/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <-  "../Exemple_Babeth/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "CH1857_rep1_2_25.jpg" ## Fichier image source

## -------- Exemple analyse par passage des noms de fichier -----------------------
analyse.image(path.sample=.path.sample,
              path.result=.path.result,
              path.image=.path.image,
              file.image=c(.file.image), ## peut contenir plusieurs noms
              leaf.area.min=.leaf.area.min,
              leaf.border=.leaf.border,
              lesion.border=.lesion.border,
              lesion.area.min=.lesion.area.min,
              lesion.area.max=.lesion.area.max,
              lesion.eccentricity.max=.lesion.eccentricity.max,
              lesion.color=.lesion.color,
              blur.diameter=.blur.diameter)
## ------------- Fin d'analyse ----------------------------------------------------

## -------- Exemple analyse d'un répertoire complet -------------------------------
analyse.image(path.sample=.path.sample,
              path.result=.path.result,
              path.image=.path.image,
              file.image=NA, ## analyse du répertoire complet
              leaf.area.min=.leaf.area.min,
              leaf.border=.leaf.border,
              lesion.border=.lesion.border,
              lesion.area.min=.lesion.area.min,
              lesion.area.max=.lesion.area.max,
              lesion.eccentricity.max=.lesion.eccentricity.max,
              lesion.color=.lesion.color,
              blur.diameter=.blur.diameter)
## ------------- Fin d'analyse ----------------------------------------------------

## ----------------- Fin de fichier -----------------------------------------------
