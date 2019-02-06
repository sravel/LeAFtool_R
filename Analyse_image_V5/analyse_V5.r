## d?tection de l?sions sur image couleur
## phase 2 : analyse d'image

source("fonctions_analyse_v5.r")


## -------------------- Param?tres de l'analyse -----------------------------------
surface.feuille.mini <- 10000 ## surface minimum d'une feuille
bordure.feuille <- 100 ## ?paisseur de bordure de feuille ? supprimer
bordure.lesion <- 3 ## ?paisseur de bordure de l?sion ? dilater / ?roder
surface.lesion.mini <- 10 ## surface minimum d'une l?sion
surface.lesion.maxi <- 120000 ## surface maximum d'une l?sion
max.eccentricity <- 1 ## excentricit? maximum d'une l?sion
couleur.lesion <-  1 ## couleur des l?sions dans l'image analys?e (0=noir, 1=blanc)
diametre.flou <- 2 ## flou si valeur > 1 (valeur impaire)

## -------------------- R?pertoires et fichiers Exemple_Babeth --------------------------
path.sample <- "../exemples/musaBrut/learning/"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../exemples/musaBrut/samplesRes/"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <-  "../exemples/musaBrut/samples/"     ## R?pertoire de stockage des fichiers images sources
file.image <- "test.jpeg" ## Fichier image source
## -------- Exemple analyse par passage des noms de fichier -----------------------
analyse.image(path.sample=path.sample,
              path.result=path.result,
              path.image=path.image,
              file.image=c(file.image), ## peut contenir plusieurs noms
              surface.feuille.mini=surface.feuille.mini,
              bordure.feuille=bordure.feuille,
              bordure.lesion=bordure.lesion,
              surface.lesion.mini=surface.lesion.mini,
              couleur.lesion=couleur.lesion)







## -------------------- R?pertoires et fichiers Exemple1---------------------------
path.sample <- "../Exemple1/Samples" ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple1/Result"  ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <- "../Exemple1/Images"   ## R?pertoire de stockage des fichiers images sources
file.image <- "IMG_5583_50.jpg"      ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple2 --------------------------
path.sample <- "../Exemple2/Samples"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple2/Result"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <- "../Exemple2/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "pCR17-6-1_kitaake3.jpg" ## Fichier image source

path.sample <- "../Exemple2/Samples2"   ## R?pertoire de stockage des fichiers ?chantillons
file.image <- "Mock1.jpg" ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple_Thomas --------------------------
path.sample <- "../Exemple_Thomas/Samples"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple_Thomas/Result"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <- "../Exemple_Thomas/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "010_repdo5_dpi40_4.jpg" ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple_Mathias --------------------------
path.sample <- "../Exemple_Mathias/Samples1"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple_Mathias/Result1"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)

path.sample <- "../Exemple_Mathias/Samples2"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple_Mathias/Result2"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)

path.image <- "../Exemple_Mathias/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "d?marche1.jpg" ## Fichier image source
file.image <- "feuille_riz.jpg" ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple_Seb_banane --------------------------
path.sample <- "../Exemple_Seb_banane/Samples"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple_Seb_banane/Result"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <- "../Exemple_Seb_banane/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "008_repdo5_dpi40_1.jpg" ## Fichier image source
file.image <- "18_cuba5_dpi42_2.jpg" ## Fichier image source
file.image <- "18_cuba5_dpi42_4.jpg" ## Fichier image source
file.image <- "046_repdo5_dpi40_4.jpg" ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple_Seb_riz --------------------------
path.sample <- "../Exemple_Seb_riz/Samples"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple_Seb_riz/Result"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <- "../Exemple_Seb_riz/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "Mock1.jpg" ## Fichier image source
file.image <- "Chi_P01_1_V2.jpg" ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple_Stella --------------------------
path.sample <- "../Exemple2_Stella/Samples"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple2_Stella/Result"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <-  "../Exemple2_Stella/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "pCR17-6-1_kitaake3.jpg" ## Fichier image source
file.image <- "pCR17-6-1_kitaake_2.jpg" ## Fichier image source

## -------------------- R?pertoires et fichiers Exemple_Babeth --------------------------
path.sample <- "../Exemple_Babeth/apprentissage"   ## R?pertoire de stockage des fichiers ?chantillons
path.result <- "../Exemple_Babeth/Result"    ## R?pertoire de stockage des r?sultats d'analyse, cr?? si inexistant (peut ?tre le m?me que path.image)
path.image <-  "../Exemple_Babeth/Images"     ## R?pertoire de stockage des fichiers images sources
file.image <- "CH1857_rep1_2_25.jpg" ## Fichier image source


## ------------- Fin d'analyse ----------------------------------------------------

## -------- Exemple analyse d'un r?pertoire complet -------------------------------
analyse.image(path.sample=path.sample,
              path.result=path.result,
              path.image=path.image,
              file.image=NA, ## analyse du r?pertoire complet
              surface.feuille.mini=surface.feuille.mini,
              bordure.feuille=bordure.feuille,
              bordure.lesion=bordure.lesion,
              surface.lesion.mini=surface.lesion.mini,
              couleur.lesion=couleur.lesion)
## ------------- Fin d'analyse ----------------------------------------------------

## ----------------- Fin de fichier -----------------------------------------------
