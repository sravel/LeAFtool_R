## détection de lésions sur image couleur
## phase 2 : analyse d'image

source("fonctions_analyse_V5.r")

## -------------------- Paramètres de l'analyse -----------------------------------
surface.feuille.mini <- 1000 ## surface minimum d'une feuille
bordure.feuille <- 3 ## épaisseur de bordure de feuille à supprimer
bordure.lesion <- 3 ## épaisseur de bordure de lésion à dilater / éroder
surface.lesion.mini <- 10 ## surface minimum d'une lésion
surface.lesion.maxi <- 12000 ## surface maximum d'une lésion
max.eccentricity <- 0.9999 ## excentricité maximum d'une lésion
couleur.lesion <-  1 ## couleur des lésions dans l'image analysée (0=noir, 1=blanc)
diametre.flou <- 5 ## flou si valeur > 1 (valeur impaire)

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

path.image <- "../Exemple_Mathias/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "démarche1.jpg" ## Fichier image source
file.image <- "feuille_riz.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Seb_banane --------------------------
path.sample <- "../Exemple_Seb_banane/Samples"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Seb_banane/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <- "../Exemple_Seb_banane/Images"     ## Répertoire de stockage des fichiers images sources
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
path.sample <- "../Exemple2_Stella/Samples"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple2_Stella/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <-  "../Exemple2_Stella/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "pCR17-6-1_kitaake3.jpg" ## Fichier image source
file.image <- "pCR17-6-1_kitaake_2.jpg" ## Fichier image source

## -------------------- Répertoires et fichiers Exemple_Babeth --------------------------
path.sample <- "../Exemple_Babeth/apprentissage"   ## Répertoire de stockage des fichiers échantillons
path.result <- "../Exemple_Babeth/Result"    ## Répertoire de stockage des résultats d'analyse, créé si inexistant (peut être le même que path.image)
path.image <-  "../Exemple_Babeth/Images"     ## Répertoire de stockage des fichiers images sources
file.image <- "CH1857_rep1_2_25.jpg" ## Fichier image source

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
## ------------- Fin d'analyse ----------------------------------------------------

## -------- Exemple analyse d'un répertoire complet -------------------------------
analyse.image(path.sample=path.sample,
              path.result=path.result,
              path.image=path.image,
              file.image=NA, ## analyse du répertoire complet
              surface.feuille.mini=surface.feuille.mini,
              bordure.feuille=bordure.feuille,
              bordure.lesion=bordure.lesion,
              surface.lesion.mini=surface.lesion.mini,
              couleur.lesion=couleur.lesion)
## ------------- Fin d'analyse ----------------------------------------------------

## ----------------- Fin de fichier -----------------------------------------------
