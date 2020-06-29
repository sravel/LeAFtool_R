
source("/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/images/R/training_functions_V6.r")
source("/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/images/R/analysis_functions_v6.r")

### Mo-ble-lab-BGPI
training(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-ble-lab-BGPI/training/', method = 'lda', transform = NULL, colormodel = 'rgb') 
analyseImages(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-ble-lab-BGPI/training/', 
              pathResult = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-ble-lab-BGPI/result', 
              pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-ble-lab-BGPI/samples",
              leafAreaMin = 100000, leafBorder = 11, lesionBorder = 1, lesionAreaMin = 15, lesionAreaMax = 120000, 
              lesionEccentricityMin = 0, lesionEccentricityMax = 0.998, lesionColorBorder = '#0000FF', lesionColorBodies = '#FE8E0000', 
              blurDiameter = 3,  watershedLeafExt = 0, watershedLesionExt = 1, outPosition = 'bottum', parallelThreadsNum = 22)

### Mo-riz-field-BGPI
training(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-field-BGPI/training/', method = 'lda', transform = NULL, colormodel = 'rgb') 
analyseImages(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-field-BGPI/training/', 
              pathResult = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-field-BGPI/result', 
              pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-field-BGPI/samples",
              leafAreaMin = 100000, leafBorder = 4, lesionBorder = 1, lesionAreaMin = 10, lesionAreaMax = 120000, 
              lesionEccentricityMin = 0, lesionEccentricityMax = 0.998, lesionColorBorder = '#0000FF', lesionColorBodies = '#FE8E0000', 
              blurDiameter = 0, outPosition = 'bottum', parallelThreadsNum = 22)

### Mo-riz-lab-BGPI
training(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-lab-BGPI/training/', method = 'lda', transform = NULL, colormodel = 'rgb') 
analyseImages(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-lab-BGPI/training/', 
              pathResult = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-lab-BGPI/result', 
              pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Mo-riz-lab-BGPI/samples",
              leafAreaMin = 10000, leafBorder = 4, lesionBorder = 1, lesionAreaMin = 10, lesionAreaMax = 120000, 
              lesionEccentricityMin = 0, lesionEccentricityMax = 1, lesionColorBorder = '#0000FF', lesionColorBodies = '#FE8E0000', 
              blurDiameter = 0, outPosition = 'bottum', parallelThreadsNum = 22)

### Pf-banane-BGPI
training(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pf-banane-BGPI/training/', method = 'lda', transform = NULL, colormodel = 'rgb') 
analyseImages(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pf-banane-BGPI/training/', 
              pathResult = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pf-banane-BGPI/result', 
              pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pf-banane-BGPI/samples",
              leafAreaMin = 10000, leafBorder = 10, lesionBorder = 1, lesionAreaMin = 10, lesionAreaMax = 120000, 
              lesionEccentricityMin = 0, lesionEccentricityMax = 0.9998, lesionColorBorder = '#0000FF', lesionColorBodies = '#FE8E0000', 
              blurDiameter = 0, outPosition = 'right', parallelThreadsNum = 22)

### Pst-ble-BGPI
training(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pst-ble-BGPI/training/', method = 'lda', transform = NULL, colormodel = 'rgb') 
analyseImages(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pst-ble-BGPI/training/', 
              pathResult = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pst-ble-BGPI/result', 
              pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/Images_Article/Pst-ble-BGPI/samples",
              leafAreaMin = 10000, leafBorder = 10, lesionBorder = 7, lesionAreaMin = 8, lesionAreaMax = 120000, 
              lesionEccentricityMin = 0, lesionEccentricityMax = 0.9998, lesionColorBorder = '#0000FF', lesionColorBodies = '#FE8E0000', 
              blurDiameter = 0, outPosition = 'bottum', parallelThreadsNum = 22)







analyseImages(pathTraining = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/learning",
             pathResult = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/results", 
             pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/samples", parallelThreadsNum=22)


training("/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/learning", method="lda", transform=NULL, colormodel="rgb")


analyseImages(pathTraining = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/learning', 
              pathResult = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/results', 
              pathImages = '/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/samples', 
              fileImage = NULL, 
              leafAreaMin = 1000, 
              leafBorder = 5, 
              lesionBorder = 3, 
              lesionAreaMin = 10, 
              lesionAreaMax = 120000, 
              lesionEccentricityMin = 0, 
              lesionEccentricityMax = 1, 
              lesionColorBorder = '#0000FF', 
              lesionColorBodies = '#FE8E0000', 
              leafColorBorder = '#FF000000', 
              leafColorBodies = '#FF000000', 
              blurDiameter = 6, 
              watershedLeafExt = 1.2, 
              watershedLesionExt = 1, 
              outPosition = 'right', 
              parallelThreadsNum = 4) 



analyseImages(pathTraining = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/learning",
              pathResult = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/results", 
              pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/exemple1/samples",
              leafAreaMin = "toto")

training("/media/work/amanda/images/LeafTool/calibrationMadu/Samples/", method="lda", transform=NULL, colormodel="rgb")

analyseImages(pathTraining = "/media/work/amanda/images/LeafTool/calibrationMadu/Samples/",
              pathResult = "/media/work/amanda/images/LeafTool/Madu2/dpi49",
              pathImages = "/media/work/amanda/images/Madu2/6AFmadu2dpi49cut",
              leafBorder = 130,
              parallelThreadsNum = 22)


pathTraining = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/riz/learning"
pathResult= "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/riz/result"
pathImages = "/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/exemples/riz/image"

fileImage=NA
leafAreaMin=10000
leafBorder=11
lesionBorder=3
lesionAreaMin=10
lesionAreaMax=12000,
lesionEccentricityMin=0
lesionEccentricityMax=1
lesionColorBorder="#0000FF11"
lesionColorBodies="#FE8E0000"
blurDiameter=0
outPosition="right"
parallelThreadsNum=1
mode="CMD"

