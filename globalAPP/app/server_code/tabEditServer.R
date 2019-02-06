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



############################################
## Output Directory path of analysis
############################################
shinyDirChoose(
  input,'dirInResult',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

# To add file path in UI
output$dirInResult <- renderText({
  rv$dirInResult
})

# extract file Path on input
observeEvent(input$dirInResult,{
  if (!is.integer(input$dirInResult))
  {
    home <- normalizePath(allVolumesAvail[input$dirInResult$root])
    rv$dirInResult <- file.path(home, paste(unlist(input$dirInResult$path[-1],"/"), collapse = .Platform$file.sep))
    loadImageEdit <- list.files(rv$dirInResult, full.names=FALSE, pattern = "*_lesion.jpeg")
    updateSelectInput(session, "imageEdit", label = NULL, choices = loadImageEdit)
  }
})

## OBSERVATEUR A MODIFIER
observe({

  if (!is.null(rv$dirInResult))# && input$imageEdit != "")
  {
    rv$lesion_color_borderEdit <- input$lesion_color_borderEdit
    rv$lesion_color_bodiesEdit <- input$lesion_color_bodiesEdit
    rv$lesion_color_borderAlphaEdit <-col2rgb(input$lesion_color_borderEdit, alpha=TRUE)[4]/255
    rv$lesion_color_bodiesAlphaEdit <-col2rgb(input$lesion_color_bodiesEdit, alpha=TRUE)[4]/255
    leaves <- rv$loadCSVcurrentImage$leaf.number
    isolate(updateAll(leaves))
  }
  #### SAVE IMAGE IF CHANGE COLOR
  if (!is.null(rv$loadcurrentImageEdit) && !is.null(rv$loadcurrentImageOriginaleEdit) && !is.null(rv$originalFileNameBoth) && !is.null(rv$originalFileName))
  {
    isolate({
      # print both sample and lesion images
      jpeg(rv$originalFileNameBoth,
           width = rv$widthSize,
           height = rv$heightSize*2,
           units = "px")
      par( mfrow = c(2,1) )
      display(rv$loadcurrentImageOriginaleEdit, method="raster")
      display(rv$loadcurrentImageEdit, method = "raster")
      dev.off()

      # print only output file image with lesion
      jpeg(rv$originalFileName,
           width = rv$widthSize,
           height = rv$heightSize,
           units = "px")
      par( mfrow = c(1,1) )
      display(rv$loadcurrentImageEdit, method = "raster")
    #  points(result$m.cx, result$m.cy, pch='+', cex=2, col="blue")

      dev.off()
    })
  }
})


output$plotcurrentImageEdit <- renderPlot({
  if ( is.null(rv$loadcurrentImageEdit)) return(NULL)
  plot(rv$loadcurrentImageEdit)
  color <- ifelse(rv$loadCSVcurrentImage$lesion.status == "keep", "green", "red")
  points(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, pch='+', cex=2, col=color)
})
output$plotcurrentImageOriginalEdit <- renderPlot({

 ### PLOT ALL FILE if checkbox uncheck
  if (rv$zoomOriginalCheck == FALSE){
    if ( is.null(rv$loadcurrentImageOriginaleEdit)) return(NULL)
    plot(rv$loadcurrentImageOriginaleEdit)
    color <- ifelse(rv$loadCSVcurrentImage$lesion.status == "keep", "green", "red")
    if (rv$pchOriginal == TRUE){
#      points(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, pch='+', cex=1, col=color)
      text(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, labels=rv$loadCSVcurrentImage$lesion.number, cex=1, col=color)
    }
    points(input$plot_hover$x, input$plot_hover$y, pch='+', cex=2, col="green")
  }
  #  else Plot zoom original File
  if (rv$zoomOriginalCheck == TRUE){

    if (is.null(rv$zoomInitial)) return(NULL)
    plot(rv$zoomInitial)
#    if (rv$pchOriginal == TRUE){
##      points(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, pch='+', cex=1, col=color)
#      color <- ifelse(rv$loadCSVcurrentImage$lesion.status == "keep", "green", "red")
#      text(rv$loadCSVcurrentImage$m.cx-rv$addleft, rv$loadCSVcurrentImage$m.cy-rv$addbottom, labels=rv$loadCSVcurrentImage$lesion.number, cex=1, col=color)
#    }
    points(rv$pointx, rv$pointy, pch='+', cex=3, col="blue")
  }
})


observeEvent(input$imageEdit, {
  #Load image for plot
  lesionImg <- strsplit(input$imageEdit, ".", fixed = TRUE)[[1]][1]
  pathImg <- paste(rv$dirInResult, input$imageEdit, sep = '/')
#  print(pathImg)
  if(file.exists(pathImg) && !is.null(rv$dirInResult) && input$imageEdit != ""){
    baseName <- gsub("_lesion", "", lesionImg)
    rv$originalFileName <- paste0(rv$dirInResult, "/",baseName,"_lesion.jpeg")
    rv$originalFileNameBoth <- paste0(rv$dirInResult, "/",baseName,"_both.jpeg")
    rv$loadcurrentImageEdit <- readImage(pathImg)
    img <- readImage(rv$originalFileNameBoth)
    rv$widthSize = dim(img)[1]
    rv$heightSize = dim(img)[2]/2
    rv$loadcurrentImageOriginaleEdit <- img[1:rv$widthSize,1:rv$heightSize,]
    rv$loadCSVcurrentImageName <- paste0(rv$dirInResult, "/",baseName,"_All_lesions.csv")
    rv$loadCSVcurrentImage <- read.csv(rv$loadCSVcurrentImageName ,header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    rv$csv_Merge_lesionsFile <- paste(rv$dirInResult, "/",baseName,"_Merge_lesions.csv", sep = '')
    rv$AG <- read.csv(rv$csv_Merge_lesionsFile ,header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    rv$MERGE <- read.csv(paste0(rv$dirInResult,"/merge_ResumeCount.csv") ,header = TRUE, sep = "\t", stringsAsFactors = FALSE)

    load(file = paste0(rv$dirInResult, "/",baseName,".RData"), envir = .GlobalEnv)
#    print(analyse.li)
  }
})


output$results <- DT::renderDataTable({

DT::datatable(data = as.data.frame(rv$loadCSVcurrentImage, stringAsFactors = FALSE, row.names = NULL),

                                     escape=FALSE,
                                     selection="single",
                                     style = "bootstrap",
                                     filter = list(position = 'top', clear = TRUE, plain = FALSE),
                                     options = list(
                                       paging=TRUE,searching = TRUE,ordering=TRUE,scrollCollapse=FALSE,server = FALSE, autoWidth = TRUE
                                     )
              )
})

output$AG <- DT::renderDataTable({

DT::datatable(data = as.data.frame(rv$AG , stringAsFactors = FALSE, row.names = NULL),

                                     escape=FALSE,
                                     selection="single",
                                     style = "bootstrap",
                                     filter = "none",
                                     options = list(
                                       paging=TRUE,searching = FALSE,ordering=TRUE,scrollCollapse=FALSE,server = FALSE, autoWidth = TRUE
                                     )
              )
})

output$MERGE <- DT::renderDataTable({

DT::datatable(data = as.data.frame(rv$MERGE , stringAsFactors = FALSE, row.names = NULL),

                                     escape=FALSE,
                                     selection="single",
                                     style = "bootstrap",
                                     filter = "none",
                                     options = list(
                                       paging=TRUE,searching = FALSE,ordering=TRUE,scrollCollapse=FALSE,server = FALSE, autoWidth = TRUE
                                     )
              )
})



observeEvent(input$plot_click$x,{

if(is.null(input$plot_click$x)) return(NULL)
  click <- c(input$plot_click$x, input$plot_click$y)
  res <- nearPoints(rv$loadCSVcurrentImage, input$plot_click, xvar = "m.cx", yvar = "m.cy", threshold = 10, maxpoints = 1)
  row <- as.integer(row.names(res))

  if(length(res$lesion.number) != 0){
    rv$loadCSVcurrentImage$lesion.status[row] <- if (rv$loadCSVcurrentImage$lesion.status[row]=="keep") "remove" else if (rv$loadCSVcurrentImage$lesion.status[row]=="remove") "keep"
  }
  write.table(
    rv$loadCSVcurrentImage,
    file = rv$loadCSVcurrentImageName,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  leaves <- res$leaf.number
  updateAll(leaves)
})

################## BRUSH

observeEvent(input$plot_brush,{
  res <- brushedPoints(rv$loadCSVcurrentImage, input$plot_brush, xvar = "m.cx", yvar = "m.cy")
  if (nrow(res) == 0)
    return()
  for (i in row.names(res)) {
    row <- as.integer(i)
    rv$loadCSVcurrentImage$lesion.status[row] <- if (rv$loadCSVcurrentImage$lesion.status[row]=="keep") "remove" else if (rv$loadCSVcurrentImage$lesion.status[row]=="remove") "keep"
  }
    write.table(
    rv$loadCSVcurrentImage,
    file = rv$loadCSVcurrentImageName,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )
  leaves <- res$leaf.number
  updateAll(leaves)

})

  output$zoomcurrentImageEdit <- renderPlot({
    if (is.null(rv$zoom)) return(NULL)
    plot(rv$zoom)
    points(rv$pointx, rv$pointy, pch='+', cex=3, col="blue")
  })

 observeEvent(input$plot_hover, {
   if (!is.null(input$plot_hover$x) &&!is.null(input$plot_hover$y)){
     xleft <- input$plot_hover$x - 100
     xright <- input$plot_hover$x + 100
     ytop <- input$plot_hover$y - 100
     ybottom <- input$plot_hover$y + 100
     rv$pointy <- (ybottom-ytop)/2
     rv$pointx <- (xright-xleft)/2

     if (xleft <= 0 ){
       xleft <- 1
       xright <- 200
       rv$pointx <- if (input$plot_hover$x <= 0 ) 1 else  input$plot_hover$x
     }
     else if (xright >= rv$widthSize ){
       xright <- rv$widthSize
       xleft <- rv$widthSize - 200
       rv$pointx <- if (input$plot_hover$x >= rv$widthSize) rv$widthSize else  input$plot_hover$x - xleft
     }
     if (ytop <= 0 ){
       ytop <- 1
       ybottom <- 200
       rv$pointy <- input$plot_hover$y
     }
     else if (ybottom >= rv$heightSize ){
       ybottom <- rv$heightSize
       ytop <- rv$heightSize - 200
       rv$pointy <- input$plot_hover$y - ytop
     }

     if (is.null(rv$pointy)) rv$pointy <- (ybottom-ytop)/2
     if (is.null(rv$pointx)) rv$pointx <- (xright-xleft)/2

     output$coor <- renderPrint(print(paste("xleft",xleft,
                 "xright",xright,
                 "ytop",ytop,
                 "ybottom",ybottom,
                 "input$plot_hover$x",input$plot_hover$x,
                 "input$plot_hover$y",input$plot_hover$y,
                 "rv$pointx",rv$pointx,
                 "rv$pointy",rv$pointy
                 ))
     )

    rv$addleft <- xleft
    rv$addbottom <- ybottom

     rv$zoom <- rv$loadcurrentImageEdit[xleft:xright, ytop:ybottom,]
     rv$zoomInitial <- rv$loadcurrentImageOriginaleEdit[xleft:xright, ytop:ybottom,]
   }

 })

updateAll <- function(leaves){

  ## sortie des résultats et coloration des lésions
  for (leaf in unique(leaves))
  {
    maskLesion <- analyse.li[[leaf]][["maskLesion"]]
    maskLesionRed <- maskLesion

    remove <-  rv$loadCSVcurrentImage$lesion.number[ (rv$loadCSVcurrentImage$lesion.status=="remove") & (rv$loadCSVcurrentImage$leaf.number==leaf) ]
    keep <-  rv$loadCSVcurrentImage$lesion.number[ (rv$loadCSVcurrentImage$lesion.status=="keep") & (rv$loadCSVcurrentImage$leaf.number==leaf) ]

    maskLesion[!(maskLesion %in% keep)] <- 0 ## suppression dans mask des objets rouges
    maskLesionRed[!(maskLesionRed %in% remove)] <- 0 ## suppression dans mask2 des objets non rouges

    tmpimage <- rv$loadcurrentImageOriginaleEdit[li[[leaf]]$b$y, li[[leaf]]$b$x,]
#    tmpimage <- rv$loadcurrentImageEdit[li[[leaf]]$b$y, li[[leaf]]$b$x,]
    ## coloration des objets selon leur surface

    tmpimage <- paintObjects(maskLesion  ,tmpimage, thick=TRUE, col=c(rv$lesion_color_borderEdit, rv$lesion_color_bodiesEdit), opac=c(rv$lesion_color_borderAlphaEdit, rv$lesion_color_bodiesAlphaEdit))
    tmpimage <- paintObjects(maskLesionRed ,tmpimage, thick=TRUE, col=c("red", "red"), opac=c(0.3, 0))

    rv$loadcurrentImageEdit[li[[leaf]]$b$y, li[[leaf]]$b$x,] <- tmpimage
  }

  result <- rv$loadCSVcurrentImage[rv$loadCSVcurrentImage$lesion.status != "remove", ]

  if ( !is.null(result) )
  {
    # Update merge leaves stats
    ag.count <- aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], length)
    names(ag.count)[4] <- "lesion.nb"
  #  print(ag.count)

    ag.surface <- aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], sum)
    names(ag.surface)[4] <- "lesion.surface"
  #  print(ag.surface)

    ag <- merge(ag.count, ag.surface)
    ag$pourcent.lesions <- ag$lesion.surface / ag$leaf.surface * 100
    ag$lesion.nb[ag$lesion.surface == 0] <- 0
    ag$lesion.surface[ag$lesion.surface == 0] <- 0

    # if no lesions in one or more leaves
    for (leafNum in unique(rv$loadCSVcurrentImage$leaf.number))
    {
      if (! (leafNum %in% unique(ag$leaf.number) ))
      {
        leafSurf <- rv$loadCSVcurrentImage$leaf.surface[rv$loadCSVcurrentImage$leaf.number == leafNum][1]
        line <- data.frame(image = ag$image[1], leaf.number = leafNum, leaf.surface = leafSurf, lesion.nb = 0, lesion.surface = 0, pourcent.lesions = 0)
        ag <- rbind(ag,line)
      }
    }

  #  print(ag)

    rv$AG <- ag[order(ag$leaf.number),]

    write.table(
      rv$AG,
      file = rv$csv_Merge_lesionsFile,
      quote = FALSE,
      row.names = FALSE,
      sep = '\t'
    )

    rv$MERGE = multmerge(rv$dirInResult, "*_Merge_lesions.csv")

    write.table(
      rv$MERGE,
      file = paste0(rv$dirInResult,"/merge_ResumeCount.csv"),
      quote = FALSE,
      row.names = FALSE,
      sep = '\t'
    )
  }
}

######### add pch to original image
observeEvent(input$pchOriginal,{
  rv$pchOriginal <- input$pchOriginal
})
observeEvent(input$zoomOriginalCheck,{
  rv$zoomOriginalCheck <- input$zoomOriginalCheck
})


#  output$plot_brushedpoints <- renderTable({
#    res <- brushedPoints(rv$loadCSVcurrentImage, input$plot_brush, xvar = "m.cx", yvar = "m.cy")
#    if (nrow(res) == 0)
#      return()
#    res
#  })

#  output$infos <- renderPrint({
#    nearPoints(rv$loadCSVcurrentImage, input$plotcurrentImageEdit, xvar = "m.cx", yvar = "m.cy", threshold = 10, maxpoints = 1)
#  })
#output$position <- renderText({
#  paste0("x=", input$plotcurrentImageEdit$x, "\ny=", input$plotcurrentImageEdit$y)

#})

#output$brushPos <- renderPrint({
#  paste0("x=", input$plot_brush)

#})
