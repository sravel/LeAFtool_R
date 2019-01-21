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

###############################################
## CODE of function for calibration
###############################################
## function of reading images of a group; return the pixel data.frame
load_group <- function(g) {
  path_group <- paste(rv$dirCalibration,g,sep='/')
  files_group <- list.files(path_group,full.name=TRUE)
  sample <- lapply(files_group,readImage)
  ## creation of the data frame of the sampled pixels
  li <- lapply(sample, function(im) {
    data.frame(group=g,red=as.numeric(imageData(im)[,,1]), green=as.numeric(imageData(im)[,,2]), blue=as.numeric(imageData(im)[,,3]))
  })
  do.call(rbind, li)
}

calibration <- function() {


}

###############################################
## CODE serveur
###############################################
#### Calibration Directory path ####

# option to load directory path bottom for calibration folder
shinyDirChoose(
  input,
  'dirCalibration',
  filetypes = c('', 'txt', 'Rdata', "png", "csv", "*"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

# return to UI path selected for calibration
output$dirCalibration <- renderText({
  rv$dirCalibration
})

# when click to bottom update path
observeEvent(
  input$dirCalibration,{
    if (!is.integer(input$dirCalibration))
    {
      # initialize path
      home <- normalizePath(allVolumesAvail[input$dirCalibration$root])
      rv$dirCalibration <- file.path(home,paste(unlist(input$dirCalibration$path[-1]), collapse = .Platform$file.sep))
      # if reload after first calibration, reset value
      rv$exitStatusCal <- -1
      rv$messCal <- NULL
      rv$errCal <- NULL

      # test if all subfolder mandatory
      listdirCalibration <- existDirCalibration(rv$dirCalibration)

      # if all subfolder exist run analysis
      if(listdirCalibration$dirlimb == TRUE && listdirCalibration$dirBackground == TRUE && listdirCalibration$dirLesion == TRUE){

        # hide app
        show(id = "loading-content")
        # add progress bar
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = 'Making calibration, please wait\n', value = 0)
        progress$inc(1/7, detail = "Start step 1/6")

        ###########################
        # call function calibration
        ###########################
         ## Search subdirectories of rv $ dirCalibration
        progress$inc(2/7, detail = "Load sub-directories 2/6")
        dirs <- list.dirs(rv$dirCalibration,full.names=FALSE)[-1] ## -1 to delete the first name (always empty)

        ## checking the existence of subdirectories passed as arguments
#        group <- list("background","limb",c("lesion","lesion2"))
        group <- list("background","limb","lesion")
        if (any(is.na(match(unlist(group),dirs)))) stop("Directory(ies) nonexistent(s).")

        ## constitution of the data.frame of the pixels of the samples
        progress$inc(3/7, detail = "Build dataframe with learning 3/6")
        li <- lapply(group,load_group)
        df2 <- do.call(rbind, li)

        ## iscriminant analysis
        progress$inc(4/7, detail = "Build analysis discriminante 4/6")
        lda1 <- lda(df2[2:4], df2$group)

        ## name common to the 3 output files, identical to the name of the directory
        basename <- tail(strsplit(rv$dirCalibration,'/')[[1]],1)

        ## writing the text file of the results
        progress$inc(5/7, detail = "Write output files (csv,jpeg) 5/6")
        file.txt <- paste(rv$dirCalibration,paste0(basename,".txt"),sep='/') ## output file texte
        sink(file.txt)
        print(table(df2$group))
        print(lda1$scaling)
        df2$predict <- predict(lda1, df2[2:4])$class
        sink()

        rv$outCalibrationCSV <- paste(rv$dirCalibration,paste0(basename,"_info.csv"),sep='/') ## output file csv
        rv$outCalibrationTable <- as.data.frame.matrix(table(df2$group, df2$predict))
        write.csv2(rv$outCalibrationTable, file = rv$outCalibrationCSV)

        ## graph of groups in the discriminant plane
        rv$plotFileCalibration <- paste(rv$dirCalibration,paste0(basename,".jpeg"),sep='/') ## output file jpeg
        df4 <- cbind(df2, as.data.frame(as.matrix(df2[2:4])%*%lda1$scaling))

        jpeg(rv$plotFileCalibration)
        print(xyplot(LD2~LD1, group=group, cex=0.8, alpha=1, pch=1, asp=1, auto.key=TRUE, data=df4))
        dev.off()

        ## sauvegarde de l'analyse
        progress$inc(6/7, detail = "Save analysis into R file 6/6")
        rv$fileRData <- paste(rv$dirCalibration,paste0(basename,".RData"),sep='/')
        save(lda1,file=rv$fileRData)
        rv$exitStatusCal <- 1
        rv$messCal <- rv$fileRData
        progress$inc(7/7, detail = "End of calibration 6/6")

       ## sauvegarde des classes
        rv$outClassesTXT <- paste(rv$dirCalibration,paste0(basename,"_classes.txt"),sep='/') ## output file csv
        rv$outClassesTable <- rbind(data.frame(class="background",subclass="background"),data.frame(class="limb",subclass="limb"),data.frame(class="lesion",subclass=c("lesion","lesion2")))
        write.table(rv$outClassesTable,rv$outClassesTXT,row.names=FALSE,quote=FALSE,sep='\t')

      }
      else{
        # print(paste("else inputdir",rv$datapath))
        errorMess <-tags$div("Error not find all sub-directories !!!!:",  tags$br(),
                           tags$ul(
                             tags$li(paste("limb: ", listdirCalibration$dirlimb)),
                             tags$li(paste("background: ", listdirCalibration$dirBackground)),
                             tags$li(paste("lesion: ", listdirCalibration$dirLesion))
                           )
        )
        rv$exitStatusCal <- 0
        rv$messCal <- NULL
        rv$errCal <- errorMess

      }
      hide(id = "loading-content", anim = TRUE, animType = "fade")
    }
  }
)

#### Output when run click ####

output$codeAna <- renderText({
  rv$exitStatusCal
})

output$mess <- renderText({
  rv$messCal
})
output$err <- renderPrint({
  rv$errCal
})

output$img <- renderImage({
  if (rv$exitStatusCal == 0 || is.null(rv$plotFileCalibration)){
    return(list(src ="",
         alt = "plot img"))
  }
  else{
    # Return a list containing the filename
    return(list(src = rv$plotFileCalibration,
         width = 400,
         height = 400,
         filetype = "image/jpeg",
         alt = "plot img"))
  }
}, deleteFile = FALSE)

output$table <- renderTable({
  rv$outCalibrationTable

},striped = TRUE, bordered = TRUE,
align = 'c',
rownames = TRUE)


outputOptions(output, "codeAna", suspendWhenHidden = FALSE)
