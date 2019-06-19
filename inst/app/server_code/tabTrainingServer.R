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
## CODE serveur
###############################################
#### Training Directory path ####

# option to load directory path bottom for Training folder
shinyDirChoose(
  input,
  'dirTraining',
  filetypes = c('', 'txt', 'Rdata', "jpeg", "png", "csv", "*"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)


# when click to bottom update path
observeEvent(
  input$dirTraining,{
    if (!is.integer(input$dirTraining))
    {
      # initialize path
      rv$dirTraining <- normalizePath(parseDirPath(allVolumesAvail, input$dirTraining))
      rv$inputMethod <- input$inputMethod
      rv$inputColorModel <- input$inputColorModel
      # return to UI path selected for Training
      output$dirTraining <- renderText({
        rv$dirTraining
      })
      # if reload after first Training, reset value
      rv$exitStatusCal <- -1
      rv$messCal <- NULL
      rv$errCal <- NULL
      rv$plotALL <- TRUE

      # test if all subfolder mandatory
      listdirTraining <- existDirTraining(rv$dirTraining)

      # if all subfolder exist run analysis
      if(listdirTraining$dirlimb == TRUE && listdirTraining$dirBackground == TRUE && listdirTraining$dirLesion == TRUE){

        # hide app
        show(id = "loading-content")
        # add progress bar
#        progress <- shiny::Progress$new()
#        on.exit(progress$close())
#        progress$set(message = 'Making Training, please wait\n', value = 0)
#        progress$inc(1/7, detail = paste0("Start training on folder: ",rv$dirTraining, " 1/6"))

        ###########################
        # call function Training
        ###########################

        source("../../R/training_functions_V6.r")
        results <- training(rv$dirTraining,method=rv$inputMethod, transform=NULL, colormodel=rv$inputColorModel, mode = "GUI")
        rv$outTrainingTable <- results$tableTrain
        rv$errorRate <- results$errorRate

        ## name common to the 3 output files, identical to the name of the directory
        rv$basename <- tail(strsplit(rv$dirTraining,'/')[[1]],1)

        ## graph of groups in the discriminant plane
        rv$plotFileTraining1_2 <- paste(rv$dirTraining,paste0(rv$basename,"1_2.jpeg"),sep='/') ## output file jpeg
        rv$plotFileTraining1_3 <- paste(rv$dirTraining,paste0(rv$basename,"1_3.jpeg"),sep='/') ## output file jpeg
        rv$plotFileTraining2_3 <- paste(rv$dirTraining,paste0(rv$basename,"2_3.jpeg"),sep='/') ## output file jpeg

        rv$fileRData <- paste(rv$dirTraining,paste0(rv$basename,".RData"),sep='/')
        rv$exitStatusCal <- 1
        rv$messCal <- rv$fileRData

      }
      else{
        # print(paste("else inputdir",rv$datapath))
        errorMess <-tags$div("Error not find all sub-folders !!!!:",  tags$br(),
                           tags$ul(
                             tags$li(paste("limb: ", listdirTraining$dirlimb)),
                             tags$li(paste("background: ", listdirTraining$dirBackground)),
                             tags$li(paste("lesion: ", listdirTraining$dirLesion))
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

output$plotALL <- renderText({
  if (isTRUE(rv$plotALL)) 1
  else 0
})

output$img1_2 <- renderImage({
  if (rv$exitStatusCal == 0 || is.null(rv$plotFileTraining1_2)){
    return(list(src ="",
         alt = ""))
  }
  else{
    # Return a list containing the filename
    return(list(src = rv$plotFileTraining1_2,
         width = 800,
         height = 800,
         filetype = "image/jpeg",
         alt = ""))
  }
}, deleteFile = FALSE)

output$img1_3 <- renderImage({
  if (rv$exitStatusCal == 0 || is.null(rv$plotFileTraining1_3)){
    return(list(src ="",
         alt = ""))
  }
  else{
    # Return a list containing the filename
    return(list(src = rv$plotFileTraining1_3,
         width = 800,
         height = 800,
         filetype = "image/jpeg",
         alt = ""))
  }
}, deleteFile = FALSE)

output$img2_3 <- renderImage({
  if (rv$exitStatusCal == 0 || is.null(rv$plotFileTraining2_3)){
    return(list(src ="",
         alt = ""))
  }
  else{
    # Return a list containing the filename
    return(list(src = rv$plotFileTraining2_3,
         width = 800,
         height = 800,
         filetype = "image/jpeg",
         alt = ""))
  }
}, deleteFile = FALSE)

output$table <- renderTable({
  rv$outTrainingTable

},striped = TRUE, bordered = TRUE,
align = 'c',
rownames = TRUE)

output$table2 <- DT::renderDataTable({

  if (is.null(rv$outTrainingTable)) return(NULL)
  df <- as.data.frame(rv$outTrainingTable, stringAsFactors = FALSE, rownames = TRUE)
  brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}

  DT::datatable(data = df,
                  caption = 'Table Training: The values are the number of pixels.Défine class on row versus predict on columns. Source: LeAFtool',
                  class = 'dt-center compact cell-border',
                  options = list(
                                  columnDefs = list(list(width = '20%', targets = seq(0,length(colnames(df)),1))),
                                  dom = 't',
                                  style = "none",
                                  ordering=F,
                                  selection="none"
                                )
#                  extensions = c('Responsive')

  ) %>%
  formatStyle(
    0, target = 'cell', fontWeight = 'bold'
  ) %>%
   formatStyle(
    names(df), backgroundColor = styleInterval(brks, clrs)
  )

#  formatStyle(0, fontWeight = styleEqual(3, "bold"))

})






# view large plot
observeEvent(input$img1_2_zoom_cal,
             {
               addResourcePath("Training",rv$dirTraining) # Images are located outside shiny App
               showModal(      # Information Dialog Box
                 modalDialog(
                   title = "Output Training",
                   size = "l",
                   easyClose = TRUE,
                   img(src=paste0('Training/',rv$basename,"1_2.jpeg"), height = "100%")
                )
               )
             })
observeEvent(input$img1_3_zoom_cal,
             {
               addResourcePath("Training",rv$dirTraining) # Images are located outside shiny App
               showModal(      # Information Dialog Box
                 modalDialog(
                   title = "Output Training",
                   size = "l",
                   easyClose = TRUE,
                   img(src=paste0('Training/',rv$basename,"1_3.jpeg"), height = "100%")
                )
               )
             })
observeEvent(input$img2_3_zoom_cal,
             {
               addResourcePath("Training",rv$dirTraining) # Images are located outside shiny App
               showModal(      # Information Dialog Box
                 modalDialog(
                   title = "Output Training",
                   size = "l",
                   easyClose = TRUE,
                   img(src=paste0('Training/',rv$basename,"2_3.jpeg"), height = "100%")
                )
               )
             })


outputOptions(output, "codeAna", suspendWhenHidden = FALSE)
outputOptions(output, "plotALL", suspendWhenHidden = FALSE)
