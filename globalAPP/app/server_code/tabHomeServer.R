observeEvent(input$show,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output file with count lesions",
                   size = "l",
                   easyClose = TRUE,

                   fluidRow(column(
                     width = 12,
                     offset = 0,
                     img(src = "exemple.jpeg", height = '100%')
                   ))

                 )
               )
             })

observeEvent(input$showCalibration,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output file for calibration",
                   size = "l",
                   easyClose = TRUE,

                   fluidRow(column(
                     width = 12,
                     offset = 0,
                     img(src = "learning.jpeg", height = '100%')
                   ))
                 )
               )
             })
observeEvent(input$showInCalibration,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of input folder for calibration",
                   size = "l",
                   easyClose = TRUE,

                   fluidRow(column(
                     width = 12,
                     offset = 0,
                     img(src = "calibrationFolder.png", height = '100%')
                   ))
                 )
               )
             })
