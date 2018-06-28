observeEvent(input$show,
             {
               showModal(      # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output file with count lesions",
                   size = "l",
                   easyClose = TRUE,
                   img(src= "exemple.jpeg",width='100%',height='100%')
                )
               )
             })

observeEvent(input$showCalibration,
             {
               showModal(      # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output file for calibration",
                   size = "l",
                   easyClose = TRUE,
                   img(src= "calibration.png",width='100%',height='100%')
                )
               )
             })