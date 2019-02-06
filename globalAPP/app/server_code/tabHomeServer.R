observeEvent(input$show,
             {
               showModal(      # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output file with count lesions",
                   size = "l",
                   easyClose = TRUE,
                   img(src= "exemple.jpeg", height='100%')
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
                   img(src= "learning.jpeg", height='100%')
                )
               )
             })
