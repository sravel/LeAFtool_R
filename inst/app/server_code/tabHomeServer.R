observeEvent(input$show,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output file with colors lesions",
                   size = "l",
                   easyClose = TRUE,
                     img(src = "exemple.jpeg", class='img-responsive')
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
                     img(src = "calibrationFolder.png", class='img-responsive')
                 )
               )
             })

observeEvent(input$showCalibration,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output for calibration",
                   size = "l",
                   easyClose = TRUE,
                     img(src = "learning.jpeg", class='img-responsive')
                 )
               )
             })
observeEvent(input$showEdit,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of Edit mode",
                   size = "l",
                   easyClose = TRUE,
                     img(src = "editMode.png", class='img-responsive')
                 )
               )
             })
