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

observeEvent(input$showInTraining,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of input folder for training",
                   size = "l",
                   easyClose = TRUE,
                     img(src = "trainingFolder.png", class='img-responsive')
                 )
               )
             })

observeEvent(input$showTraining,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of output for training",
                   size = "l",
                   easyClose = TRUE,
                     img(src = "training.jpeg", class='img-responsive')
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
observeEvent(input$showSplit,
             {
               showModal(
                 # Information Dialog Box
                 modalDialog(
                   title = "Exemple of Split",
                   size = "l",
                   easyClose = TRUE,
                     img(src = "splitExemple.png", class='img-responsive')
                 )
               )
             })
