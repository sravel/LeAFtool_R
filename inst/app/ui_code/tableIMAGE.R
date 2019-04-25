library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)


datapathAnalysisTEST <- "~/Bayer/AnalyseImagesV4/Exemple1/Images/"
datapathOutAnalysisTEST <- "~/Bayer/AnalyseImagesV4/Exemple1/"

addResourcePath("Original",datapathAnalysisTEST) # Images are located outside shiny App
addResourcePath("LesionColor",datapathOutAnalysisTEST) # Images are located outside shiny App

LeafNames <- list.files(datapathAnalysisTEST, full.names=FALSE)
LeafNames2 <- list.files(datapathOutAnalysisTEST, full.names=FALSE, pattern = "*.png")
LeafTable <- data.frame(LeafNames,LeafNames2)
LeafTable<- within(LeafTable, thumbnail <- paste0("<img src='","Original/",LeafTable$LeafNames,"' height='60'></img>"))
LeafTable<- within(LeafTable, thumbnail2 <- paste0("<img src='","LesionColor/",LeafTable$LeafNames2 ,"' height='60'></img>"))

# print(LeafTable)
# View(LeafTable)

ui <-shinyUI( dashboardPage(
  dashboardHeader(title = span(tagList(icon("image"), "Example"))),
  dashboardSidebar(),
  dashboardBody(
    
    div(style="display:inline-block",uiOutput("infoButton")),
    
    DT::dataTableOutput("table2")
    
  )
))


server <- shinyServer(function(input, output) {
  
  output$table2<-DT::renderDataTable({
    
    responseDataFilter2 <- LeafTable[,c(1,2,3,4)]
    
    displayableData<-DT::datatable(data = as.data.frame(responseDataFilter2, stringAsFactors = FALSE, row.names = NULL),
                                   
                                   escape=FALSE,selection="single",rownames=FALSE,colnames=c("Original","LesionColor","Name","toto"),
                                   
                                   callback = JS("table.on('dblclick.dt', 'td', function() {
                                                 var row=table.cell(this).index().row;
                                                 Shiny.onInputChange('rows_home',[row, Math.random()])});
                                                 table.on('click.dt', 'td', function() {
                                                 var k=table.cell(this).index().row;
                                                 if(table.rows('.selected').indexes().toArray()!= '' && table.rows('.selected').indexes().toArray() == k){k=-1;}
                                                 Shiny.onInputChange('rows_up_home',[k, Math.random()]);
                                                 Shiny.onInputChange('row_path', table.rows(this).data().toArray());
                                                 });"),

                               options = list(

                                 paging=TRUE,searching = TRUE,ordering=TRUE,scrollY = 750,scrollCollapse=TRUE,server = TRUE

                               ))

})

output$infoButton = renderUI({
s = input$table2_rows_selected # Row number of selected row 
if (length(s)!= 0) {
  tagList(
    actionButton("info", "",icon("info-circle"),style="color:rgb(57,156,8);border-color:rgb(255,255,255)"),

    # Information Dialog Box
    bsModal("ObjectInfo", LeafTable[s,c(2)], "info", size = "large", # Enables Pop up Screen

            img(src= paste0("Original/",LeafTable$LeafNames[s]),width='44%',height='44%'), img(src= paste0("LesionColor/",LeafTable$LeafNames2[s]),width='44%',height='44%')
            

    )
  )

}
})
})



shinyApp(ui = ui , server = server)
