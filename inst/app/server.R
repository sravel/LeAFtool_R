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

# Add specific option for run shiny
# options(shiny.port = 3838)
# options(shiny.host = "194.254.138.139")

# Create a file logger:
clearLoggers() # reset log file
rv$logfilename <- tempfile()
#isolate(addDefaultFileLogger(rv$logfilename))
logInfo("Hello to LeAFtool !!!!")


############################################
## writing server function
############################################

shinyServer(function(input, output, session) {

  observe_helpers() # active help icon

  # Load functions for tab Training
  source(file.path("server_code", "tabTrainingServer.R"), local = TRUE)$value

  # Load functions for tab analysis
  source(file.path("server_code", "tabAnalysisServer.R"), local = TRUE)$value

  # Load functions for tab Home
  source(file.path("server_code", "tabHomeServer.R"), local = TRUE)$value

  # Load functions for tab Edit
  source(file.path("server_code", "tabEditServer.R"), local = TRUE)$value

  ## LOG FILE PRINT
  eventLog <- reactivePoll(4000, session,
    # This function returns the time that rv$logfilename was last modified
    checkFunc = function() {
      if (file.exists(rv$logfilename))
        file.info(rv$logfilename)$mtime[1]
      else
        ""
    },
    # This function returns the content of log_file
    valueFunc = function() {
      linesLog <- readLines(rv$logfilename)
      rows <- strsplit(linesLog, "\t")
      malformed <- sapply(rows, function(x) length(x) != 6)
      rows <- rows[!malformed]
      if (!length(rows)) return(NULL)
      eventLog <- data.frame(Timestamp = as.Date(sapply(rows, function(x) x[1])),
                           Thread = sapply(rows, function(x) x[2]),
                           Level = sapply(rows, function(x) x[3]),
                           Package = sapply(rows, function(x) x[4]),
                           Function = sapply(rows, function(x) x[5]),
                           Message = sapply(rows, function(x) x[6]))
      rv$levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
      updateSelectInput(session, "level", label = "Level", choices = rv$level, selected = "INFO")

      threads <- as.character(unique(eventLog$Thread))
      threads <- threads[order(threads)]
      rv$threads <- c("All", threads)
      updateSelectInput(session, "thread", label = "Thread", choices = rv$threads, selected = "All")

      package <- as.character(unique(eventLog$Package))
      rv$package <- c("All", package)
      return(eventLog)
    }
  )

  observe({
    updateSelectInput(session, "level", label = "Level", choices = rv$levels, selected = "INFO")
  })
  observe({
    updateSelectInput(session, "thread", label = "Thread", choices = rv$threads, selected = "All")
  })
  observe({
    updateSelectInput(session, "package", label = "package", choices = rv$package, selected = "All")
  })

  output$logTable <- renderDataTable({
    eventLog <- eventLog()
    if (is.null(eventLog)) return(NULL)

    colorLevels <- c("TRACE", "DEBUG", "WARN", "ERROR", "FATAL")
    colors <- c(rgb(0.8, 0.9, 1), rgb(0.8, 1, 0.8), rgb(1.0, 0.88, 0.7), rgb(1, 0.84, 0.8), rgb(1, 0.8, 0.94))

    visibleLevels <- rv$levels[which(rv$levels == input$level):length(rv$levels)]

    idx <- eventLog$Level %in% visibleLevels
    if (input$thread != "All") {
      idx <- idx & eventLog$Thread == input$thread
    }
    if (input$package != "All") {
      idx <- idx & eventLog$Package == input$package
    }
    options = list(pageLength = 10000,
                   searching = TRUE,
                   lengthChange = FALSE,
                   ordering = FALSE,
                   paging = FALSE,
                   scrollY = '75vh')
    selection = list(mode = "single", target = "row")
    table <- datatable(eventLog[idx, ],
                       options = options,
                       selection = selection,
                       rownames = FALSE,
                       class = "stripe nowrap compact")
    table <- formatStyle(table = table,
                         columns = 3,
                         target = "row",
                         backgroundColor = styleEqual(colorLevels, colors))
    return(table)
  })

  output$logFilePath <- renderPrint ({normalizePath(rv$logfilename)})

  session$onSessionEnded( function() {
    clearLoggers() # reset log file
    isolate(
      if (rv$parallelMode == TRUE){
        try(parallel::stopCluster(cl), silent = TRUE) # Close cluster mode
        closeAllConnections(); # for kill all process, use to add button for stop work
        registerDoSEQ()
      }
    )
    stopApp()
  })

#  observe({
#    # debug output to show the listN content.
#    output$debug <- renderPrint({
#      rv %>% reactiveValuesToList
#    })

#  })
})
