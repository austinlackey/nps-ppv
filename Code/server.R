# Libraries
library(tidyverse)
library(openxlsx)
library(tcltk)
library(readxl)
library(plotly)
library(scales)
library(stringr)
library(anytime)
library(patchwork)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(formattable)
library(lubridate)
data <- NULL # Global Data Initialization

SERVER = shinyServer(function(input, output, session) {
  #!#########################################################################
  #!###################      Code for the table page     ####################
  #!#########################################################################

  #* Initializing the table
  values <- reactiveValues()
  values$DT <- data.frame(Date = character(),
                          Park = character(),
                          Entrance = character(),
                          Time = character(),
                          One = character(),
                          Two = character(),
                          Three = character(),
                          Four = character(),
                          Five = character(),
                          Six = character(),
                          SevenPlus = character(),
                          TOTAL = character())
  #* Render the rows on the page
  output$table <- renderTable({
    return(values$DT)
  })
  #* Addding a row to the table page
  newEntry <- observeEvent(input$addrow, {
    line <- c(convertDate(input$dateIn), input$parkNameIn, input$entranceIn, formatTime(input$timeIn), as.integer(input$vehOneIn1) + as.integer(input$vehOneIn2) + as.integer(input$vehOneIn3),
                 as.integer(input$vehTwoIn1) + as.integer(input$vehTwoIn2) + as.integer(input$vehTwoIn3), as.integer(input$vehThreeIn1) + as.integer(input$vehThreeIn2) + as.integer(input$vehThreeIn3), 
                 as.integer(input$vehFourIn1) + as.integer(input$vehFourIn2) + as.integer(input$vehFourIn3), as.integer(input$vehFiveIn1) + as.integer(input$vehFiveIn2) + as.integer(input$vehFiveIn3),
                 as.integer(input$vehSixIn1) + as.integer(input$vehSixIn2) + as.integer(input$vehSixIn3), paste(input$vehSevenIn1, input$vehSevenIn2, input$vehSevenIn3, sep=","))
    line[is.na(line)] <- 0
    singleVals <- as.integer(line[5:10])
    sevenVals <- strsplit(gsub(" ", "", line[11]), ",")[[1]]
    sevenVals <- sevenVals[sevenVals != "0"]
    sevenVals <- sevenVals[sevenVals != ""]
    line[11] <- paste(sevenVals, collapse = ",")
    newLine <- append(line, (sum(singleVals) + length(sevenVals)))
    values$DT <- rbind(newLine, values$DT)
    colnames(values$DT) <- c("Date", "Park", "Entrance", "Time", "One", "Two", "Three", "Four", "Five", "Six", "SevenPlus", "TOTAL")
  })
  #* Deleting a row to the table page
  newEntry <- observeEvent(input$revrow, {
    deleteLine <- values$DT[-1, ]
    values$DT <- deleteLine
  })
  #* Sumbit dataframe to be converted to an excel file
  newEntry <- observeEvent(input$submit, {
    values2 <- values$DT #Grab the dataframe
    runTally(values2, input$parkNameIn, input$checkbox) #Run the tally funciton with dataframe
  })
  observeEvent(input$resetData, {
    #? Swappable code to randomize the input values (Used for testing) 
    # reset("lowerData")
    updateTextInput(session, "timeIn", NULL, c("AM", "PM")[round(runif(1, 1, 2))])
    updateTextInput(session, "vehOneIn1", NULL, as.character(round(runif(n=1, min=0, max=15))))
    updateTextInput(session, "vehTwoIn1", NULL, as.character(round(runif(n=1, min=0, max=15))))
    updateTextInput(session, "vehThreeIn1", NULL, as.character(round(runif(n=1, min=0, max=15))))
    updateTextInput(session, "vehFourIn1", NULL, as.character(round(runif(n=1, min=0, max=15))))
    updateTextInput(session, "vehFiveIn1", NULL, as.character(round(runif(n=1, min=0, max=15))))
    updateTextInput(session, "vehSixIn1", NULL, as.character(round(runif(n=1, min=0, max=15))))
    updateTextInput(session, "vehSevenIn1", NULL, paste(as.character(round(runif(n=runif(n=1, min=1, max=5), min=7, max=25))), collapse=","))

    # updateTextInput(session, "timeIn", NULL, "")
    # updateTextInput(session, "vehOneIn1", NULL, "0")
    # updateTextInput(session, "vehTwoIn1", NULL, "0")
    # updateTextInput(session, "vehThreeIn1", NULL, "0")
    # updateTextInput(session, "vehFourIn1", NULL, "0")
    # updateTextInput(session, "vehFiveIn1", NULL, "0")
    # updateTextInput(session, "vehSixIn1", NULL, "0")
    # updateTextInput(session, "vehSevenIn1", NULL, "0")
    updateTextInput(session, "vehOneIn2", NULL, "0")
    updateTextInput(session, "vehTwoIn2", NULL, "0")
    updateTextInput(session, "vehThreeIn2", NULL, "0")
    updateTextInput(session, "vehFourIn2", NULL, "0")
    updateTextInput(session, "vehFiveIn2", NULL, "0")
    updateTextInput(session, "vehSixIn2", NULL, "0")
    updateTextInput(session, "vehSevenIn2", NULL, "0")
    updateTextInput(session, "vehOneIn3", NULL, "0")
    updateTextInput(session, "vehTwoIn3", NULL, "0")
    updateTextInput(session, "vehThreeIn3", NULL, "0")
    updateTextInput(session, "vehFourIn3", NULL, "0")
    updateTextInput(session, "vehFiveIn3", NULL, "0")
    updateTextInput(session, "vehSixIn3", NULL, "0")
    updateTextInput(session, "vehSevenIn3", NULL, "0")
  })

  #!##########################################################################
  #!###################      Code for the charts page     ####################
  #!##########################################################################

  #* Load in an excel spreadsheet and calculate the tables and figures
  observeEvent(input$loadData, {
    tryCatch({
          # Code to run
          chart_data_path <- tk_choose.files(caption = "Select excel spreadsheet:")
          edaData$sheet_one_data <- formatSheet1(read_excel(chart_data_path, sheet=1))
          edaData$sheet_two_data <- formatSheet2(read_excel(chart_data_path, sheet=2))
          edaData$sheet_one_data_table <- edaData$sheet_one_data
          edaData$sheet_two_data_table <- edaData$sheet_two_data
          colnames(edaData$sheet_one_data_table) <- c("Sheet Number", "Number of Observations", "Entrance", "Date", "Month", "Day of Week", "Time of Day", "Month Number")
          colnames(edaData$sheet_two_data_table) <- c("Observation Number", "Entrance", "Month", "Date", "Day of Week", "Time of Day", "Occupants", "Month Number")
          edaData$table_eda <- getTableEDA(edaData$sheet_one_data, edaData$sheet_two_data)
          edaData$table_month <- getTableMonth(edaData$sheet_one_data, edaData$sheet_two_data)
          edaData$fig_hist <- getFigHistogram(edaData$sheet_two_data, input$monthSelector)$plot
          edaData$fig_hist_data <- getFigHistogram(edaData$sheet_two_data, input$monthSelector)$data
          edaData$table_tod <- getTableTOD(edaData$sheet_one_data)
          edaData$table_dow <- getTableDOW(edaData$sheet_one_data)
          edaData$fig_ppv <- getFigPPV(edaData$sheet_two_data)$plot
          edaData$fig_ppv_data <- getFigPPV(edaData$sheet_two_data)$data %>% select(c("Month", "Mean"))
          showNotification("Successfully Imported Data.", type="message")
        },
        warning = function(w) {
          # Catch Warnings
          print(w)
          showNotification(paste("WARNING: ", w$message), type="warning")
        },
        error = function(e) {
          # Catch Errors
          print(e)
          showNotification(paste("ERROR: Issue with dataset. Use a dataset created by PPV App."), type="error")
        },
        finally = {
          # Code to always run
        }
        )
  })

  #* Code to update the data that used a month filter
  observeEvent(input$monthSelector, {
    if (is.null(edaData$fig_hist_data)) return()
    edaData$fig_hist <- getFigHistogram(edaData$sheet_two_data, input$monthSelector)$plot
    edaData$fig_hist_data <- getFigHistogram(edaData$sheet_two_data, input$monthSelector)$data
  })

  #!#######################################################################
  #!###################      Code for the eda page     ####################
  #!#######################################################################
  #* Initalizing the values
  edaData <- reactiveValues(
    sheet_one_data = NULL,
    sheet_two_data = NULL,
    sheet_one_data_table = NULL,
    sheet_two_data_table = NULL,
    table_eda = NULL,
    table_month = NULL,
    table_tod = NULL,
    table_dow = NULL,
    fig_hist = NULL,
    fig_hist_data = NULL,
    fig_ppv = NULL,
    fig_ppv_data = NULL
  )

  output$data_tallysheets <- DT::renderDataTable({ 
    if (is.null(edaData$sheet_one_data_table)) return()
    return(DT::datatable(edaData$sheet_one_data_table))
  })

  output$data_vehicles <- DT::renderDataTable({
    if (is.null(edaData$sheet_two_data_table)) return()
    return(DT::datatable(edaData$sheet_two_data_table))
  })

  output$vb_sheets_returned <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Sheets Returned",
          color = "purple"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[1]), "Sheets Returned", icon = icon("file"),
        color = "purple"
      )
    )
  })

  output$vb_sheets_with_zero <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Sheets with 0 observations",
          color = "red"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[2]), "Sheets with 0 observations", icon = icon("creative-commons-zero"),
        color = "red"
      )
    )
  })

  output$vb_vehicles <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Vehicles Observed",
          color = "green"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[3]), "Vehicles Observed", icon = icon("car"),
        color = "green"
      )
    )
  })

  output$vb_min <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Minimum Passengers",
          color = "orange"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[4]), "Minimum Passengers", icon = icon("arrows-down-to-line"),
        color = "orange"
      )
    )
  })

  output$vb_max <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Maximum Passengers",
          color = "orange"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[5]), "Maximum Passengers", icon = icon("arrows-up-to-line"),
        color = "orange"
      )
    )
  })

  output$vb_mean <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Mean Passengers",
          color = "yellow"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[6]), "Mean Passengers", icon = icon("down-left-and-up-right-to-center"),
        color = "yellow"
      )
    )
  })

  output$vb_median <- renderValueBox({
    if (is.null(edaData$table_eda)) {
      return(
        valueBox(
          paste0(""), "Median Passengers",
          color = "yellow"
        )
      )
    }
    return(
      valueBox(
        paste0(edaData$table_eda$col2[7]), "Median Passengers", icon = icon("down-left-and-up-right-to-center"),
        color = "yellow"
      )
    )
  })


  # output$table_eda <- renderTable({
  #   if (is.null(edaData$table_eda)) return()
  #   return(edaData$table_eda)
  # }, colnames=FALSE)

  output$table_month <- renderFormattable({
    if (is.null(edaData$table_month)) return()
    formattable(
      edaData$table_month,
      list('Number of Observations' = color_bar("lightblue"),
          'Persons Per Vehicle (PPV)' = color_tile(customGreen0, customGreen),
          'Number of Obs (%)' = color_tile(customBlue0, customBlue))
    )
  })

  output$table_tod <- renderFormattable({
    if (is.null(edaData$table_tod)) return()
    formattable(
      edaData$table_tod,
      list('Mean Number of Observed Vehicles' = color_tile(customBlue0, customBlue))
    )
  })

  output$table_dow <- renderFormattable({
    if (is.null(edaData$table_dow)) return()
    formattable(
      edaData$table_dow,
      list('Mean Number of Observed Vehicles' = color_tile(customBlue0, customBlue))
    )
  })

  output$figPPV <- renderPlotly({
    if (is.null(edaData$fig_ppv)) return()
    return(ggplotly(edaData$fig_ppv, tooltip="text"))
  })

  output$figPPVData <- renderFormattable({
    if (is.null(edaData$fig_ppv_data)) return()
    formattable(
      edaData$fig_ppv_data, align = c('l'), list('Mean' = color_tile(customBlue0, customBlue))
    )
  })

  output$figHist <- renderPlotly({
    if (is.null(edaData$fig_hist)) return()
    return(ggplotly(edaData$fig_hist, tooltip="text"))
  })

  output$figHistData <- renderFormattable({
    if (is.null(edaData$fig_hist_data)) return()
    formattable(
      edaData$fig_hist_data, align = c('l'), list()
    )
  })

  #!##########################################################################
  #!###################      Code for the custom page     ####################
  #!##########################################################################
  custom_Data <- reactiveValues(
    rangeData = NULL,
    fig_range_plot = NULL,
    filtered_data = NULL,
    ci_plot = NULL
  )

  output$plotRange <- renderPlotly({
    if (is.null(custom_Data$fig_range_plot)) return()
    return(ggplotly(custom_Data$fig_range_plot, height=400, width=700))
  })
  output$sliders <- renderUI({
    lapply(1:as.integer(input$numGroups), function(i) {
      textInput(paste("group", as.character(i)), h5(paste("Group", as.character(i), "Range")), value="1-4")
    })
  })
  output$sliders_text <- renderUI({
    lapply(1:as.integer(input$numGroups), function(i) {
      textInput(paste("text_group", as.character(i)), h5(paste("Group", as.character(i), "name")), value=paste("Group", as.character(i)))
    })
  })
  observeEvent(input$generateGroups, {
    ranges <- data.frame(ranges = character(), names = character())
    for (i in 1:as.integer(input$numGroups)) {
      ranges[nrow(ranges) + 1,] <- c(input[[paste("group", as.character(i))]], input[[paste("text_group", as.character(i))]])
    }
    custom_Data$rangeData <- checkRange(ranges)
    if (!(custom_Data$rangeData$logical)) {
      showNotification("ERROR: Invalid Month Range.", type="error")
    }
    else {
        custom_Data$fig_range_plot <- figRange(custom_Data$rangeData$ranges_plot)
        custom_Data$filtered_data <- applyRanges(edaData$sheet_two_data, custom_Data$rangeData$ranges)
        tryCatch({
          custom_Data$ci_plot <- poiReg(custom_Data$filtered_data)
          showNotification("Successfully Ran Poisson Regression", type="message")
        },
        warning = function(w) {
          print(w)
          showNotification(paste("WARNING: ", w$message), type="warning")
        },
        error = function(e) {
          print(e)
          showNotification(paste("ERROR: ", e$message), type="error")
        },
        finally = {

        }
        )
    }
  })
  output$custom_ci_plot <- renderPlotly({
    if (is.null(custom_Data$ci_plot)) return()
    return(ggplotly(custom_Data$ci_plot$plot, height=400, width=600))
  })
  output$custom_ci_table <- renderTable({
    if (is.null(custom_Data$ci_plot)) return()
    return(custom_Data$ci_plot$table)
  })

  filter_Data <- reactiveValues(
    data = NULL
  )
  observeEvent(input$filterSelector, {
    if (is.null(edaData$sheet_two_data)) return()
    filter_Data$data <- filterData(edaData$sheet_two_data, input$filterSelector)
    colnames(filter_Data$data) <- c("Entrance", "Count", "Min", "Max", "Mean")
  })
  output$filterTable <- renderTable({
    if (is.null(filter_Data$data)) return()
    return(filter_Data$data)
  })
})