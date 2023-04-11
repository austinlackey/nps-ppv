data <- NULL


SERVER = shinyServer(function(input, output, session) {
  #TABLE PAGE ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  #Render Row
  output$table <- renderTable({
    return(values$DT)
  })
  #Add row
  newEntry <- observeEvent(input$addrow, {
    line <- c(input$dateIn, input$parkNameIn, input$entranceIn, input$timeIn, as.integer(input$vehOneIn1) + as.integer(input$vehOneIn2),
                 as.integer(input$vehTwoIn1) + as.integer(input$vehTwoIn2), as.integer(input$vehThreeIn1) + as.integer(input$vehThreeIn2), 
                 as.integer(input$vehFourIn1) + as.integer(input$vehFourIn2), as.integer(input$vehFiveIn1) + as.integer(input$vehFiveIn2),
                 as.integer(input$vehSixIn1) + as.integer(input$vehSixIn2), paste(input$vehSevenIn1, input$vehSevenIn2, sep=","))
    singleVals <- as.integer(line[5:10])
    sevenVals <- strsplit(gsub(" ", "", line[11]), ",")[[1]]
    sevenVals <- sevenVals[sevenVals != "0"]
    sevenVals <- sevenVals[sevenVals != ""]
    line[11] <- paste(sevenVals, collapse = ",")
    newLine <- append(line, (sum(singleVals) + length(sevenVals)))
    values$DT <- rbind(values$DT, newLine)
    colnames(values$DT) <- c("Date", "Park", "Entrance", "Time", "One", "Two", "Three", "Four", "Five", "Six", "SevenPlus", "TOTAL")
  })
  #Delete row
  newEntry <- observeEvent(input$revrow, {
    deleteLine <- values$DT[-nrow(values$DT), ]
    values$DT <- deleteLine
  })
  #Sumbit dataframe
  newEntry <- observeEvent(input$submit, {
    values2 <- values$DT #Grab the dataframe
    runTally(values2, input$parkNameIn, input$checkbox) #Run the tally funciton with dataframe
  })
  observeEvent(input$resetData, {
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
  })

  #CHARTS PAGE ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  observeEvent(input$loadData, {
    chart_data_path <- tk_choose.files(caption = "Select excel spreadsheet:")
    eda_Data$sheet_one_data <- formatData(read_excel(chart_data_path, sheet=1))
    eda_Data$sheet_two_data <- formatData(read_excel(chart_data_path, sheet=2))
    eda_Data$eda_plot_one <- eda_PPV_Scatterplot(eda_Data$sheet_two_data)
    eda_Data$eda_plot_two <- eda_PPV_Bar_Plot(eda_Data$sheet_two_data, as.integer(input$monthSelector))
    eda_Data$entranceChoices <- getEntrances(eda_Data$sheet_two_data)
    filter_Data$data <- filterData(eda_Data$sheet_two_data, input$filterSelector)
    colnames(filter_Data$data) <- c("Entrance", "Count", "Min", "Max", "Mean")
  })
  #EDA TAB ------------------------------------------------------------------------------------------------------------------------
  eda_Data <- reactiveValues(
    sheet_one_data = NULL,
    sheet_two_data = NULL,
    eda_plot_one = NULL,
    eda_plot_two = NULL,
    entranceChoices = NULL
  )

  observeEvent(input$monthSelector, {
    if (is.null(eda_Data$eda_plot_two)) return()
    eda_Data$eda_plot_two <- eda_PPV_Bar_Plot(eda_Data$sheet_two_data, as.integer(input$monthSelector))
  })
  output$plot1_table <- renderTable({
    if (is.null(eda_Data$eda_plot_one)) return()
    return(data.frame(Month = eda_Data$eda_plot_one$data$monthNum, PPV = eda_Data$eda_plot_one$data$ppv))
  })
  output$plot2_table <- renderTable({
    if (is.null(eda_Data$eda_plot_two)) return()
    return(data.frame(PPV = eda_Data$eda_plot_two$table$num, Count = eda_Data$eda_plot_two$table$count))
  })
  output$plot1 <- renderPlotly({
    if (is.null(eda_Data$eda_plot_one)) return()
    return(ggplotly(eda_Data$eda_plot_one$plot, height=600, width=800))
  })
  output$plot2 <- renderPlotly({
    if (is.null(eda_Data$eda_plot_two)) return()
    return(ggplotly(eda_Data$eda_plot_two$plot, height=600, width=800))
  })
  output$plot1_stat_table <- renderTable({
    if (is.null(eda_Data$eda_plot_one)) return()
    return(
      data.frame(Stat = c("min", "max", "median", "mean"),
                PPV = c(min(eda_Data$sheet_two_data$occupants), 
                          max(eda_Data$sheet_two_data$occupants), 
                          median(eda_Data$sheet_two_data$occupants), 
                          mean(eda_Data$sheet_two_data$occupants)))
    )
  })
  output$plot2_stat_table <- renderTable({
    if (is.null(eda_Data$eda_plot_two)) return()
    if (input$monthSelector == "-1") {
      tempFrame <- eda_Data$eda_plot_two$data
    }
    else {
      tempFrame <- eda_Data$eda_plot_two$data %>% filter(monthNum == as.integer(input$monthSelector))
    }
    return(
      data.frame(Stat = c("min", "max", "median", "mean"),
            PPV = c(min(tempFrame$occupants), 
                      max(tempFrame$occupants), 
                      median(tempFrame$occupants), 
                      mean(tempFrame$occupants)))
    )
  })



  #CUSTOM TAB ------------------------------------------------------------------------------------------------------------------------
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
        custom_Data$filtered_data <- applyRanges(eda_Data$sheet_two_data, custom_Data$rangeData$ranges)
        custom_Data$ci_plot <- poiReg(custom_Data$filtered_data)
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
    if (is.null(eda_Data$sheet_two_data)) return()
    filter_Data$data <- filterData(eda_Data$sheet_two_data, input$filterSelector)
    colnames(filter_Data$data) <- c("Entrance", "Count", "Min", "Max", "Mean")
  })
  output$filterTable <- renderTable({
    if (is.null(filter_Data$data)) return()
    return(filter_Data$data)
  })
})