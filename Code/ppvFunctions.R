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

#!#################################################################
#!###################      Code for Colors     ####################
#!#################################################################
palette <- hue_pal()(16)
month.name <- c("January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
g.code <- c("#FFFFFF", "#EEEEEE", "#DDDDDD", "#CCCCCC", "#BBBBBB", "#AAAAAA", "#999999", "#888888", "#777777", "#666666", "#555555", "#444444", "#333333", "#222222", "#111111", "#000000")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customBlue0 = "#Dfecf7"
customBlue = "#71aac9"

opts = c("One", "Two", "Three")

# show_col(palette)
# show_col(g.code)
underTheme <- theme(panel.background = element_rect(fill = g.code[3], size = 2, linetype = "solid"),
                    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
                    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))

file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path, 
               open = "w"), 
          silent = TRUE
      )
    )
  )
}
updateDataTypes <- function(df, sheet) {
  if (sheet == 2) {
    df$Obs_Num <- as.numeric(df$Obs_Num)
    df$Occupants <- as.numeric(df$Occupants)
  }
  else {
    df$Sheet_Num <- as.numeric(df$Sheet_Num)
    df$Num_of_Observations <- as.numeric(df$Num_of_Observations)
    df$Time_of_Day <- as.character(df$Time_of_Day)
  }
  df$Entrance <- as.character(df$Entrance)
  df$Month <- as.character(df$Month)
  df$Date <- as.Date(df$Date, format="%m/%d/%y")
  df$Day_of_Week <- as.character(df$Day_of_Week)
  return(df)
}
#!#########################################################################
#!###################      Code for the table page     ####################
#!#########################################################################
#* Function to output the excel file
runTally <- function(data, parkCode, appendingSheet) { # Takes in the data, the parkCode (XXXX), and a boolean for appending
  appendingFilePath <- NULL # Initalizing the boolean to NULL
  if (appendingSheet) { # If we are adding to existing data, grab the file location
    appendingFilePath <- tk_choose.files(caption = "Select the excel document that you would like to append this data to:")
    if (identical(appendingFilePath, character(0))) {
      appendingSheet <- FALSE
      print("No File Selected. Please Try again or de-select the appending sheet option.")
      return(NULL)
    }
  }
  num_tally_sheets <- nrow (data) # Number of tally sheets
  tally_frame <- data #Copying the data into a new dataframe
  colnames(tally_frame) <- c("DATE", "PARK", "ENTRANCE", "TIME", "X_ONE", "X_TWO", "X_THREE", "X_FOUR", "X_FIVE", "X_SIX", "X_SEVEN_PLUS", "TOTAL") # Resetting the Column Names
  tally_frame <- subset(tally_frame, select = -c(TOTAL)) # Getting rid of the TOTAL Column
  rownames(tally_frame) <- seq(1:length(tally_frame$X_ONE)) # Resetting the index's

  tally_frame <- tally_frame %>% mutate_at(c("X_ONE", "X_TWO", "X_THREE", "X_FOUR", "X_FIVE", "X_SIX"), as.numeric) # Setting the numeric columns to numeric
  tally_frame <- tally_frame %>% mutate(X_SEVEN_PLUS = ifelse(X_SEVEN_PLUS == 0, 0, strsplit(X_SEVEN_PLUS, ","))) # Formatting the column wth 7+ vehicles

  # Creating the First Sheet
  first_sheet <- data.frame(Sheet_Num = numeric(),
                            Num_of_Observations = numeric(),
                            Entrance = character(),
                            Date = character(),
                            Month = character(),
                            Day_of_Week = character(),
                            Time_of_Day = character())
  for (i in 1:nrow(tally_frame)) { # Going through each tally sheet
    single_val_sum <- sum(as.numeric(tally_frame[i, c("X_ONE", "X_TWO", "X_THREE", "X_FOUR", "X_FIVE", "X_SIX")])) # Summing up the columns 1-6
    seven_plus_count <- length(tally_frame[i, c("X_SEVEN_PLUS")][[1]]) # Counting the number of vehicles that are 7+
    row_sum = single_val_sum + seven_plus_count # Summing both of these counts
    # Creating a row for the first sheet
    row <- c(i, row_sum, tally_frame$ENTRANCE[i], tally_frame$DATE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%B"), format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%A"), tally_frame$TIME[i])
    first_sheet <- rbind(first_sheet, row) # Adding the row to the first sheet
  }
  # Resetting the column names for the sheet
  colnames(first_sheet) <- c("Sheet_Num", "Num_of_Observations", "Entrance", "Date", "Month", "Day_of_Week", "Time_of_Day")

  # Creating the Second Sheet
  second_sheet <- data.frame(Obs_Num = numeric(),
                            Entrance = character(),
                            Month = character(),
                            Date = character(),
                            Day_of_Week = character(),
                            Time_of_Day = character(),
                            Occupants = character())
  for (i in 1:nrow(tally_frame)) { # Going through each tally sheet
    single_vals <- as.numeric(tally_frame[i, c("X_ONE", "X_TWO", "X_THREE", "X_FOUR", "X_FIVE", "X_SIX")])
    for (num_pass in 1:6) { # For each column
      if (single_vals[num_pass] != 0) {  # If the number is not 0
        for (count in 1:single_vals[num_pass]) { # Iterate that many of times
          row <- c(i, tally_frame$ENTRANCE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%B"), tally_frame$DATE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%A"), tally_frame$TIME[i], num_pass)
          second_sheet <- rbind(second_sheet, row)
        }
      }
    }
    seven_vals <- as.numeric(tally_frame[i, c("X_SEVEN_PLUS")][[1]]) # 7+ values
    if (length(seven_vals) != 0) {
      for (index in 1:length(seven_vals)) { # For each car add the row
        row <- c(i, tally_frame$ENTRANCE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%B"), tally_frame$DATE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%A"), tally_frame$TIME[i], seven_vals[index])
        second_sheet <- rbind(second_sheet, row)
      }
    }
  }
  colnames(second_sheet) <- c("Obs_Num", "Entrance", "Month", "Date", "Day_of_Week", "Time_of_Day", "Occupants") # Resetting the column names for the sheet
  second_sheet$Obs_Num <- seq(1:nrow(second_sheet))
  # if (appendingSheet & file.opened(appendingFilePath)) {
  #   print("ERROR: File path selected is currently open. Please close the file and try again!")
  #   print(paste("FILEPATH: ", appendingFilePath))
  # }
  first_sheet <- updateDataTypes(first_sheet, 1)
  second_sheet <- updateDataTypes(second_sheet, 2)
  if (appendingSheet) {
    print(appendingFilePath)
    orig_sheet_one <- read_xlsx(paste(appendingFilePath), 1)
    orig_sheet_two <- read_xlsx(paste(appendingFilePath), 2)
    orig_sheet_one <- updateDataTypes(orig_sheet_one, 1)
    orig_sheet_two <- updateDataTypes(orig_sheet_two, 2)
    updated_sheet_one <- bind_rows(orig_sheet_one, first_sheet)
    updated_sheet_two <- bind_rows(orig_sheet_two, second_sheet)
    updated_sheet_one$Num_of_Observations <- as.numeric(updated_sheet_one$Num_of_Observations)
    updated_sheet_two$Occupants <- as.numeric(updated_sheet_two$Occupants)
    updated_sheet_one <- updated_sheet_one[order(updated_sheet_one$Date), ]
    updated_sheet_two <- updated_sheet_two[order(updated_sheet_two$Date, updated_sheet_two$Occupants), ]
    updated_sheet_one$Sheet_Num <- seq(1:nrow(updated_sheet_one))
    updated_sheet_two$Obs_Num <- seq(1:nrow(updated_sheet_two))

    filePathList <- unlist(strsplit(appendingFilePath, "/"))
    filename <- paste("Updated", parkCode, tail(first_sheet$Month, 1), "Raw PPV data.xlsx")
    output_names <- list("Tally Sheets" = updated_sheet_one, "Observed Vehicles" = updated_sheet_two)
    openxlsx::write.xlsx(output_names, file = filename)
    print(paste("Outputted File:", filename))
  }
  else {
    first_sheet <- first_sheet[order(first_sheet$Date), ]
    second_sheet <- second_sheet[order(second_sheet$Date, second_sheet$Occupants), ]
    first_sheet$Sheet_Num <- seq(1:nrow(first_sheet))
    second_sheet$Obs_Num <- seq(1:nrow(second_sheet))
    first_sheet <- updateDataTypes(first_sheet, 1)
    second_sheet <- updateDataTypes(second_sheet, 2)
    filename <- paste(parkCode, tail(first_sheet$Month, 1), "Raw PPV Data.xlsx")
    output_names <- list("Tally Sheets" = first_sheet, "Observed Vehicles" = second_sheet)
    openxlsx::write.xlsx(output_names, file = filename)
    print(paste("Outputted File:", filename))
  }
}
#* Converting a string to a consistent date format
convertDate <- function(date_string) {
  date <- parse_date_time(date_string, orders = c("mdy", "dmy", "ymd", "bY"))
  return(format(date, "%m/%d/%y"))
}
#* Formatting the AM/PM string to filter out lower case characters and spaces
formatTime <- function(text) {
  text <- toupper(gsub(" ", "", text))
  return(text)
}

#!##########################################################################
#!###################      Code for the charts page     ####################
#!##########################################################################

#* Get a unique list for the entrances
getEntrances <- function(data) {
  return(unique(data$entrance))
}

formatSheet1 <- function(sheet) {
  sheet$Date <- as.Date(sheet$Date, format="%m/%d/%y")
  colnames(sheet) <- c("sheetNum", "numObs", "entrance", "date", "month", "dayOfWeek", "timeOfDay")
  suppressWarnings(sheet <- sheet %>%
    mutate(monthNum = as.integer(format(date, "%m")),
          sheetNum = as.integer(sheetNum),
          numObs = as.integer(numObs)))
  return(sheet)
}

formatSheet2 <- function(sheet) {
  sheet$Date <- as.Date(sheet$Date, format="%m/%d/%y")
  colnames(sheet) <- c("obsNum", "entrance", "month", "date", "dayOfWeek", "timeOfDay", "occupants")
  suppressWarnings(sheet <- sheet %>%
      mutate(monthNum = as.integer(format(date, "%m")),
            occupants = as.integer(occupants),
            obsNum = as.integer(obsNum)))
  return(sheet)
}

getTableEDA <- function(sheet_one, sheet_two) {
  table <- data.frame(col1 = c("Sheets returned", "Sheets with 0 obs", "Vehicles observed", "Min passengers", "Max passengers", "Mean passengers", "Median passengers"), 
                    col2 = c(nrow(sheet_one), nrow(filter(sheet_one, numObs == 0)), nrow(sheet_two), min(sheet_two$occupants), max(sheet_two$occupants), round(mean(sheet_two$occupants), 2), round(median(sheet_two$occupants), 2)))
  return(table)
}

getTableMonth <- function(sheet_one, sheet_two) {
  table1 <- sheet_one %>%
      group_by(monthNum) %>%
      summarise(numSheets = n(),
              minVehObs = min(numObs),
              maxVehObs = max(numObs))
  table2 <- sheet_two %>%
      group_by(monthNum) %>%
      summarise(numObs = n(),
              minOccupancy = min(occupants),
              maxOccupancy = max(occupants),
              ppv = round(mean(occupants), 2)) %>%
      mutate(numObsPrcnt = round(numObs / nrow(sheet_two), 2))
  table3 <- left_join(table1, table2, by="monthNum")
  table3 <- table3 %>%
      mutate(monthName = month.name[monthNum])
  table3 <- table3[, c("monthName", "numSheets", "numObs", "numObsPrcnt", "ppv", "minVehObs", "maxVehObs", "minOccupancy", "maxOccupancy")]
  colnames(table3) <- c("Month", "Number of Tallysheets", "Number of Observations", "Number of Obs (%)", "Persons Per Vehicle (PPV)", "Min Vehicles Observed", "Max Vehicles Observed", "Min Vehicle Occupancy", "Max Vehicle Occupancy")
  return(table3)
}

getTableTOD <- function(sheet_one) {
  table <- sheet_one %>%
      group_by(timeOfDay) %>%
      summarise(count = n(),
              mean = round(mean(numObs), 2),
              sd = round(sd(numObs), 2),
              se = round(sd(numObs)/length(numObs), 2),
              min = min(numObs),
              max = max(numObs))
  colnames(table) <- c("Time of Day", "Number of One Hour Collection Periods", "Mean Number of Observed Vehicles", "Standard Deviation (of mean)", "Standard Error (of mean)", "Minimum Number of Observed Vehicles", "Maximum Number of Observed Vehicles")
  return(table)
}

getTableDOW <- function(sheet_one) {
  table <- sheet_one %>%
      group_by(dayOfWeek) %>%
      summarise(count = n(),
              mean = round(mean(numObs), 2),
              sd = round(sd(numObs), 2),
              se = round(sd(numObs)/length(numObs), 2),
              min = min(numObs),
              max = max(numObs))
  colnames(table) <- c("Day of Week", "Number of One Hour Collection Periods", "Mean Number of Observed Vehicles", "Standard Deviation (of mean)", "Standard Error (of mean)", "Minimum Number of Observed Vehicles", "Maximum Number of Observed Vehicles")
  return(table)
}

getFigPPV <- function(data) {
    plotData <- data %>%
        group_by(monthNum) %>%
        summarise(Mean = round(mean(occupants), 2)) %>%
        mutate(Month = month.name[monthNum])
    plot <- ggplot(plotData) + 
        geom_point(aes(x=monthNum, y=Mean, text=paste('Month: ', Month, '<br>Mean: ', Mean))) + 
        geom_hline(yintercept=mean(plotData$Mean), color="red", linetype='dashed') + 
        labs(x="Month", y="PPV") + 
        theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
        scale_x_continuous(labels = month.name, breaks = seq(1, 12))
    return(list("plot" = plot, "data" = plotData))
}

getFigHistogram <- function(data, monthSelector) {
  if (monthSelector != "-1") {
    data <- filter(data, monthNum == as.integer(monthSelector))
  }
  data <- data %>%
    group_by(occupants) %>%
    summarise(count = n())
  colnames(data) <- c("Occupants", "Count")
  plot <- ggplot(data) + 
              geom_bar(stat = "identity", aes(x=Occupants, y = Count, text=paste('Occupants: ', Occupants, '<br>Count: ', Count)), color=customBlue, fill=customBlue) +
              labs(x="Occupants", y="Frequency")
  return(list("plot" = plot, "data" = data))
}

#!##########################################################################
#!###################      Code for the custom page     ####################
#!##########################################################################


# Plot for the ranges
figRange <- function(input) {
  input$start <- as.integer(input$start)
  input$end <- as.integer(input$end)
  input$end[1:length(input$end)] <- input$end[1:length(input$end)] + 1
  return(
    ggplot(data = input, aes(color=group)) + 
      geom_segment(aes(x=start, xend=end, y=group, yend=group), linewidth=12) +
      scale_x_discrete(limits=1:12) + 
      labs(x="Month", y="Group", color="Group") + 
      underTheme
  )
}

# Function to check if the ranges are valid
checkRange <- function(input) {
  ranges_formatted <- data.frame(month = seq(1, 12, 1), group = NA)
  ranges_plot <- data.frame(start = integer(), end = integer(), group = character())
  for (i in 1:length(input$ranges)) {
    items <- unlist(str_split(gsub(" ", "", input$ranges[i]), ","))
    currRange <- c()
    for (j in 1:length(items)) {
      if (grepl("-", items[j])) {
        range <- as.integer(unlist(str_split(items[j], "-")))
        if (range[1] > range[2]) {
          ranges_plot[nrow(ranges_plot) + 1,] <- c(range[1], 12, input$names[i])
          ranges_plot[nrow(ranges_plot) + 1,] <- c(1, range[2], input$names[i])
          currRange <- c(currRange, seq(range[1], 12, 1), seq(1, range[2], 1))
        }
        else {
          currRange <- c(currRange, seq(range[1], range[2], 1))
          ranges_plot[nrow(ranges_plot) + 1,] <- c(range[1], range[2], input$names[i])
        }
      }
      else {
        single <- as.integer(items[j])
        currRange <- c(currRange, single)
        ranges_plot[nrow(ranges_plot) + 1,] <- c(single, single, input$names[i])
      }
    }
    for (month in currRange) {
      if (!is.na(ranges_formatted[month,]$group)) {
        return(list("logical" = FALSE, "ranges" = ranges_formatted, "ranges_plot" = ranges_plot))
      }
      ranges_formatted[month,]$group <- input$names[i]
    }
  }
  if (sum(is.na(ranges_formatted$group)) != 0) {
    return(list("logical" = FALSE, "ranges" = ranges_formatted, "ranges_plot" = ranges_plot))
  }
  return(list("logical" = TRUE, "ranges" = ranges_formatted, "ranges_plot" = ranges_plot))
}

# test_ranges <- data.frame(ranges = c("12-4", "5-6, 9", "7-8, 10-11"), names = c("Group 1", "Group 2", "Group 3"))
# testData <- formatData(read_excel("RUCA\ Raw\ Data.xlsx", sheet=2))
# testData$monthNum <- as.integer(testData$monthNum)
# checkRange(test_ranges)

applyRanges <- function(data, ranges) {
  data$monthNum <- as.integer(data$monthNum)
  data$group <- ranges$group[match(data$monthNum, ranges$month)]
  colnames(data) <- c("obs_num", "entrance", "month", "date", "day_of_week", "time_of_day", "occupants", "monthNum", "group")
  return(data)
}

customPlot_ppv <- function(inputData) {
  data <- inputData %>%
                  group_by(group) %>%
                  summarise(total = sum(as.integer(occupants)),
                            ppv = sum(as.integer(occupants))/length(occupants))
  colnames(data) <- c("Group", "Total", "PPV")
  plot <- ggplot(data=data) + 
      geom_point(aes(x=Group, y=PPV, color=Group)) + 
      underTheme + 
      labs(x="Group", y="Average Vehicle Occupancy", title="PPV by Group", color="Group") + 
      theme(axis.title=element_text(face="bold"), axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ylim(min(data$PPV) - 0.1, max(data$PPV) + 0.1)
  return(list("data" = data, "plot" = plot))
}

poiReg <- function(data) {
  # Regression
  table <- data %>%
                group_by(group) %>%
                summarise(total = sum(as.integer(occupants)),
                          ppv = sum(as.integer(occupants))/length(occupants))
  colnames(table) <- c("Group", "Total", "PPV")
  group_names <- sort(unique(data$group))
  data$group <- relevel(factor(data$group, levels = group_names), ref=group_names[1])
  reg <- glm(formula=occupants ~ group, data=data, family=poisson)
  reg_sum <- as.data.frame(summary(reg)$coefficients)
  # coeff
  output <- data.frame(coeff = reg_sum[,1])
  # rownames(output) <- 1:12 # Resetting row names
  # e_coeff
  output <- output %>% mutate(e_coeff = exp(coeff + coeff[1]))
  output$e_coeff[1] <- exp(output$coeff[1])
  # se coeff
  output$se_coeff <- reg_sum[,2]
  # e_se_coeff
  output <- output %>% mutate(e_se_coeff = exp(se_coeff))
  # bounds
  output <- output %>% mutate(lwr_bnd = exp((coeff - 1.96*se_coeff) + coeff[1]))
  output$lwr_bnd[1] <- exp(output$coeff[1] - 1.96*output$se_coeff[1])
  output <- output %>% mutate(upr_bnd = exp((coeff + 1.96*se_coeff) + coeff[1]))
  output$upr_bnd[1] <- exp(output$coeff[1] + 1.96*output$se_coeff[1])
  output$group <- group_names
  output <- output %>% select(group, e_coeff, e_se_coeff, lwr_bnd, upr_bnd)
  # Plot
  plotData <- output
  plotData$xn <- 1:nrow(plotData)
  plot <- ggplot(data = plotData) + 
    geom_rect(aes(xmin=xn-0.06, xmax=xn+0.06, ymin=lwr_bnd, ymax=upr_bnd, fill=as.factor(xn)), alpha=0.6) + 
    geom_rect(aes(xmin=xn-0.01, xmax=xn+0.01, ymin=lwr_bnd, ymax=upr_bnd), alpha=1) + 
    geom_point(aes(x=xn, y=e_coeff), size=3, shape=18) + 
    # geom_point(aes(x=xn, y=upr_bnd), size=2) + 
    # geom_point(aes(x=xn, y=lwr_bnd), size=2) + 
    geom_segment(aes(x=xn-0.06, y=upr_bnd, xend=xn+0.06, yend=upr_bnd), linewidth=1.5, alpha=1) + 
    geom_segment(aes(x=xn-0.06, y=lwr_bnd, xend=xn+0.06, yend=lwr_bnd), linewidth=1.5, alpha=1) + 
    geom_text(aes(label = round(e_coeff, 2), x=xn, y=e_coeff), nudge_x=0.3) + 
    geom_text(aes(label = round(upr_bnd, 2), x=xn, y=upr_bnd), nudge_x=0.3) + 
    geom_text(aes(label = round(lwr_bnd, 2), x=xn, y=lwr_bnd), nudge_x=0.3) + 
    scale_x_discrete(limits = plotData$group) + 
    labs(x="Group", y="PPV") + 
    ylim(min(plotData$lwr_bnd) - 1, max(plotData$upr_bnd) + 1) + 
    underTheme
  return(list("data" = data, "table" = table, "plot" = plot))
}
# # Function to apply the ranges to the data

filterData <- function(data, filterNum) {
  filters <- c("0" = "entrance", "1" = "timeOfDay", "2" = "dayOfWeek")
  data <- data %>% group_by(across(all_of(as.character(filters[as.character(filterNum)])))) %>% 
    summarise(numVeh = length(occupants),
              min = min(as.integer(occupants)),
              max = max(as.integer(occupants)),
              mean = mean(as.integer(occupants)))
  return(data)
} 
