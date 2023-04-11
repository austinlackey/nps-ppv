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
# Below are variables to refrence plot color and names
palette <- hue_pal()(16)
month.name <- c("January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
g.code <- c("#FFFFFF", "#EEEEEE", "#DDDDDD", "#CCCCCC", "#BBBBBB", "#AAAAAA", "#999999", "#888888", "#777777", "#666666", "#555555", "#444444", "#333333", "#222222", "#111111", "#000000")
# show_col(palette)
# show_col(g.code)
underTheme <- theme(panel.background = element_rect(fill = g.code[3], size = 2, linetype = "solid"),
                    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
                    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"))


#FUNCTIONS FOR TABLE PAGE ---------------------------------------------------------------------
# Function to output the excel file
runTally <- function(data, parkCode, appendingSheet) {
  appendingFilePath <- NULL
  if (appendingSheet) { # If er are adding to existing data, grab the file location
    appendingFilePath <- tk_choose.files(caption = "Select the excel document that you would like to append this data to:")
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
          row <- c(i, tally_frame$ENTRANCE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%B"), tally_frame$DATE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%A"), tally_frame$TIME[i], single_vals[num_pass])
          second_sheet <- rbind(second_sheet, row)
        }
      }
    }
    seven_vals <- as.numeric(tally_frame[i, c("X_SEVEN_PLUS")][[1]]) # 7+ values
    for (val in seven_vals) { # For each car add the row
      row <- c(i, tally_frame$ENTRANCE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%B"), tally_frame$DATE[i], format(as.Date(tally_frame$DATE[i], format="%m/%d/%Y"), "%A"), tally_frame$TIME[i], val)
      second_sheet <- rbind(second_sheet, row)
    }
  }
  colnames(second_sheet) <- c("Obs_Num", "Entrance", "Month", "Date", "Day_of_Week", "Time_of_Day", "Occupants") # Resetting the column names for the sheet
  second_sheet$Obs_Num <- seq(1:nrow(second_sheet))
  if (appendingSheet) {
    orig_sheet_one <- read_excel(appendingFilePath, 1)
    orig_sheet_two <- read_excel(appendingFilePath, 2)
    updated_sheet_one <- bind_rows(orig_sheet_one, first_sheet)
    updated_sheet_two <- bind_rows(orig_sheet_two, second_sheet)
    updated_sheet_one$Obs_Num <- seq(1:nrow(updated_sheet_one))
    updated_sheet_two$Obs_Num <- seq(1:nrow(updated_sheet_two))

    filePathList <- unlist(strsplit(appendingFilePath, "/"))
    filename <- paste("Updated", parkCode, tail(first_sheet$Month, 1), "Raw PPV data.xlsx")
    output_names <- list("Tally Sheets" = updated_sheet_one, "Observed Vehicles" = updated_sheet_two)
    openxlsx::write.xlsx(output_names, file = filename)
    print(paste("Outputted File:", filename))
  }
  else {
    filename <- paste(parkCode, tail(first_sheet$Month, 1), "Raw PPV Data.xlsx")
    output_names <- list("Tally Sheets" = first_sheet, "Observed Vehicles" = second_sheet)
    openxlsx::write.xlsx(output_names, file = filename)
    print(paste("Outputted File:", filename))
  }
}

#FUNCTIONS FOR CHARTS PAGE ---------------------------------------------------------------------

# Function to format the data to get the month number
formatData <- function(sheet) {
  sheet$Date <- as.Date(sheet$Date, format="%m/%d/%y")
  colnames(sheet) <- c("obsNum", "entrance", "month", "date", "dayOfWeek", "timeOfDay", "occupants")
    suppressWarnings(sheet <- sheet %>%
        mutate(monthNum = as.integer(format(date, "%m")),
              occupants = as.integer(occupants),
              obsNum = as.integer(obsNum)))
    return(sheet)
}

# Plot for the ppv averages per month
eda_PPV_Scatterplot <- function(input) {
  data <- input %>%
    group_by(monthNum) %>%
      summarise(total = sum(occupants),
                ppv = sum(occupants)/length(occupants))
  suppressWarnings(plot <- ggplot(data=data) + 
      geom_point(aes(x=as.integer(monthNum), y=ppv), color=g.code[12]) + 
      geom_hline(yintercept=mean(data$ppv), linetype="dashed", color=palette[1]) + 
      labs(x="Month", y="Persons Per Vehicle", title="PPV by Month") + 
      theme(axis.title=element_text(face="bold"), axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_discrete(limits=1:12, labels= month.name) + 
      underTheme)
  return(list(data=data, plot=plot))
}

# Plot for the Histograms
eda_PPV_Bar_Plot <- function(data, monthFilter = -1) {
  name <- "PPV Data for Jan-Dec"
  if (monthFilter != -1) {
    data <- data %>%
      filter(monthNum == monthFilter)
    name <- paste("PPV Data for", month.name[monthFilter])
  }
  table <- as.data.frame(table(data$occupants))
  colnames(table) <- c("num", "count")
  plot <- ggplot(data=table, aes(x=num, y=count)) + 
    geom_bar(fill=palette[11], stat="identity", alpha=1) + 
    theme(axis.title=element_text(face="bold"), plot.title=element_text(face="bold")) + 
    underTheme +
    labs(x="Number of Occupants", y="Number of Observations", title = name) + 
    geom_text(aes(label=count, y = count + 2), vjust=2)
  return(list(data=data, table = table, plot=plot))
}

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
    geom_rect(aes(xmin=xn-0.01, xmax=xn+0.01, ymin=lwr_bnd, ymax=upr_bnd), alpha=0.8) + 
    geom_point(aes(x=xn, y=e_coeff), size=3, shape=18) + 
    # geom_point(aes(x=xn, y=upr_bnd), size=2) + 
    # geom_point(aes(x=xn, y=lwr_bnd), size=2) + 
    geom_segment(aes(x=xn-0.06, y=upr_bnd, xend=xn+0.06, yend=upr_bnd), linewidth=1.5, alpha=0.4) + 
    geom_segment(aes(x=xn-0.06, y=lwr_bnd, xend=xn+0.06, yend=lwr_bnd), linewidth=1.5, alpha=0.4) + 
    geom_text(aes(label = round(e_coeff, 1), x=xn, y=e_coeff), nudge_x=0.3) + 
    geom_text(aes(label = round(upr_bnd, 1), x=xn, y=upr_bnd), nudge_x=0.3) + 
    geom_text(aes(label = round(lwr_bnd, 1), x=xn, y=lwr_bnd), nudge_x=0.3) + 
    scale_x_discrete(limits = plotData$group) + 
    labs(x="Group", y="PPV") + 
    ylim(min(plotData$lwr_bnd) - 1, max(plotData$upr_bnd) + 1) + 
    underTheme
  return(list("data" = data, "table" = table, "plot" = plot))
}
# # Function to apply the ranges to the data

# filteredData <- applyRanges(testData, checkRange(test_ranges)$ranges)

# ppv_data <- customPlot_ppv_data(filteredData)
# ggplotly(customPlot_ppv(ppv_data))

getEntrances <- function(data) {
  return(unique(data$entrance))
}

filterData <- function(data, filterNum) {
  filters <- c("0" = "entrance", "1" = "timeOfDay", "2" = "dayOfWeek")
  data <- data %>% group_by(across(all_of(as.character(filters[as.character(filterNum)])))) %>% 
    summarise(numVeh = length(occupants),
              min = min(as.integer(occupants)),
              max = max(as.integer(occupants)),
              mean = mean(as.integer(occupants)))
  return(data)
} 
