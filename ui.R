HEADER <- dashboardHeader(title="NPS PPV Study App")

SIDEBAR <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Table", icon = icon("th"), tabName = "table"),
    menuItem("Charts", icon = icon("chart-simple"), tabName = "chart", badgeLabel = "new", badgeColor = "green")
  )
)

BODY <- dashboardBody(
  tabItems(
    tabItem(tabName = "table",
      fluidPage(
        useShinyjs(),
        div(
          id = "upperData",
          fluidRow(
            box(
              h4("Output Options"),
              strong("Park Unit Code"),
              textInput("parkNameIn", NULL, "XXXX", width="70px"),
              checkboxInput("checkbox", "Append to existing excel doc? (Choose path after you create excel file)", value = FALSE),
              actionButton("addrow", "Add Tally Sheet", style="color: #fff; background-color: #00BA38"),
              actionButton("revrow", "Remove Tally Sheet", style="color: #fff; background-color: #F8766D"),
              actionButton("submit", "Create Excel File", style="color: #fff; background-color: #619CFF")
            )
          )
        )
      ),
      fluidPage(
        useShinyjs(),
        div(
          id = "lowerData",
          fluidRow(
            column(12,
              fluidRow(
                column(4,
                h4("Input Data"),
                actionButton("resetData", "Reset Input's", style="color: #fff; background-color: #ff9661"),
                  textInput("entranceIn", "Entrance", "Main", width="200px"),
                  textInput("dateIn", "Date", "01/1/23", width="100px"),
                  strong("Time of Day"),
                  textInput("timeIn", NULL, c("AM", "PM")[round(runif(1, 1, 2))], width="60px"),
                  tabBox(width=12,
                    tabPanel("Lane 1",
                            h4("Lane 1 Sheet Input"),
                            # strong("Number of cars with 1 Passenger"),
                            # textInput("vehOneIn1", NULL, as.character(round(runif(n=1, min=0, max=15))), width="60px"),
                            # strong("Number of cars with 2 Passengers"),
                            # textInput("vehTwoIn1", NULL, as.character(round(runif(n=1, min=0, max=15))), width="60px"),
                            # strong("Number of cars with 3 Passengers"),
                            # textInput("vehThreeIn1", NULL, as.character(round(runif(n=1, min=0, max=15))), width="60px"),
                            # strong("Number of cars with 4 Passengers"),
                            # textInput("vehFourIn1", NULL, as.character(round(runif(n=1, min=0, max=15))), width="60px"),
                            # strong("Number of cars with 5 Passengers"),
                            # textInput("vehFiveIn1", NULL, as.character(round(runif(n=1, min=0, max=15))), width="60px"),
                            # strong("Number of cars with 6 Passengers"),
                            # textInput("vehSixIn1", NULL, as.character(round(runif(n=1, min=0, max=15))), width="60px"),
                            # strong("Number of cars with 7+ Passengers (7,8,9...)"),
                            # textInput("vehSevenIn1", NULL, paste(as.character(round(runif(n=3, min=7, max=25))), collapse=","), width="250px")
                            strong("Number of cars with 1 Passenger"),
                            textInput("vehOneIn1", NULL, "0", width="60px"),
                            strong("Number of cars with 2 Passengers"),
                            textInput("vehTwoIn1", NULL, "0", width="60px"),
                            strong("Number of cars with 3 Passengers"),
                            textInput("vehThreeIn1", NULL, "0", width="60px"),
                            strong("Number of cars with 4 Passengers"),
                            textInput("vehFourIn1", NULL, "0", width="60px"),
                            strong("Number of cars with 5 Passengers"),
                            textInput("vehFiveIn1", NULL, "0", width="60px"),
                            strong("Number of cars with 6 Passengers"),
                            textInput("vehSixIn1", NULL, "0", width="60px"),
                            strong("Number of cars with 7+ Passengers (7,8,9...)"),
                            textInput("vehSevenIn1", NULL, "0", width="250px")
                    ),
                    tabPanel("Lane 2",
                            h4("Lane 2 Sheet Input"),
                            strong("Number of cars with 1 Passenger"),
                            textInput("vehOneIn2", NULL, "0", width="60px"),
                            strong("Number of cars with 2 Passengers"),
                            textInput("vehTwoIn2", NULL, "0", width="60px"),
                            strong("Number of cars with 3 Passengers"),
                            textInput("vehThreeIn2", NULL, "0", width="60px"),
                            strong("Number of cars with 4 Passengers"),
                            textInput("vehFourIn2", NULL, "0", width="60px"),
                            strong("Number of cars with 5 Passengers"),
                            textInput("vehFiveIn2", NULL, "0", width="60px"),
                            strong("Number of cars with 6 Passengers"),
                            textInput("vehSixIn2", NULL, "0", width="60px"),
                            strong("Number of cars with 7+ Passengers (7,8,9...)"),
                            textInput("vehSevenIn2", NULL, "0", width="250px")
                    )
                  )
                ),
                column(6,
                  h4("Output Table"),
                  tableOutput("table")
                )
              )
            )
          )
        )
      )
    ),
    tabItem(tabName = "chart",
      fluidPage(
        fluidRow(
          h2("Interactive Charts and Visualizations"),
          actionButton("loadData", "Load Data", style="color: #fff; background-color: #6161ff"),
          br(),
          br(),
          tabBox(width=12,
            tabPanel("EDA",
              fluidRow(
                #TODO All of this page!
                h2("Main Plot"),
                column(1,
                  h3("Table"),
                  tableOutput("plot1_table")
                ),
                column(1,
                  h3("Stats"),
                  tableOutput("plot1_stat_table")
                ),
                column(6,
                  # tableOutput("plot1_table")
                  plotlyOutput("plot1")
                )
              ),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              fluidRow(
                #TODO All of this page!
                h2("Bar Plot"),
                selectInput("monthSelector", "Select Month", 
                  choices = c("All Months" = -1, "January" = "1", "Febuary" = "2", "March" = "3", "April" = "4", "May" = "5", "June" = "6", "July" = "7", "August" = "8", "September" = "9", "October" = "10", "November" = "11", "December" = "12")),
                column(1,
                  h3("Table"),
                  tableOutput("plot2_table")
                ),
                column(1,
                  h3("Stats"),
                  tableOutput("plot2_stat_table")
                ),
                column(6,
                  plotlyOutput("plot2")
                )
              ),
              br(),
              br(),
              fluidRow(
                h2("Filter By..."),
                selectInput("filterSelector", "Select Filter", 
                  choices = c("Location" = 0, "Time of Day" = 1, "Day of Week" = 2), selected = "Location"),
                tableOutput("filterTable")
              )
            ),
            tabPanel("Custom",
              fluidRow(
                column(3,
                  sliderInput("numGroups", h2("Number of Groups"), min=2, max=12, value=2),
                  br(),
                  br(),
                  uiOutput("sliders")
                ),
                column(3,
                  h2("Group Names"),
                  uiOutput("sliders_text")
                ),
                column(5,
                  h2("Group Ranges"),
                  plotlyOutput("plotRange")
                )
              ),
              actionButton("generateGroups", "Generate Groups", style="color: #fff; background-color: #61BDFF"),
              fluidRow(
                #TODO All of this page!
                h2("Statistics"),
                column(2,
                  h3("Table"),
                  tableOutput("custom_ci_table")
                ),
                column(6,
                  plotlyOutput("custom_ci_plot")
                )
              )
            )
          )
        )
      )
    )
  )
)