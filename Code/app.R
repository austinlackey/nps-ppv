relWD <- "Code/" # Setting the Relative Working Directory
source(paste(relWD, "ppvFunctions.R", sep="")) # Load the functions
source(paste(relWD, "server.R", sep="")) # Load the server
source(paste(relWD, "ui.R", sep="")) # Load the UI

# Run ShinyApp to start webpage
shinyApp(
  ui=dashboardPage(HEADER, SIDEBAR, BODY), 
  server=SERVER
)










