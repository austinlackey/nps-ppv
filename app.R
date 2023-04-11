relWD <- "New\ Stuff/"
source(paste(relWD, "ppvFunctions.R", sep=""))
source(paste(relWD, "server.R", sep=""))
source(paste(relWD, "ui.R", sep=""))

# Run ShinyApp to start webpage
shinyApp(
  ui=dashboardPage(HEADER, SIDEBAR, BODY), 
  server=SERVER
)
