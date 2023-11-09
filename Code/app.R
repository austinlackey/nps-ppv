  #!#########################################################################
  #!###################       Tallysheet Automation      ####################
  #!#########################################################################

# HOW TO SETUP
# STEP 1: Ensure packages designated in the 'ppvFunctions.R' file are installed.
#     - You can install them by running the following command in the console
#     - 'install.packages("<package name>")'
#
# STEP 2: Set the working directory in R-Studio to the location of the files
#     - To do this, first click the 3 little dots on the right side of
#       the 'Files' Pane, it can be hard to find (it should be under the
#       grey refresh button).
#     - A window should pop up for you to navigate to the folder where the
#       files reside.
#     - Once the folder has been opened, click the blue cog button and then
#       click "Set As Working Directory".
#
# STEP 3: Change the variable 'relWD' to "" (empty string).
#     - This sets the relative working directory to nothing.
#     - This variable is used if the files reside in a folder thats nested
#       inside the working directory.
#     - For example, the files for me are located in a folder called "New Stuff"
#       which is one folder deeper than my current working directory. If you
#       set your working directory to the same folder where the files are,
#       make sure the string is empty.
#
# STEP 4: Click the 'Run App' Button on the top of R-Studio
#     - If this does not work as expected, run each line of 'app.R' in the
#       console separately before running the shinyApp() line.
#     - If you have any packages that are not installed you will get an error
#       which will let you know what packages are missing/not installed.
#
# If you did everything correct a window should pop up with the app loaded.
# TIP: Click 'Open in Browser' at the top of the window for a better experience.



# HOW TO USE
# STEP 1: Enter the Park 4-character Unit Code.
# STEP 2: Unless you want to add new data to an already existing document,
#       leave the "Append to..." checkmark unchecked.
#
# STEP 3: Enter the Entrance (name), Date (XX/XX/XXXX), and
#       Time of day (AM/PM) for the tallysheet.
#
# STEP 4: Enter the number of TALLY MARKS for each car occupancy.
#     - For cars with 7+ passengers, enter each specific occupancy number
#       followed by a comma).  EX (for 5 cars): "7, 7, 9, 15, 25"
#     - Keep in mind this works for parks with multiple lanes per
#       entrance, it will add Lanes 1 and 2 together.
#       (The default values for both lanes are 0)
#     - If you only collect data at one lane, you can ignore the
#       Lane 2 tab as it will not affect anything)
#
# STEP 5: Click the green 'Add Tallysheet' Button. If you did the math
#       on the tallysheet correctly, the TOTAL column should match.
#     - If it doesn't, you multiplied/added wrong or you entered a
#       typo in the app, so make sure everything is correct.
#     - If you find you mess up entered data, you can keep removing
#       the most recent tallysheet added until the error you found
#       disappears, there is no way to delete specific entries.
#
# STEP 6: Make sure you click the orange 'Reset Input's'
#       Button after each entry, 
#     - This will reset the input fields for new data to be
#       added (except for Entrance and Date)
#     - This makes it easier to quickly enter data for
#       multiple entrances on the same day)
#
# STEP 7: When you are finished, click the blue 'Create Excel File' Button.
#      - This will output an excel file in your working directory
#       with all of the data.
#      TIP: If you have a lot of data, periodically click the
#     'Create Excel File' Button to save the current data.
#      - If the app or R crashes due to an error, all data will be lost.
#       By saving after every/every other month, it will help prevent
#       having to re-enter data).


# INSIGHTS
#   - You will notice there is an 'Insights' tab on the left. If you are eager,
#     you can visualize the data you just saved by clicking the blue
#     'Load Data' button.
#   - A window will pop up for you to choose an excel file to analyze.
#   - If you do not see a window pop up, it sometimes pops up behind other
#     windows. 
#   - Once loaded in, you can view the data itself, or you can view tables,
#     charts, or view custom confidence interval groups for ppv's. 
#   - A more advanced and trivial version of this is currently being made in
#     Power BI.

relWD <- "Code/" # Setting the Relative Working Directory
source(paste(relWD, "ppvFunctions.R", sep="")) # Load the functions
source(paste(relWD, "server.R", sep="")) # Load the server
source(paste(relWD, "ui.R", sep="")) # Load the UI

# Run ShinyApp to start webpage
shinyApp(
  ui=dashboardPage(HEADER, SIDEBAR, BODY), 
  server=SERVER
)
