Version = 3.0

#----------------------------------------------------------------------------
# Adjusting "MainLocation" is all that is required for first-time setup
#----------------------------------------------------------------------------

# Main directory of all folders/files
MainLocation = "C:/path/to/files/"

#----------------------------------------------------------------------------
#
#----------------------------------------------------------------------------

# Location of run.R, ui.R, and server.R (MUST ALSO BE CHANGED IN BATCH FILE)
AppLocation = paste(MainLocation,"shiny", sep = "")

# Location of R packages library
LibraryLocation = paste(MainLocation,"library", sep = "")

# Location of additional scripts required for cleaning
ScriptLocation = paste(MainLocation,"scripts", sep = "")

# Install/load required R packages
usePackage = function(p) {
  if (!is.element(p, installed.packages(lib.loc = LibraryLocation)[,1]))
    install.packages(p, dep = TRUE,lib = LibraryLocation,repos = "http://cran.us.r-project.org")
  require(p, character.only = TRUE,lib.loc = LibraryLocation)
}
usePackage("dplyr")
usePackage("shiny")
usePackage("shinyWidgets")
usePackage("shinyjs")
usePackage("ggplot2")
usePackage("ff")
usePackage("lubridate")
usePackage("xts")
usePackage("reshape2")
usePackage("mitml")
usePackage("mice")

# Source required R script
source(paste(ScriptLocation,"/LVL1toLVL2.R", sep = ""))

# Launch App
runApp(appDir = AppLocation,launch.browser = T)