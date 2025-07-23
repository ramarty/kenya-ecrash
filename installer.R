# Install R Packages and Create Shortcut for ShinyApp.

# INSTRUCTIONS
  # 1. Click "Source" to run script
  # 2. After running once, restart R (close and open), then run again (click "Source").

# Installs:
  # 1. Packages
  # 2. Tinytex
  # 3. Nairobi basemaps

DOWNLOAD_BASEMAP <- FALSE
INSTALL_LATEX <- FALSE

# Install Packages -------------------------------------------------------------
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"bcrypt" %in% installed.packages()) install.packages("bcrypt")
if (!"shiny" %in% installed.packages()) install.packages("shiny")
#if (!"aws.s3" %in% installed.packages()) install.packages("aws.s3")
if (!"aws.s3" %in% installed.packages()) install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
if (!"purrr" %in% installed.packages()) install.packages("purrr")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"DT" %in% installed.packages()) install.packages("DT")
if (!"curl" %in% installed.packages()) install.packages("curl")
if (!"data.table" %in% installed.packages()) install.packages("data.table")
if (!"shinyjs" %in% installed.packages()) install.packages("shinyjs")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"DBI" %in% installed.packages()) install.packages("DBI")
if (!"shinydashboard" %in% installed.packages()) install.packages("shinydashboard")
if (!"leaflet" %in% installed.packages()) install.packages("leaflet")
if (!"mapview" %in% installed.packages()) install.packages("mapview")
if (!"stringr" %in% installed.packages()) install.packages("stringr")
if (!"tinytex" %in% installed.packages()) install.packages("tinytex")
if (!"readr" %in% installed.packages()) install.packages("readr")
if (!"openssl" %in% installed.packages()) install.packages("openssl")
if (!"knitr" %in% installed.packages()) install.packages("knitr")
if (!"shinyShortcut" %in% installed.packages()) install.packages("shinyShortcut")
if (!"RgoogleMaps" %in% installed.packages()) install.packages("RgoogleMaps")
if (!"plotly" %in% installed.packages()) install.packages("plotly")
if (!"tidyr" %in% installed.packages()) install.packages("tidyr")
if (!"forcats" %in% installed.packages()) install.packages("forcats")
if (!"scales" %in% installed.packages()) install.packages("scales")
if (!"tools" %in% installed.packages()) install.packages("tools")
if (!"grid" %in% installed.packages()) install.packages("grid")
if (!"png" %in% installed.packages()) install.packages("png")
if (!"jpeg" %in% installed.packages()) install.packages("jpeg")
if (!"reactable" %in% installed.packages()) install.packages("reactable")
if (!"shinyWidgets" %in% installed.packages()) install.packages("shinyWidgets")
if (!"gmailr" %in% installed.packages()) install.packages("gmailr")
if (!"shinyalert" %in% installed.packages()) install.packages("shinyalert")
if (!"R.utils" %in% installed.packages()) install.packages("R.utils")
if (!"kableExtra" %in% installed.packages()) install.packages("kableExtra")
if (!"devtools" %in% installed.packages()) install.packages("devtools")
library(devtools)
if (!"dashboardthemes" %in% installed.packages()) install_github("nik01010/dashboardthemes")

# Install LaTeX ----------------------------------------------------------------
# To download LaTeX, we need to install (1) the tinytex R package and (2) tinytex dependency
# (which downloads on the computer). After installing the tinytex R package, R needs
# to be restarted. Consequently, we don't install tinytex dependency if the R package is
# installed in the same session.

if(INSTALL_LATEX){

  # Check if the tinytex R package pacakge is installed...
  if (!"tinytex" %in% installed.packages()){
    
    # ... if tinytex R package isn't installed, install it.
    install.packages("tinytex")
  } else{
    
    # ... if tinytex R package is installed, install tinytex dependency on computer 
    # if hasn't been installed already.
    if(tinytex:::is_tinytex() %in% FALSE) tinytex::install_tinytex()
    
    
  }
  
}

tinytex::reinstall_tinytex()

# Download Nairobi Basemap -----------------------------------------------------
if(DOWNLOAD_BASEMAP){
  library(RgoogleMaps)
  
  dir.create(file.path(dirname(sys.frame(1)$ofile), "basemap")) 
  
  for(zoom in 1:16){
    GetMapTiles(center = c(lat = -1.286389, lon = 36.817222), 
                lonR = c(36.596913, 37.110709),
                latR = c(-1.439163, -1.093424),
                zoom=zoom, 
                tileDir = file.path(dirname(sys.frame(1)$ofile), "basemap"),
                TotalSleep = .1,
                CheckExistingFiles = T) #               
  }
}


# Create Shortcut for Shiny App ------------------------------------------------
library(shinyShortcut)
shinyShortcut(dirname(sys.frame(1)$ofile))


