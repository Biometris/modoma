suppressPackageStartupMessages({
  library(shiny)
  library(shinyFiles)
  library(shinyjs)
  library(shinythemes)
  library(knitr)
  library(DT)
  library(plotly)
  library(tiff)
  library(png)
  library(readxl)
  library(modoma)
})

## Set to TRUE for demo app.
## The demo app has no options for reading input files but uses the
## Floricode Gerbera data inlcuded in the package.
demoApp <- FALSE

source("busyInd.R")
source("utils.R")
source("ui.R", local = TRUE)
source("server.R")

