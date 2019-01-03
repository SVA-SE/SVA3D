require(shiny)
require(markdown)
require(shinythemes)
require(plotly)
require(DT)
library(bindrcpp)
require(shinycssloaders)
library(stringr)
#library(magrittr)
#library(shinyjs)

source("SVA3D_definitions.r")

#SVASSS
source(paste0(sourcefiles,"SVASSS2/SVASSS/Definitions.r"))
load(paste0(sourcefiles,"SVASSS2/history_files/menu.summaries.RData"))
source(paste0(sourcefiles,"SVASSS2/SVASSS/ui_SVASSS.r"))

#AMR
source(paste0(sourcefiles,"SVA3D/AMR/ui_AMR.r"))

#Campy
source(paste0(sourcefiles,"SVA3D/Campy/ui_Campy.r"))

#VLT
source(paste0(sourcefiles,"SVA3D/VLT/ui_VLT.r"))


shinyUI(navbarPage(
  theme = shinythemes::shinytheme("united"),  
  "SVA3D",
  
  
  SVASSS_tab,
  AMR_tab,
  Campy_tab,
  VLT_tab
  
  
  
  )
)
