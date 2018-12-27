require(shiny)
require(markdown)
require(shinythemes)
require(plotly)
require(DT)
library(bindrcpp)
require(shinycssloaders)
#library(magrittr)
#library(shinyjs)

source("SVA3D_definitions.r")

#SVASSS
source(paste0(sourcefiles,"SVASSS2/SVASSS/Definitions.r"),local=TRUE,encoding="latin1")
load(paste0(shiny.history,"/menu.summaries.RData"))
source(paste0(sourcefiles,"SVASSS2/SVASSS/ui_SVASSS.r"),local=TRUE,encoding="latin1")

#AMR
source(paste0(sourcefiles,"SVA3D/AMR/ui_AMR.r"),local=TRUE,encoding="latin1")

#Campy
source(paste0(sourcefiles,"SVA3D/Campy/ui_Campy.r"),local=TRUE,encoding="latin1")

#VLT
source(paste0(sourcefiles,"SVA3D/VLT/ui_VLT.r"),local=TRUE,encoding="latin1")


shinyUI(navbarPage(
  theme = shinythemes::shinytheme("united"),  
  "SVA3D",
  
  
  SVASSS_tab,
  AMR_tab,
  Campy_tab,
  VLT_tab
  
  
  
  )
)
