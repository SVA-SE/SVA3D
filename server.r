

library(shiny)
library(plotly)
library(ISOweek)
library(DT)
library(bindrcpp)
library(stringr)
#library(shinycssloaders)
#library(magrittr)
#library(shinyjs)


source("SVA3D_functions.r")
source("SVA3D_definitions.r")


shinyServer(function(input, output, session) {
  
  
  source(paste0(sourcefiles,"SVASSS2/SVASSS/server_SVASSS.r"),local=TRUE)
  
  source(paste0(sourcefiles,"SVA3D/AMR/server_AMR.r"),local=TRUE)
  
  source(paste0(sourcefiles,"SVA3D/Campy/server_Campy.r"),local=TRUE)
  
  source(paste0(sourcefiles,"SVA3D/VLT/server_VLT.r"),local=TRUE)
  
  
  
})
