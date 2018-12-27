

library(shiny)
library(plotly)
library(ISOweek)
library(DT)
library(bindrcpp)
#library(shinycssloaders)
#library(magrittr)
#library(shinyjs)


source("SVA3D_functions.r")


shinyServer(function(input, output, session) {
  
  
  source(paste0(sourcefiles,"SVASSS2/SVASSS/server_SVASSS.r"),local=TRUE,encoding="latin1")
  
  source(paste0(sourcefiles,"SVA3D/AMR/server_AMR.r"),local=TRUE,encoding="latin1")
  
  source(paste0(sourcefiles,"SVA3D/AMR/server_Campy.r"),local=TRUE,encoding="latin1")
  
  source(paste0(sourcefiles,"SVA3D/AMR/server_VLT.r"),local=TRUE,encoding="latin1")
  
  
  
})
