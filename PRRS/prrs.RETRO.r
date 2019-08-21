Sys.setlocale("LC_ALL", "swedish")

library(tidyverse)
library(lubridate)
library(stringr)

prrs.sva.retro1 <- read.csv2("I:/ESS/SVA3D/PRRS/retro data/syndromic.gris.1.txt", 
                             sep = "\t",
                             header = TRUE,
                             na.strings = "",
                             stringsAsFactors = FALSE,
                             encoding = "UTF-8",
                             quote = "")
prrs.sva.retro2 <- read.csv2("I:/ESS/SVA3D/PRRS/retro data/syndromic.gris.2.txt", 
                             sep = "\t",
                             header = TRUE,
                             na.strings = "",
                             stringsAsFactors = FALSE,
                             encoding = "UTF-8",
                             quote = "")
prrs.sva.retro3 <- read.csv2("I:/ESS/SVA3D/PRRS/retro data/syndromic.gris.3.txt", 
                             sep = "\t",
                             header = TRUE,
                             na.strings = "",
                             stringsAsFactors = FALSE,
                             encoding = "UTF-8",
                             quote = "")
prrs.sva.retro4 <- read.csv2("I:/ESS/SVA3D/PRRS/retro data/syndromic.gris.4.txt", 
                             sep = "\t",
                             header = TRUE,
                             na.strings = "",
                             stringsAsFactors = FALSE,
                             encoding = "UTF-8",
                             quote = "")

prrs.sva.retro <- bind_rows(prrs.sva.retro1, prrs.sva.retro2, prrs.sva.retro3, prrs.sva.retro4)
