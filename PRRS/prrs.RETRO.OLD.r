library(devtools)
install_github("nandadorea/vetsyn")
require(vetsyn)

require(plotly)


prrs.sva.retro1 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.1.txt", 
                 sep = "\t",
                 header = TRUE,
                 na.strings = "",
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8",
                 quote = "")
prrs.sva.retro2 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.2.txt", 
                         sep = "\t",
                         header = TRUE,
                         na.strings = "",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         quote = "")
prrs.sva.retro3 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.3.txt", 
                         sep = "\t",
                         header = TRUE,
                         na.strings = "",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         quote = "")
prrs.sva.retro4 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.4.txt", 
                         sep = "\t",
                         header = TRUE,
                         na.strings = "",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         quote = "")
prrs.sva.retro <- rbind(prrs.sva.retro1,prrs.sva.retro2,prrs.sva.retro3,prrs.sva.retro4)
rm(prrs.sva.retro1,prrs.sva.retro2,prrs.sva.retro3,prrs.sva.retro4)
prrs.sva.retro$Uppdrag <- gsub("<[^>]+>","",as.character(prrs.sva.retro$Uppdrag))


agens.prrs.list <- read.csv2(file="PRRS/prrs.agens.csv",encoding = "UTF-8",colClasses = "character")

agens.prrs1 <- unique(agens.prrs.list[agens.prrs.list$FINAL==1,1])
agens.prrs2 <- unique(agens.prrs.list[agens.prrs.list$FINAL==2,1])

prrs.sva.retro$PRRSagens <- 0
prrs.sva.retro$PRRSagens[prrs.sva.retro$Agens%in%agens.prrs1] <- 1
prrs.sva.retro$PRRSagens[prrs.sva.retro$Agens%in%agens.prrs2] <- 2
#table(prrs.sva.retro$PRRSagens)


unsersokning.prrs.list <- read.csv2(file="PRRS/prrs.undersokning.csv",encoding = "UTF-8",colClasses = "character")
unsersokning.prrs1 <- unique(unsersokning.prrs.list[unsersokning.prrs.list$FINAL==1,1])
unsersokning.prrs2 <- unique(unsersokning.prrs.list[unsersokning.prrs.list$FINAL==2,1])

prrs.sva.retro$PRRSunders <- 0
prrs.sva.retro$PRRSunders[prrs.sva.retro$Analys.Kod%in%unsersokning.prrs1] <- 1
prrs.sva.retro$PRRSunders[prrs.sva.retro$Analys.Kod%in%unsersokning.prrs2] <- 2
prrs.sva.retro$PRRSunders[prrs.sva.retro$Undersökning.Kod%in%unsersokning.prrs1] <- 1
prrs.sva.retro$PRRSunders[prrs.sva.retro$Undersökning.Kod%in%unsersokning.prrs2] <- 2
#table(prrs.sva.retro$PRRSunders)


#check1 <- prrs.sva.retro[prrs.sva.retro$PRRSunders==0&prrs.sva.retro$PRRSagens!=0,]
#   table(check1$Undersökning.Kod)


prrs.sva.retro$origin <- NA
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Besättningsutredning"] <- "Herd sanitization"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Ej angiven"] <- "Passive or not known"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Epizootisjukdom/ -misstanke"] <- "Passive or not known"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Export"] <- "Others"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Forskning/Projekt/Avtalsuppdrag"] <- "Others"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Hälsokontroll"] <- "Health control"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Hälsokontrollprogram (off)"] <- "Health control"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Klinisk mastit"] <- "Others"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Offentlig kontroll"] <- "Active surveillance"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Produktkontroll"] <- "Active surveillance"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Sjukdom/Skada/Dödsfall"] <- "Passive or not known"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Smittspårning"] <- "Tracing and follow up"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Spärrad besättning"] <- "Tracing and follow up"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Uppföljningsprover"] <- "Tracing and follow up"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Övervakningsprogram"] <- "Active surveillance"
prrs.sva.retro$origin[prrs.sva.retro$Provtagningsorsak.1=="Övrig"] <- "Others"
prrs.sva.retro$origin[is.na(prrs.sva.retro$origin)] <- "UNCLASSIFIED"


#table(prrs.sva.retro$origin,prrs.sva.retro$PRRSagens,prrs.sva.retro$PRRSunders)
#table(prrs.sva.retro$Överordnat.Uppdrag)



#write.csv2(prrs.sva.retro[,c(1,6,15,18,19,21,22,23,25,26,27,32,33,34)],file="prrs.sva.retro.csv")
# file to explore with domain experts
#not sure how to create teh time series from here - which events matter for eahc of the different origin?




# SJV data ----

prrs.sjv.retro <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\djursjukdata.HIST.GRIS_20190522_3800dagar.txt", 
                             sep = "\t",
                             header = TRUE,
                             na.strings = "",
                             stringsAsFactors = FALSE,
                             encoding = "UTF-8",
                             quote = "")

prrs.sjv.retro$Datum <- as.Date(prrs.sjv.retro$Datum,format="%Y-%m-%d")
prrs.sjv.retro <- prrs.sjv.retro[-which(prrs.sjv.retro$Datum>"2019-01-01"),]

source("PRRS//prrs.sjv.classifications.r")

prrs.sjv.retro$syndrome <- NA
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.inespecific.death]<-"inespecific.death"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.inespecific.repro]<-"inespecific.repro"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.allm]<-"allmänt"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.circ]<-"circulatorisk"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.resp]<-"respiratorisk"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.infek]<-"infektioner"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.gift]<-"förgiftning"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.repro]<-"reproduktion"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.nerv]<-"nervsystemet"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.oron]<-"öron och ögon"
prrs.sjv.retro$syndrome[prrs.sjv.retro$Diagnoskod%in%sjv.gris.hud]<-"hud och klövar"

#table(prrs.sjv.retro$syndrome)
#sum(is.na(prrs.sjv.retro$syndrome))


#plotted as time series to discuss with domain experts



# PROSPECTIVE ----
# 
# sjv <- read.csv2("T:\\SVA3D\\SVASSS\\Utdrag data Djursjukdata.txt", 
#                  sep = "\t",
#                  header = TRUE,
#                  na.strings = "",
#                  stringsAsFactors = FALSE,
#                  encoding = "UTF-8",
#                  quote = "")
# 
# 
# sva <- read.csv2("T:\\SVA3D\\SVASSS\\Syndromic report base data.txt", 
#                  sep = "\t",
#                  header = TRUE,
#                  na.strings = "",
#                  stringsAsFactors = FALSE,
#                  encoding = "UTF-8",
#                  quote = "")