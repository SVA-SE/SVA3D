prrs.retro1 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.1.txt", 
                 sep = "\t",
                 header = TRUE,
                 na.strings = "",
                 stringsAsFactors = FALSE,
                 encoding = "UTF-8",
                 quote = "")
prrs.retro2 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.2.txt", 
                         sep = "\t",
                         header = TRUE,
                         na.strings = "",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         quote = "")
prrs.retro3 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.3.txt", 
                         sep = "\t",
                         header = TRUE,
                         na.strings = "",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         quote = "")
prrs.retro4 <- read.csv2("I:\\ESS\\SVA3D\\PRRS\\retro data\\syndromic.gris.4.txt", 
                         sep = "\t",
                         header = TRUE,
                         na.strings = "",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8",
                         quote = "")
prrs.retro <- rbind(prrs.retro1,prrs.retro2,prrs.retro3,prrs.retro4)
rm(prrs.retro1,prrs.retro2,prrs.retro3,prrs.retro4)
prrs.retro$Uppdrag <- gsub("<[^>]+>","",as.character(prrs.retro$Uppdrag))


agens.prrs.list <- read.csv2(file="PRRS/prrs.agens.csv",encoding = "UTF-8",colClasses = "character")

agens.prrs1 <- unique(agens.prrs.list[agens.prrs.list$FINAL==1,1])
agens.prrs2 <- unique(agens.prrs.list[agens.prrs.list$FINAL==2,1])


unsersokning.prrs.list <- read.csv2(file="PRRS/prrs.undersokning.csv",encoding = "UTF-8",colClasses = "character")
unsersokning.prrs1 <- unique(unsersokning.prrs.list[unsersokning.prrs.list$FINAL==1,1])
unsersokning.prrs2 <- unique(unsersokning.prrs.list[unsersokning.prrs.list$FINAL==2,1])


#table(prrs.retro$Provtagningsorsak.1)
#table(prrs.retro$Provtagningsorsak.1,prrs.retro$Undersökning)
#write.csv2(table(prrs.retro$Provtagningsorsak.1,prrs.retro$Undersökning),file="quality.time.csv")

prrs.retro$origin <- NA
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Besättningsutredning"] <- "Herd sanitization"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Ej angiven"] <- "Passive or not known"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Epizootisjukdom/ -misstanke"] <- "Passive or not known"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Export"] <- "Others"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Forskning/Projekt/Avtalsuppdrag"] <- "Others"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Hälsokontroll"] <- "Health control"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Hälsokontrollprogram (off)"] <- "Health control"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Klinisk mastit"] <- "Others"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Offentlig kontroll"] <- "Active surveillance"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Produktkontroll"] <- "Active surveillance"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Sjukdom/Skada/Dödsfall"] <- "Passive or not known"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Smittspårning"] <- "Tracing and follow up"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Spärrad besättning"] <- "Tracing and follow up"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Uppföljningsprover"] <- "Tracing and follow up"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Övervakningsprogram"] <- "Active surveillance"
prrs.retros$origin[prrs.retro$Provtagningsorsak.1=="Övrig"] <- "Others"
prrs.retro$origin[is.na(prrs.retro$origin)] <- "UNCLASSIFIED"

#table(prrs.retro$Agens)



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