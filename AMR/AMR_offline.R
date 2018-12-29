library(tidyr)

source("I:/ESS/SVA3D/AMR/AMR_functions.R")



#resANT <- read.table("T:\\resistensrapporter\\Resistensbestämningar_ANT_allt_fcd.txt", header=TRUE, sep="\t", dec=",", encoding="latin1", quote = "\"",comment.char = "",fill=TRUE ) 

resANT <- read.csv2("T:\\resistensrapporter\\Resistensbestämningar_ANT_allt_fcd.csv", header=TRUE, encoding="latin1", colClasses = "character")
resBKT <- read.csv2("T:\\resistensrapporter\\Resistensbestämningar_BKT_allt_fcd.csv", header=TRUE, encoding="latin1", colClasses = "character") 

#str(resANT)
#str(resBKT)

source <- "ANT"
resANT <- cbind(source,resANT)

source <- "BKT"
Överordnade.uppdrag <- NA
ResultatID <- resBKT$ResultatID
resBKT <- cbind(source,ResultatID,Överordnade.uppdrag,resBKT[,2:22])

res <- rbind(resANT,resBKT)

#res$Analys <- as.character(res$Analys)
res$Analys <- gsub("\\s*\\([^\\)]+\\)","",res$Analys)
res$Analys <- gsub("brytpunkt","",res$Analys)
res$Analys <- gsub("[[:digit:]]+","",res$Analys)
res$Analys <- gsub(">","",res$Analys)
res$Analys <- gsub(",","",res$Analys)
res$Analys <- gsub("\\\"","",res$Analys)
res$Analys <- gsub("GN","",res$Analys)
res$Analys <- tolower(res$Analys)
res$Analys <- gsub("\\ /","\\/",res$Analys)
res$Analys <- gsub(" gp","",res$Analys)
res$Analys <- gsub("-gp","",res$Analys)
res$Analys <- gsub(" strept","",res$Analys)
res$Analys <- gsub(" staf","",res$Analys)
res$Analys <- gsub(" s aureus","",res$Analys)
res$Analys <- gsub("-staf aur","",res$Analys)
res$Analys <- gsub(" övr","",res$Analys)
res$Analys <- trimws(res$Analys)
res$Analys[which(res$Analys=="trim-sulfa")]<- "trimetoprim-sulfa"
res$Analys[which(res$Analys=="cefotaxim")]<- "cefotaxime"
res$Analys[which(res$Analys=="gentamycin")]<- "gentamicin"
res$Analys[which(res$Analys=="sulfamehtoxazol")]<- "sulfametoxazol"
res$Analys[which(res$Analys=="trimetoprim-sulfa")]<- "trimetoprim/sulfa"
res$Analys[which(res$Analys=="tetracyklin")]<- "tetracycline"
res$Analys[which(res$Analys=="trimetoprim/sulfamethoxazol")]<- "trimetoprim/sulfa"
res$Analys[which(res$Analys=="fusidinsyra")]<- "fusidin"

#unique(sort(as.character(res$Analys)))

res <- res[-which(res$Analys==""),]
res$Antibiotikaresultat[which(res$Antibiotikaresultat=="")]<-NA
res$Antibiotikaresultat <- gsub("\\?","<=",res$Antibiotikaresultat)

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="ab-nummer",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="cefinas",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="esbl-a-pcr",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="esbl-m-pcr",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="kvot caz och caz-c",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="kvot ctx och ctx-c",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="maldi-tof",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="penicillinastest",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="staf maldi",resultColumn="Antibiotikaresultat")

res <- fillResultat(data=res,IDColumn="ResultatID",analysColumn="Analys",
                    analysValue="tmsz från biomic",resultColumn="Antibiotikaresultat")

res <- res[-(which(duplicated(res))),]

res <- spread(data=res,Analys,Antibiotikaresultat)

year <- as.numeric(paste0("20",substr(res$InsäntmaterialID,1,2)))
res <- cbind(res,year)




save(res,file="I:/ESS/SVA3D/AMR/AMR_total.RData")


