Sys.setlocale("LC_ALL", "swedish")

library(tidyverse)
library(lubridate)
library(stringr)

# read the 4 data files into data frames

# prrs.sva.retro1 <-
#   read.csv2(
#     "I:/ESS/SVA3D/PRRS/retro data/syndromic.gris.1.txt",
#     sep = "\t",
#     header = TRUE,
#     na.strings = "",
#     stringsAsFactors = FALSE,
#     encoding = "UTF-8",
#     quote = ""
#   )


# # combine into one data frame and remove the original data frames
# prrs.sva.retro <-
#   rbind(prrs.sva.retro1,
#             prrs.sva.retro2,
#             prrs.sva.retro3,
#             prrs.sva.retro4)
# rm(prrs.sva.retro1,
#    prrs.sva.retro2,
#    prrs.sva.retro3,
#    prrs.sva.retro4)

html_pattern <- "<[^>]*>(.+)<[^>]*>"

prrs.sva.retro <-
  read.csv2(
    "I:/ESS/SVA3D/PRRS/retro data/prrs.retro.2018.txt",
    sep = "\t",
    header = TRUE,
    na.strings = "",
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    quote = ""
  ) %>%
  mutate(Uppdrag = ifelse(str_detect(Uppdrag, html_pattern), str_replace(Uppdrag, html_pattern, "\\1"), Uppdrag))

# import list of possible agens and their relevancy to PRRS
agens.prrs.list <-
    read.csv2(file = "PRRS/prrs.agens.csv",
            encoding = "UTF-8",
            colClasses = "character",
            na.strings = "")

# extract only the agens which have been marked relevant to PRRS, and discard irrelevant columns
agens.relevant <- agens.prrs.list %>%
  filter(!(is.na(FINAL) | FINAL == "?" | FINAL == "x")) %>%
  select(agens = X.U.FEFF.Agens, final = FINAL) %>%
  mutate(final = as.numeric(final))

undersokning.prrs.list <-
  read.csv2(
    file = "PRRS/prrs.undersokning.csv",
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = ""
  )

undersokning.relevant <- undersokning.prrs.list %>%
  filter(!is.na(FINAL)) %>%
  select(undersokning = X.U.FEFF.Kod, final = FINAL) %>%
  mutate(final = as.numeric(final))

# Find rows in dataset that have relevant agens and undersökning and attach value to them
prrs.sva.retro <- prrs.sva.retro %>%
  left_join(agens.relevant, by = c("Agens" = "agens")) %>%
  left_join(undersokning.relevant, by = c("Undersökning.Kod" = "undersokning", "Analys.Kod" = "undersokning"))

# set all other rows' values to 0
prrs.sva.retro <- prrs.sva.retro %>%
  mutate(
    PRRSagens = ifelse(is.na(final.x), 0, final.x),
    PRRSundersokning = ifelse(is.na(final.y), 0, final.y)
  ) %>%
  select(-final.x,-final.y)

prrs.sva.interesting <- prrs.sva.retro %>%
  filter(PRRSagens > 0 | PRRSundersokning > 0)

prrs.passive.orsak <- prrs.sva.interesting %>%
  filter(Provtagningsorsak.1 == "Epizootisjukdom/ -misstanke" | Provtagningsorsak.1 == "Sjukdom/Skada/Dödsfall")

prrs.no.orsak <- prrs.sva.interesting %>%
  filter(is.na(Provtagningsorsak.1))

prrs.no.orsak.numberless <- prrs.no.orsak %>%
  filter(is.na(Överordnade.uppdrag))

prrs.sva.interesting <- rbind(prrs.passive.orsak, prrs.no.orsak.numberless) %>%
  mutate(Ankomstdatum = as.Date(Ankomstdatum, "%m/%d/%y"))

interesting.timeseries <- prrs.sva.interesting %>%
  group_by(Ankomstdatum) %>%
  count() %>%
  complete(Ankomstdatum = seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")) %>%
  mutate(n = ifelse(is.na(n), 0, n))

interesting.timeseries %>%
  ggplot(aes(x = Ankomstdatum, y = n)) +
  geom_line()
