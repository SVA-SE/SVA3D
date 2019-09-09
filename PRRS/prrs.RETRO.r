Sys.setlocale("LC_ALL", "swedish")

library(lubridate)
library(stringr)
library(gtools)
library(data.table)
library(ggplot2)

make_timeseries <- function(data, daterange) {
  ret <-
    merge(data[, .N, by = date][order(date)],
          daterange,
          by = "date",
          all = TRUE)[, c("doy", "week", "month", "year") := list(as.numeric(strftime(date, format = "%j")),
                                                                  week(date),
                                                                  month(date),
                                                                  as.factor(year(date)))]
  ret <- ret[, yearweek :=  str_c(as.character(year), as.character(week), sep = "-")]
  ret[is.na(N)]$N <- 0
  
  factor()
  ret <- ret[]
  return(ret)
}

#### SVA DATA ####

html_pattern <- "<[^>]*>(.+)<[^>]*>"

sva.retro <-
  fread(
    "I:/ESS/SVA3D/PRRS/retro data/sva.retro.2014-2018.txt",
    header = TRUE,
    na.strings = "",
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    quote = "\"",
    data.table = TRUE
  )[str_detect(Uppdrag, html_pattern), Uppdrag := str_replace(Uppdrag, html_pattern, "\\1")] %>%
  .[, Ankomstdatum := as.Date(Ankomstdatum, "%m/%d/%y")] %>%
  .[is.na(Ankomstdatum), Ankomstdatum := as.Date(str_match(Uppdrag, "^U(\\d{6})")[, 2], format = "%y%m%d")] %>%
  setnames(old = "Ankomstdatum", new = "date")

# import list of possible agens and their relevance to PRRS
sva.agens <-
  fread(
    "I:/ESS/SVA3D/PRRS/prrs.sva.agens.csv",
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = ""
  )

# extract only the agens which have been marked relevant to PRRS, and discard irrelevant columns
sva.agens.relevant <- sva.agens %>%
  .[!(is.na(FINAL) | FINAL == "?" | FINAL == "x"), .(Agens, final = FINAL)] %>%
  .[, final := as.numeric(final)]

# repeat for undersökningar
sva.undersokning <-
  unique(fread(
    "I:/ESS/SVA3D/PRRS/prrs.sva.undersokning.csv",
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = ""
  )) %>%
  .[, mean(as.numeric(FINAL)), by = Kod]

sva.undersokning.relevant <- sva.undersokning %>%
  .[!is.na(V1), .(undersokning = Kod, final = V1)] %>%
  .[, final := as.numeric(final)]

# Find rows in dataset that have relevant agens and undersökning and attach value to them
sva.with.agens <- sva.agens.relevant[sva.retro, on = "Agens"]

sva.with.agens.undersokning <-
  sva.undersokning.relevant[sva.with.agens,
                            on = c("undersokning" = "Undersökning.Kod",
                                   "undersokning" = "Analys.Kod")] %>%
  setnames(
    old = c("undersokning", "undersokning.1", "i.final", "final"),
    new = c(
      "Undersökning.Kod",
      "Analys.Kod",
      "PRRSagens",
      "PRRSundersokning"
    )
  )


# set all other rows' values to 0
sva.with.agens.undersokning[is.na(PRRSagens)]$PRRSagens <- 0
sva.with.agens.undersokning[is.na(PRRSundersokning)]$PRRSundersokning <- 0

# load table with classification of provtagningsorsaker
sva.provtagningsorsak <- fread("I:/ESS/SVA3D/PRRS/prrs.sva.provtagningsorsak.csv",
                        encoding = "UTF-8",
                        colClasses = "character",
                        na.strings = "",
                        stringsAsFactors = FALSE
                        )

# add provtagningsorsak class to the data
sva.agens.undersokning.origin <- merge(sva.with.agens.undersokning, sva.provtagningsorsak, all.x = T, by = "Provtagningsorsak.namn")
sva.agens.undersokning.origin[is.na(ori)]$ori <- "Unclassified"

# extract the rows with a PRRS-relevant agens or undersökning, and that are assumed to be passive surveillance cases 
# (i.e. have a provtagningsorsak classified as passive surveillance or are missing an Överordnat uppdrag)
sva.interesting <-
  sva.agens.undersokning.origin[
    (PRRSagens > 0 | PRRSundersokning > 0) & 
      (ori == "Passive/unknown" | is.na(`Överordnade uppdrag`))]


#### SJV DATA ####

# read the data
sjv.retro <- fread("I:/ESS/SVA3D/PRRS/retro data/sjv.retro.3800-days.txt",
                   encoding = "UTF-8",
                   na.strings = "",
                   stringsAsFactors = FALSE
                   )
sjv.retro$Datum <- as.Date(sjv.retro$Datum)
setnames(sjv.retro, old = "Datum", new = "date")

# remove strange dates
sjv.retro <- sjv.retro[date <= as.Date("2018-08-08")]

# import diagnosis codes and their classifications
sjv.diagnos <- fread(
  "I:/ESS/SVA3D/PRRS/prrs.sjv.diagnos.csv",
  encoding = "UTF-8",
  na.strings = "",
  stringsAsFactors = FALSE
)

# import handelsvara codes and their relevance to reproductive and/or respiratory diagnoses
sjv.handelsvara <-
  fread(
    "I:/ESS/SVA3D/PRRS/prrs.sjv.handelsvara.csv",
    encoding = "UTF-8",
    na.strings = "",
    stringsAsFactors = FALSE
  )
sjv.handelsvara[is.na(Handelsvara.respiratory)]$Handelsvara.respiratory <- 0
sjv.handelsvara[is.na(Handelsvara.reproductive)]$Handelsvara.reproductive <- 0

### the following code looks messy, but essentially does the following:
# 1. split the data into rows that have a handelsvarakod and rows that don't
# 2. for the data that have handelsvarakod, make sure each code only appears once, 
# but still keep all the variants of beskrivning by:
#   2.1. grouping by handelsvarakod and saving a list of all beskrivning for each code
#   2.2. for the respiratory and reproductive classifications, doing the same but only saving
#        the first element of the lists (since all elements are identical for a specific 
#        code)
#   2.3. filtering out all (now) duplicate rows so that each row represents a unique handelsvarakod

sjv.handelsvara.nocode <- sjv.handelsvara[is.na(Handelsvarakod)]
sjv.handelsvara <- sjv.handelsvara[!is.na(Handelsvarakod)]

sjv.handelsvara <-
  unique(sjv.handelsvara[,
                         c("names",
                           "Handelsvara.respiratory",
                           "Handelsvara.reproductive") :=
                           list(list(c(Handelsvara.Beskrivning)),
                                list(c(Handelsvara.respiratory))[[1]],
                                list(c(Handelsvara.reproductive))[[1]]),
                         by = Handelsvarakod] %>%
           .[, .(
             Handelsvara.Beskrivning = names,
             Handelsvarakod,
             Handelsvara.respiratory,
             Handelsvara.reproductive
           )],
         by = "Handelsvarakod")

# add diagnos and handelsvara classification columns to original data
# does not add list of possible beskrivning per handelsvarakod (for now)
sjv.with.diagnos <- merge(sjv.retro,
                          sjv.diagnos,
                          by = "Diagnoskod",
                          all.x = TRUE)

sjv.with.diagnos.handelsvara <- merge(
  sjv.with.diagnos,
  sjv.handelsvara[, -"Handelsvara.Beskrivning"],
  by = "Handelsvarakod",
  all.x = TRUE
)
sjv.with.diagnos.handelsvara[is.na(Handelsvara.respiratory)]$Handelsvara.respiratory <- 0
sjv.with.diagnos.handelsvara[is.na(Handelsvara.reproductive)]$Handelsvara.reproductive <- 0

sjv.respiratory <-
  sjv.with.diagnos.handelsvara[Diagnos.class == "Respiratory" |
                                 Handelsvara.respiratory > 0]
sjv.reproductive <-
  sjv.with.diagnos.handelsvara[Diagnos.class == "Reproductive" |
                                 Handelsvara.reproductive > 0]

sjv.any.code <- sjv.with.diagnos.handelsvara[!is.na(Diagnos.class)]


#### TIMESERIES ####

sva.all.dates <- data.table(date = seq.Date(min(sva.retro$date), max(sva.retro$date), by = "day"))
sjv.all.dates <- data.table(date = seq.Date(min(sjv.retro$date), max(sjv.retro$date), by = "day"))

sva.timeseries <- make_timeseries(sva.interesting, sva.all.dates)
sjv.respiratory.series <- make_timeseries(sjv.respiratory, sjv.all.dates)
sjv.reproductive.series <- make_timeseries(sjv.reproductive, sjv.all.dates)

sva.timeseries[, sum(N), by = yearweek] %>%
  ggplot(aes(yearweek, V1)) +
  geom_line(group=1)

sjv.respiratory.series[, sum(N), by = yearweek] %>%
  ggplot(aes(yearweek, V1)) +
  geom_line(group = 1)

sjv.reproductive.series[, sum(N), by = yearweek] %>%
  ggplot(aes(yearweek, V1)) +
  geom_line(group = 1)


