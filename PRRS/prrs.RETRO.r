Sys.setlocale("LC_ALL", "swedish")

library(lubridate)
library(stringr)
library(data.table)
library(ggplot2)

data_path <-
  "I:/ESS/SVA3D/PRRS/data/" # path to data
csv_path <-
  "C:/Users/wiktor.gustafsson/SVA/PigPeaks - Documents/sysModel/data/" # path to output location for timeseries csv tables

make_timeseries <- function(data, daterange) {
  ret <-
    merge(data[, .N, by = date][order(date)],
          daterange,
          by = "date",
          all = TRUE)[, c("doy", "week", "month", "year") := list(
            as.numeric(strftime(date, format = "%j")),
            isoweek(date),
            lubridate::month(date),
            as.factor(lubridate::year(date))
          )]
  
  yearweek_levels <-
    levels(as.factor(str_c(
      as.character(ret$year), as.character(ret$week), sep = "-"
    )))
  yearmonth_levels <-
    levels(as.factor(str_c(
      as.character(ret$year), as.character(ret$month), sep = "-"
    )))
  
  ret <-
    ret[, c("yearweek", "yearmonth") :=  list(factor(str_c(
      as.character(year), as.character(week), sep = "-"
    ), levels = yearweek_levels),
    factor(str_c(
      as.character(year), as.character(month), sep = "-"
    ), levels = yearmonth_levels))]
  ret[is.na(N)]$N <- 0
  
  return(ret)
}

#### SVA DATA ####

html.pattern <- "<[^>]*>(.+)<[^>]*>"

sva.retro <-
  fread(
    paste0(data_path, "retro/sva.retro.2014-2018.txt"),
    header = TRUE,
    na.strings = "",
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    quote = "\"",
    data.table = TRUE
  )[str_detect(Uppdrag, html.pattern), Uppdrag := str_replace(Uppdrag, html.pattern, "\\1")] %>%
  .[, Ankomstdatum := as.Date(Ankomstdatum, "%m/%d/%y")] %>%
  .[is.na(Ankomstdatum), Ankomstdatum := as.Date(str_match(Uppdrag, "^U(\\d{6})")[, 2], format = "%y%m%d")] %>%
  setnames(old = "Ankomstdatum", new = "date")

# import list of possible agens and their relevance to PRRS
sva.agens <-
  fread(
    paste0(data_path, "tables/prrs.sva.agens.csv"),
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = ""
  )

# extract only the agens which have been marked relevant to PRRS, and discard irrelevant columns
sva.agens.relevant <- sva.agens %>%
  .[!(is.na(FINAL) |
        FINAL == "?" | FINAL == "x"), .(Agens, final = FINAL)] %>%
  .[, final := as.numeric(final)]

# repeat for unders?kningar
sva.undersokning <-
  unique(fread(
    paste0(data_path, "tables/prrs.sva.undersokning.csv"),
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = ""
  )) %>%
  .[, mean(as.numeric(FINAL)), by = Kod]

sva.undersokning.relevant <- sva.undersokning %>%
  .[!is.na(V1), .(undersokning = Kod, final = V1)] %>%
  .[, final := as.numeric(final)]

# Find rows in dataset that have relevant agens and unders?kning and attach value to them
sva.with.agens <- sva.agens.relevant[sva.retro, on = "Agens"]

sva.with.agens.undersokning <-
  sva.undersokning.relevant[sva.with.agens,
                            on = c("undersokning" = "Unders\u00F6kning.Kod",
                                   "undersokning" = "Analys.Kod")] %>%
  setnames(
    old = c("undersokning", "undersokning.1", "i.final", "final"),
    new = c(
      "Unders\u00F6kning.Kod",
      "Analys.Kod",
      "PRRSagens",
      "PRRSundersokning"
    )
  )


# set all other rows' values to 0
sva.with.agens.undersokning[is.na(PRRSagens)]$PRRSagens <- 0
sva.with.agens.undersokning[is.na(PRRSundersokning)]$PRRSundersokning <-
  0

# load table with classification of provtagningsorsaker
sva.provtagningsorsak <-
  fread(
    paste0(data_path, "tables/prrs.sva.provtagningsorsak.csv"),
    encoding = "UTF-8",
    colClasses = "character",
    na.strings = "",
    stringsAsFactors = FALSE
  )

# add provtagningsorsak class to the data
sva.agens.undersokning.origin <-
  merge(
    sva.with.agens.undersokning,
    sva.provtagningsorsak,
    all.x = T,
    by = "Provtagningsorsak.namn"
  )
sva.agens.undersokning.origin[is.na(ori)]$ori <- "Unclassified"

# extract the rows with a PRRS-relevant agens or unders?kning, and that are assumed to be passive surveillance cases
# (i.e. have a provtagningsorsak classified as passive surveillance or are missing an ?verordnat uppdrag)
sva.interesting <-
  sva.agens.undersokning.origin[(PRRSagens > 0 |
                                   PRRSundersokning > 0) &
                                  (ori == "Passive/unknown" |
                                     is.na(`Överordnade uppdrag`))]

#### SJV DATA ####

# read the data
sjv.retro <- fread(
  paste0(data_path, "retro/sjv.retro.3800-days.txt"),
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
  paste0(data_path, "tables/prrs.sjv.diagnos.csv"),
  encoding = "UTF-8",
  na.strings = "",
  stringsAsFactors = FALSE
)

# import handelsvara codes and their relevance to reproductive and/or respiratory diagnoses
sjv.handelsvara <-
  fread(
    paste0(data_path, "tables/prrs.sjv.handelsvara.csv"),
    encoding = "UTF-8",
    na.strings = "",
    stringsAsFactors = FALSE
  )
sjv.handelsvara[is.na(Handelsvara.respiratory)]$Handelsvara.respiratory <-
  0
sjv.handelsvara[is.na(Handelsvara.reproductive)]$Handelsvara.reproductive <-
  0

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

sjv.with.diagnos.handelsvara <- merge(sjv.with.diagnos,
                                      sjv.handelsvara[, -"Handelsvara.Beskrivning"],
                                      by = "Handelsvarakod",
                                      all.x = TRUE)
sjv.with.diagnos.handelsvara[is.na(Handelsvara.respiratory), Handelsvara.respiratory := 0]
sjv.with.diagnos.handelsvara[is.na(Handelsvara.reproductive), Handelsvara.reproductive := 0]

sjv.respiratory <-
  sjv.with.diagnos.handelsvara[Diagnos.class == "Respiratory" |
                                 Handelsvara.respiratory > 0]
sjv.reproductive <-
  sjv.with.diagnos.handelsvara[Diagnos.class == "Reproductive" |
                                 Handelsvara.reproductive > 0]

sjv.any.code <- sjv.with.diagnos.handelsvara[!is.na(Diagnos.class)]


#### TIMESERIES ####

sva.all.dates <-
  data.table(date = seq.Date(floor_date(min(sva.retro$date), "month"),
                             ceiling_date(max(sva.retro$date), "month") - 1,
                             by = "day"))

sjv.all.dates <-
  data.table(date = seq.Date(floor_date(min(sjv.retro$date), "month"), ceiling_date(max(sjv.retro$date), "month") - 1, by = "day"))

sva.timeseries <-
  make_timeseries(unique(sva.interesting, by = c("Ins\u00E4ntmaterial", "date")), sva.all.dates)
sjv.respiratory.series <-
  make_timeseries(unique(sjv.respiratory, by = c("Ppn", "date")), sjv.all.dates)
sjv.reproductive.series <-
  make_timeseries(unique(sjv.reproductive, by = c("Ppn", "date")), sjv.all.dates)
sjv.codes.series <-
  make_timeseries(unique(sjv.any.code, by = c("Ppn", "date")), sjv.all.dates)
sjv.all.series <-
  make_timeseries(unique(sjv.retro, by = c("Ppn", "date")), sjv.all.dates)
sjv.prop.series <-
  merge(sjv.codes.series, sjv.all.series[, .(date, N_tot = N)], by = "date") %>%
  .[, N_prop := N / N_tot] %>%
  .[is.nan(N_prop), N_prop := 0]

sva.timeseries[, sum(N), by = yearweek] %>%
  ggplot(aes(yearweek, V1)) +
  geom_line(group = 1)

sjv.respiratory.series[, sum(N), by = yearweek] %>%
  ggplot(aes(yearweek, V1)) +
  geom_line(group = 1)

sjv.reproductive.series[, sum(N), by = yearweek] %>%
  ggplot(aes(yearweek, V1)) +
  geom_line(group = 1)

# The weekly mean of proportions...
sjv.prop.series[, mean := mean(N_prop), by = yearweek] %>%
  ggplot(aes(yearweek, mean)) +
  geom_line(group = 1)

# ...or the proportions of weekly totals?
sjv.prop.series[, proportion := sum(N) / sum(N_tot), by = yearweek] %>%
  ggplot(aes(yearweek, proportion)) +
  geom_line(group = 1)

get_absweek <- function(daterange) {
  isoweeks <- isoweek(daterange)
  
  current_isoweek <- isoweeks[1]
  absweeks <- current_isoweek
  
  for (i in 2:length(isoweeks)) {
    new_isoweek <- isoweeks[i]
    
    if (new_isoweek == current_isoweek) {
      absweeks <- c(absweeks, tail(absweeks, 1))
    } else {
      absweeks <- c(absweeks, tail(absweeks, 1) + 1)
      current_isoweek <- new_isoweek
    }
  }
  
  return(data.table(date = daterange, absweek = absweeks))
  
}

absweeks <- get_absweek(seq.Date(min(
  min(sva.all.dates$date),
  min(sjv.all.dates$date)
),
max(
  max(sva.all.dates$date),
  max(sjv.all.dates$date)
),
by = "day"))

retro.sva <- merge(sva.timeseries,
                   absweeks,
                   by = "date",
                   all.x = TRUE) %>%
  .[, start_date := floor_date(date, "week", week_start = 1)] %>%
  .[, .(N = sum(N)),
    by = c("start_date",
           "absweek")] %>%
  .[, .(start_date,
        year = year(start_date),
        week = isoweek(start_date),
        absweek,
        N)]

retro.sjv.respiratory <- merge(sjv.respiratory.series,
                               absweeks,
                               by = "date",
                               all.x = TRUE) %>%
  .[, start_date := floor_date(date, "week", week_start = 1)] %>%
  .[, .(N = sum(N)),
    by = c("start_date",
           "absweek")] %>%
  .[, .(start_date,
        year = year(start_date),
        week = isoweek(start_date),
        absweek,
        N)]
retro.sjv.reproductive <- merge(sjv.reproductive.series,
                                absweeks,
                                by = "date",
                                all.x = TRUE) %>%
  .[, start_date := floor_date(date, "week", week_start = 1)] %>%
  .[, .(N = sum(N)),
    by = c("start_date",
           "absweek")] %>%
  .[, .(start_date,
        year = year(start_date),
        week = isoweek(start_date),
        absweek,
        N)]
retro.sjv.proportion <- merge(sjv.prop.series,
                              absweeks,
                              by = "date",
                              all.x = TRUE) %>%
  .[, start_date := floor_date(date, "week", week_start = 1)] %>%
  .[, .(N_prop = sum(N) / sum(N_tot),
        N_tot = sum(N_tot)),
    by = c("start_date",
           "absweek")] %>%
  .[, .(
    start_date,
    year = year(start_date),
    week = isoweek(start_date),
    absweek,
    N_prop,
    N_tot
  )]

save(
  retro.sva,
  retro.sjv.proportion,
  retro.sjv.respiratory,
  retro.sjv.reproductive,
  file = paste0(data_path,
                "prrs.retro.timeseries.RData")
)

# fwrite(
#   sva.output,
#   file = paste0(csv_path,
#                 "prrs.retro.sva.csv"),
#   sep = ",",
#   eol = "\r\n",
#   na = ""
# )
#
# fwrite(
#   sjv.respiratory.output,
#   file = paste0(csv_path,
#                 "prrs.retro.sjv.respiratory.csv"),
#   sep = ",",
#   eol = "\r\n",
#   na = ""
# )
#
# fwrite(
#   sjv.reproductive.output,
#   file = paste0(csv_path,
#                 "prrs.retro.sjv.reproductive.csv"),
#   sep = ",",
#   eol = "\r\n",
#   na = ""
# )
#
# fwrite(
#   sjv.prop.output,
#   file = paste0(csv_path,
#                 "prrs.retro.sjv.proportion.csv"),
#   sep = ",",
#   eol = "\r\n",
#   na = ""
# )
