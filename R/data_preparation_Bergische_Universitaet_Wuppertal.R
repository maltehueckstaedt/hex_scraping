#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD PACKAGES -------------------------------------------- 
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, naniar)

#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD DATA ------------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////

# Pfad zu den RDS-Dateien
path <- "C:/SV/HEX/Scraping/data/single_universities/Friedrich-Schiller-Universitaet_Jena/"

# Einlesen der RDS-Dateien und Hinzufügen der Dateinamen als neue Spalte
data_list <- list.files(path = path, pattern = "^courses.*\\.rds$", full.names = TRUE) |>
  map(~ mutate(readRDS(.), source = basename(.)))

# Zusammenführen der DataFrames
raw_data_jena <- bind_rows(data_list)

#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# CHECK DATA ------------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Check for NAs: 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

colnames(raw_data_jena)
vis_miss(raw_data_jena)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: this columns that are 100% NA: 
# - hochschule 
# - veranstaltungstyp
# - ects
# - lernziele
# - lernmethode
# - zusatzinformationen
# - anmerkungen
# - primaersprache
# - mehrsprachig

# Remove this columns from raw_data_jena!
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::-

raw_data_jena <- raw_data_jena |> select(-c(hochschule, 
                                            veranstaltungstyp,
                                            ects,
                                            lernziele,
                                            lernmethode,
                                            zusatzinformationen,
                                            anmerkungen,
                                            primaersprache,
                                            mehrsprachig))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Check all other variables for unique values:
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

for (i in colnames(raw_data_jena)) {
  unique_values <- unique(raw_data_jena[[i]])

  cat("\033[34mColumn:", i, "\033[0m\n")  

  if (length(unique_values) > 10) {
    print(unique_values[1:10])
    cat("\033[31m... (Attention: There are more unique values!)\033[0m\n")
  } else {
    print(unique_values)   
  }
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: removes whitespace at the start and end, and 
# replaces all internal whitespace with a single space
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_data_jena <- raw_data_jena |> 
  mutate(across(where(is.character), ~ str_squish(.)))


#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# DATA PREPARATION ------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////

colnames(raw_data_jena)

# --------------------- fakultaet ---------------------------

tester_df <- raw_data_jena |> filter(is.na(fakultaet))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: three NAs on fakultaet that have NA on all columns 
# on all columns. Remove:
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_data_jena <- raw_data_jena |> filter(!is.na(fakultaet))

# --------------------- institut ----------------------------

tester_df <- raw_data_jena |> filter(is.na(institut))
tester_df <- raw_data_jena |> select(institut, url)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: split at "//"" and create a list from the resulting
# elements:
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_data_jena <- raw_data_jena |> 
  mutate(institut = str_split(institut, " \\\\ "))

# --------------------- organisation ----------------------------

tester_df <- raw_data_jena |> filter(is.na(organisation))

raw_data_jena <- raw_data_jena |> 
  mutate(organisation = str_remove(organisation, "Vorlesungsverzeichnis \\\\")) |> 
  mutate(organisation = str_replace_all(organisation, "\\\\", ">"))

# --------------------- studiengaenge ----------------------------

tester_df <- raw_data_jena |> filter(is.na(studiengaenge))
class(raw_data_jena$studiengaenge)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: studiengaenge contains list with tibbles and nulls. 
# there are four columns in the tibbles, e.g. like this: 
# [
# {
# Abschluss: "Lehramt JM Regelschule",
# Studiengang: "Chemie",
# Semester: "-",
# Prüfungsversion: 2007
# },
# {
# Abschluss: "Lehramt JM Gymnasium",
# Studiengang: "Chemie",
# Semester: "-",
# Prüfungsversion: 2007
# }
# ]

# In the following, due to multiple occurrences of degree programs 
# in one line, degree and study program are combined and separated by a 
# ": "

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_data_jena <- raw_data_jena %>%
  mutate(studiengang = map(studiengaenge, ~{
    if (is.null(.x)) {
      return(NA)  # NULL durch NA ersetzen
    } else {
      return(paste(.x$Abschluss, .x$Studiengang, sep = ": "))  # Abschluss und Studiengang mit Trennzeichen
    }
  }))


# --------------------- titel ----------------------------

raw_data_jena  |> sample_n(100) |> pull(titel)
table(is.na(raw_data_jena$titel))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- nummer ----------------------------

raw_data_jena  |> sample_n(100) |> pull(nummer)
table(is.na(raw_data_jena$nummer))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- url ----------------------------

raw_data_jena  |> sample_n(100) |> pull(url)
table(is.na(raw_data_jena$url))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- dozierende ----------------------------

raw_data_jena  |> sample_n(100) |> pull(dozierende)
table(is.na(raw_data_jena$dozierende))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Separate lecturers at “\\” and put the individual
# string elements into a list. Remove the pattern “ -- NA \\\\”
# as well as and replaces all internal whitespace with a single space
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_data_jena <- raw_data_jena |> 
  mutate(dozierende = map(str_split(dozierende, " \\\\ "), 
                              ~ .x |> str_remove_all("\\\\+|--|NA+") |> str_squish()))


# --------------------- veranstaltungsart ----------------------------

raw_data_jena  |> sample_n(100) |> pull(veranstaltungsart)
table(is.na(raw_data_jena$veranstaltungsart))
table(raw_data_jena$veranstaltungsart)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- sprache ----------------------------

raw_data_jena  |> sample_n(100) |> pull(sprache)
table(is.na(raw_data_jena$sprache))
table(raw_data_jena$sprache)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: set invalid values to NA. Set "Franz" to "Französich"
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

raw_data_jena <- raw_data_jena %>%
  mutate(sprache = if_else(sprache %in% c("http", "https"), NA_character_, sprache)) |>
  mutate(sprache = str_replace(sprache, "Franz", "Französisch"))

# --------------------- sws ----------------------------

raw_data_jena  |> sample_n(100) |> pull(sws)
table(is.na(raw_data_jena$sws))
table(raw_data_jena$sws)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
# --------------------- kursbeschreibung ----------------------------

raw_data_jena  |> sample_n(100) |> pull(kursbeschreibung)
table(is.na(raw_data_jena$kursbeschreibung))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- literatur ----------------------------

raw_data_jena  |> sample_n(100) |> pull(literatur)
table(is.na(raw_data_jena$literatur))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- voraussetzungen ----------------------------

raw_data_jena  |> sample_n(100) |> pull(voraussetzungen)
table(is.na(raw_data_jena$voraussetzungen))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- teilnehmerzahl ----------------------------

raw_data_jena  |> sample_n(100) |> pull(teilnehmerzahl)
table(is.na(raw_data_jena$teilnehmerzahl))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# --------------------- pruefung ----------------------------

raw_data_jena  |> sample_n(100) |> pull(pruefung)
table(is.na(raw_data_jena$pruefung))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- scrape_datum ----------------------------

raw_data_jena  |> sample_n(100) |> pull(scrape_datum)
table(is.na(raw_data_jena$scrape_datum))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- semester ----------------------------

raw_data_jena  |> sample_n(100) |> pull(semester)
unique(raw_data_jena$semester)
table(is.na(raw_data_jena$semester))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- jahr ----------------------------

raw_data_jena  |> sample_n(100) |> pull(jahr)
unique(raw_data_jena$jahr)
table(is.na(raw_data_jena$jahr))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- hyperlink ----------------------------

raw_data_jena  |> sample_n(1000) |> pull(hyperlink)
unique(raw_data_jena$hyperlink)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Junk-Variable (?): Only the levels: 
# [1] ""
# [2] "Veranstaltung aufzeichnen/streamen"
# [3] "https://www.rewi.uni-jena.de/studium/juristisches-lernen-und-wissenschaftliches-arbeiten"
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- rhythmus ----------------------------

raw_data_jena  |> sample_n(1000) |> pull(rhythmus)
raw_data_jena <- raw_data_jena |> 
  mutate(rhythmus = str_split(rhythmus, "\\|"))  

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Separate rhythm patterns at “|” and write 
# individual elements in a list
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- source ----------------------------

raw_data_jena  |> sample_n(1000) |> pull(source)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# --------------------- studiengang ----------------------------

raw_data_jena  |> sample_n(1000) |> pull(studiengang)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Looks good, nothing else to do.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
# creating codebook ------------------------------------------------------------
codebook_universitaet_jena <- tibble(Variablen = colnames(raw_data_jena))
colnames(raw_data_jena)
# data wrangling for hex database ----------------------------------------------

db_data_universitaet_jena <- tibble(id = 1:nrow(raw_data_jena))
db_data_universitaet_jena$hochschule <- rep("Friedrich-Schiller-Universität Jena.R", nrow(raw_data_jena))
db_data_universitaet_jena$fakultaet <- raw_data_jena$fakultaet
db_data_universitaet_jena$fachbereich <- NA
db_data_universitaet_jena$MINT_MN <- NA
db_data_universitaet_jena$MINT_T <- NA
db_data_universitaet_jena$institut <- raw_data_jena$institut
db_data_universitaet_jena$organisation <- NA
db_data_universitaet_jena$pfad <- raw_data_jena$organisation
db_data_universitaet_jena$studiengaenge <- raw_data_jena$studiengaenge
db_data_universitaet_jena$titel <- raw_data_jena$titel
db_data_universitaet_jena$nummer <- raw_data_jena$nummer
db_data_universitaet_jena$url <- raw_data_jena$url
db_data_universitaet_jena$dozierende <- raw_data_jena$dozierende
db_data_universitaet_jena$veranstaltungsart <- raw_data_jena$veranstaltungsart
db_data_universitaet_jena$kursformat <- NA
db_data_universitaet_jena$lehrtyp <- NA
db_data_universitaet_jena$sprache <- raw_data_jena$sprache
db_data_universitaet_jena$primaersprache <-NA
db_data_universitaet_jena$sws <- raw_data_jena$sws
db_data_universitaet_jena$ects <- NA
db_data_universitaet_jena$kursbeschreibung <- raw_data_jena$kursbeschreibung
db_data_universitaet_jena$lernziele <- NA
db_data_universitaet_jena$lernmethode <- NA
db_data_universitaet_jena$literatur <- raw_data_jena$literatur
db_data_universitaet_jena$voraussetzungen <- raw_data_jena$voraussetzungen
db_data_universitaet_jena$zusatzinformationen <- NA
db_data_universitaet_jena$anmerkungen <- NA
db_data_universitaet_jena$teilnehmerzahl <- raw_data_jena$teilnehmerzahl
db_data_universitaet_jena$pruefung <- raw_data_jena$pruefung
db_data_universitaet_jena$scrape_datum <- raw_data_jena$scrape_datum
db_data_universitaet_jena$semester <- raw_data_jena$semester
db_data_universitaet_jena$jahr <- raw_data_jena$jahr

db_data_universitaet_jena$id <- NULL

# save/export data -------------------------------------------------------------

saveRDS(raw_data_jena, "C:/SV/HEX/Scraping/data/single_universities/Friedrich-Schiller-Universitaet_Jena/data_friedrich-schiller-universitaet_jena.rds")
saveRDS(codebook_universitaet_jena, "C:/SV/HEX/Scraping/data/single_universities/Friedrich-Schiller-Universitaet_Jena/codebook_friedrich-schiller-universitaet_jena.rds")
saveRDS(db_data_universitaet_jena, "C:/SV/HEX/Scraping/data/single_universities/Friedrich-Schiller-Universitaet_Jena/db_data_friedrich-schiller-universitaet_jena.rds")
