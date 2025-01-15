# scrape.R
library(rvest)

# URL der Website
url <- "https://gepris.dfg.de/gepris/OCTOPUS"

# HTML-Inhalt abrufen
page <- read_html(url)

# Beispiel: Überschriften extrahieren
titles <- page %>%
  html_nodes("h1") %>%
  html_text()

# Ergebnis speichern
write.csv(titles, "titles.csv")
