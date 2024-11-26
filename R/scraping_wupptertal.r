#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD PACKAGES/FUNCTIONS -----------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, RSelenium,rlang)

source("R/functions/helper_functions.r")

#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# START RSELENIUM -------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Starte Browser
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Starten einer Remote-Sitzung mit Chrome auf Privat-PC
# driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.60", port = 1234L)

# Starten einer Remote-Sitzung mit Chrome auf Abeits-PC
driver <- rsDriver(browser = "chrome", chromever = "131.0.6778.85", port = 1234L)

 
# Zugriff auf die gestartete Sitzung
rmdr <- driver[["client"]]
rmdr$maxWindowSize() # erzeuge maximale Fenstergröße damit alles Informationen gescrapet werden können.

 
#////////////////////////////////////////////////////////////
## SCRAPE DATA ----------------------------------------------
#////////////////////////////////////////////////////////////

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Gehe zur Basisseite und wähle gewünschtes Semester
# aus.
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
# jüngstens Semester == 0, ältestestes derzeit == 16
# Parameter muss genau über Chrome verifiziert werden
choose_semester(rmdr, 2)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Die Vorlesungen werden angezeigt. Immer zehn Stück 
# pro Seite bei insgesamt ca. 200-300 Seiten. Erzeuge 
# Selektoren der Links zu den Kursen:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

css_selectors <- sprintf(
  "#genSearchRes\\:id3df798d58b4bacd9\\:id3df798d58b4bacd9Table\\:%d\\:tableRowAction",
  0:9
)
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Erzeuge Iteration-Zähler, für das Durchszählen der 
# Kurse im Output (für Vereinfacherung möglichen Debuggings)
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

iteration <- 1

ergebnisse <- tibble()

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Betätigt Kurs-Selektor
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

for (i in css_selectors) { 
  
  print(i)
  # Finde die Kurse auf der Überblicksseite
  kurs <- tryCatch({
    elem <- rmdr$findElement(using = "css selector", i)
    elem$clickElement()
  }, error = function(e) {
    message("\033[31m", "css zum Kurs nicht gefunden: ", i, "\033[0m")
    next
  })
  
  iteration <- iteration + 1 # Aktualisiere Iteration

  # Erzeuge Nachricht, welcher Kurs gescrapet wird:
  titel <- get_element('#\\31 8a8022569d6ced829f833aa855530ce')
  cat("\033[34m", paste0("Start das scraping von Kurs Nr.", iteration, ":"), "\033[0m",
    "\033[32m", titel, "\033[0m\n")

  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 1. Tab: Parallelgruppen/Termine. Extrahiere die gewünschten 
  # Informationen und bestätige Extraktion. Starte mit 
  # >>Grunddaten<<
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  labels <- rmdr$findElements(using = "css selector", ".labelItemLine label")
  label_texts <- sapply(labels, function(el) el$getElementText())
  answers <- rmdr$findElements(using = "css selector", ".labelItemLine .answer")
  answer_texts <- sapply(answers, function(el) el$getElementText())

  base_info_df <- tibble(
    Label = label_texts,
    Answer = answer_texts) %>%
    # Gruppiere nach Label, um mehrfach vorkommende Einträge zusammenzufassen
    group_by(Label) %>%
    summarise(Answer = list(unique(Answer)), .groups = "drop") %>%
    # In Long-Format überführen
    pivot_wider(names_from = Label, values_from = Answer)

  # Erstelle einen Tibble mit den extrahierten Daten
  neue_zeile <- base_info_df

  # Printe welche Variablen gescrapet wurden
  check_obj_exist(base_info_df)

  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 2. Tab: Inhalte
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  tryCatch({
    inhalte_tab <- rmdr$findElement(using = "css selector", '#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:contentsTab')
    inhalte_tab$clickElement()
  }, error = function(e) {
    message("Inhalte-Tab nicht gefunden.")
  })
  
  # Erzeuge die Liste der XPaths für die spezifischen IDs
  container_ids <- sprintf(
    '//*[@id="detailViewData:tabContainer:term-planning-container:j_id_6m_13_2_%d_1"]',
    0:20 # Suche sicherheitshalber in bis zu 20 Elementen mit fortlaufender Nummerierung des obigen selectors
  )

  # Zähle die Container, die tatsächlich auf der Seite existieren
  found_containers <- 0

  for (xpath in container_ids) {
    elements <- rmdr$findElements(using = "xpath", xpath)
    if (length(elements) > 0) {
      found_containers <- found_containers + 1
    }
  }

  # Anzahl der gefundenen Container ausgeben
  cat("Anzahl der gefundenen Container im Tab Inhalt:", "\033[32m", found_containers, "\033[0m", "\n")

  # Listen für die gesammelten Titel und Inhalte
  container_titles <- list()
  container_contents <- list()

  if (found_containers > 0) {
    cat("\033[32mEs werden folgende Variablen gescrapt:\033[0m\n")
    for (i in 1:found_containers) {
      # Dynamische Erstellung der XPathes für Titel und Inhalt
      title_xpath <- sprintf(
        '/html/body/div[1]/div[3]/div/div[1]/div/div/form[3]/div[2]/div/div/fieldset/div[4]/div/div[2]/fieldset/div[%d]/div/div/div/div[1]/div/div[2]/h2',
        i + 3 # Start bei 4, daher Offset 3
      )
      content_xpath <- sprintf(
        '/html/body/div[1]/div[3]/div/div[1]/div/div/form[3]/div[2]/div/div/fieldset/div[4]/div/div[2]/fieldset/div[%d]/div/div/div/div[2]',
        i + 3
      )

      # Extrahiere den Titel
      title_elements <- rmdr$findElements(using = "xpath", title_xpath)
      if (length(title_elements) > 0) {
        container_titles[[i]] <- title_elements[[1]]$getElementText()[[1]]
      } else {
        container_titles[[i]] <- NA
      }

      # Extrahiere den Inhalt
      content_elements <- rmdr$findElements(using = "xpath", content_xpath)
      if (length(content_elements) > 0) {
        container_contents[[i]] <- content_elements[[1]]$getElementText()[[1]]
      } else {
        container_contents[[i]] <- NA
      }
    }
  }

  # Erstelle ein tibble aus den extrahierten Daten
  if (found_containers > 0) {
    # Überprüfen, ob die Listen für Titel und Inhalte gleich lang sind
    if (length(container_titles) == length(container_contents)) {
      # Erstelle ein tibble mit Titeln als Spaltennamen und den entsprechenden Inhalten
      data_tibble <- tibble::tibble(
        !!!setNames(container_contents, container_titles)
      )
    } else {
      stop("Die Anzahl der Titel und Inhalte stimmt nicht überein.")
    }
  } else {
    data_tibble <- tibble() # Leeres tibble, falls keine Container gefunden wurden
  }
  
  # Printe welche Variablen gescrapet wurden
  check_obj_exist(data_tibble)

  if (found_containers > 0) {
  neue_zeile <- bind_cols(neue_zeile,data_tibble)
  }
  
  # Füge die neue Zeile zum bestehenden tibble hinzu
  ergebnisse <- bind_rows(ergebnisse, neue_zeile)
  
  # Gehe zurück zur Basisseite
  rmdr$goBack()
  rmdr$goBack()
}

rmdr$close() 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# Kill all java Mac:
# system("killall java", intern=FALSE, ignore.stdout=FALSE)