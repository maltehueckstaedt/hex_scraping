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
# Comment: 
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

 
# Starten einer Remote-Sitzung mit Chrome
driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.60", port = 1234L)

# Starte auf Mac
driver <- rsDriver(browser = "chrome", chromever = "131.0.6778.86", port = 1234L)

 
# Zugriff auf die gestartete Sitzung
rmdr <- driver[["client"]]
 
#////////////////////////////////////////////////////////////
## READ URLS ------------------------------------------------
#////////////////////////////////////////////////////////////

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Gehe zur Basisseite und wähle gewünschtes Semester
# aus.
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
rmdr$navigate("https://www.studilöwe.uni-wuppertal.de/qisserver/pages/cm/exa/coursemanagement/basicCourseData.xhtml?_flowId=searchCourseNonStaff-flow&_flowExecutionKey=e1s1")

# Finde Dropdownmenü für Sem-Auswahl
sem_dropdown <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_3_abb156a1126282e4cf40d48283b4e76d:idabb156a1126282e4cf40d48283b4e76d:termSelect_label"]')
sem_dropdown$clickElement()

# Wähle "Wintersemester 2023"
sem <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_3_abb156a1126282e4cf40d48283b4e76d:idabb156a1126282e4cf40d48283b4e76d:termSelect_2"]')
sem$clickElement()
 
# Finde das Feld "Suchbegriffe" über xpath
Suchbegriffe <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_0_1ad08e26bde39c9e4f1833e56dcce9b5:id1ad08e26bde39c9e4f1833e56dcce9b5"]')

# Klicke in das Feld Suchbegriffe
Suchbegriffe$clickElement()

# Drücke ohne weitere Eingabe >Enter< im Feld Suchbegriffe, damit alle Vorlesungen angezeigt werden
Suchbegriffe$sendKeysToElement(list(key = "enter"))

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
# Comment: Erzeuge leeren Tibble zum befüllen:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Initialisiere ein leeres tibble
ergebnisse <- tibble(
  Titel = character(),
  Nummer = character(),
  Organisationseinheit = character(),
  Veranstaltungsart = character(),
  Angebotshaeufigkeit = character(),
  Boxtitel = character(),
  Boxinhalt = character()
)

# Erzeuge Iteration-Zähler
iteration <- 1

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Erzeuge leeren Tibble zum befüllen:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

for (i in css_selectors) { 
  
  rmdr$maxWindowSize() # erzeuge maximale Fenstergröße damit alles Informationen gescrapet werden können.

  # Finde die Kurse auf der Überblicksseite
  kurs <- tryCatch({
    elem <- rmdr$findElement(using = "css selector", i)
    elem$clickElement()
  }, error = function(e) {
    message("\033[31m", "css zum Kurs nicht gefunden: ", i, "\033[0m")
    next
  })
  
  iteration <- iteration +1

  # Erzeuge Nachricht, welcher Kurs gescrapet wird:
  titel <- get_element('#\\31 8a8022569d6ced829f833aa855530ce')
  cat("\033[34m", paste0("Start das scraping von Kurs Nr.", iteration, ":"), "\033[0m",
    "\033[32m", titel, "\033[0m\n")

  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # 1. Tab: Semesterplanung
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # Extrahiere die gewünschten Informationen
  check_obj_exist(titel)

  nummer <- get_element('#a6b7089fcf43a67764ca850c1e4661d5')
  check_obj_exist(nummer)

  organisationseinheit <- get_element('ul.listStyleIconSimple:nth-child(1) > li:nth-child(1)')
  check_obj_exist(organisationseinheit)

  veranstaltungsart <- get_element('#\\34 fc695e29c07ca4ad6b71c515398e8e8')
  check_obj_exist(veranstaltungsart)

  angebotshaeufigkeit <- get_element('#\\37 fad543acae49a98047a57220463ecdd')
  check_obj_exist(angebotshaeufigkeit)

  for (i in 1:4) {
    feld_titel <- get_element(sprintf(
      "#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(%d) > label:nth-child(1)", 
      i
    ))
    feld_wert <- get_element(sprintf(
      "#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(%d) > div:nth-child(2)", 
      i
    ))

    # Dynamisch Objekte erstellen
    assign(paste0("feld_", i, "_titel"), feld_titel)
    assign(paste0("feld_", i, "_wert"), feld_wert)
    
    # Prüfen
    check_obj_exist_value(feld_titel)
  } 
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
    0:20
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
  cat("Anzahl der gefundenen Container:", found_containers, "\n")

  
  # Listen für die gesammelten Titel und Inhalte
  container_titles <- list()
  container_contents <- list()

  if (found_containers > 0) {
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
 
  
  # Erstelle ein tibble mit den extrahierten Daten
  neue_zeile <- tibble(
    Titel = titel,
    Nummer = nummer,
    Organisationseinheit = organisationseinheit,
    Veranstaltungsart = veranstaltungsart,
    Angebotshaeufigkeit = angebotshaeufigkeit,

  )


  #Dynamische Spalte nur hinzufügen, wenn feld_1_titel nicht NA ist
  # Liste der Felder (Titel und Werte)
  felder_titel <- list(feld_1_titel, feld_2_titel, feld_3_titel, feld_4_titel)
  felder_wert <- list(feld_1_wert, feld_2_wert, feld_3_wert, feld_4_wert)

 # Dynamische Spalten nur hinzufügen, wenn Titel nicht NA sind
  for (i in seq_along(felder_titel)) {
    if (!is.na(felder_titel[[i]]) && felder_titel[[i]] != "") {
      # Stelle sicher, dass Werte als Liste eingefügt werden
      neue_zeile <- neue_zeile %>% mutate(!!felder_titel[[i]] := list(felder_wert[[i]]))
    }
  }

  if (found_containers > 0) {
  neue_zeile <- bind_cols(neue_zeile,data_tibble)
  }
  print(neue_zeile)
  # Füge die neue Zeile zum bestehenden tibble hinzu
  ergebnisse <- bind_rows(ergebnisse, neue_zeile)
  
  # Gehe zurück zur Basisseite
  rmdr$goBack()
  rmdr$goBack()
}



rmdr$close() 
#system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# Kill all java Mac:
system("killall java", intern=FALSE, ignore.stdout=FALSE)

