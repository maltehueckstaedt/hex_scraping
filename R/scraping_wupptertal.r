#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD PACKAGES -------------------------------------------- 
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, RSelenium,rlang)
 
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# START RSELENIUM ---------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: 
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

 
# Starten einer Remote-Sitzung mit Chrome
driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.60", port = 1234L)
 
# Zugriff auf die gestartete Sitzung
rmdr <- driver[["client"]]
 
#////////////////////////////////////////////////////////////
## READ URLS -------------------------------------------------
#////////////////////////////////////////////////////////////
 
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
# Comment: 
# Die Vorlesungen werden angezeigt. Immer zehn Stück pro Seite
# bei insgesamt ca. 200-300 Seiten. Es muss nun jede Seite
# angeklickt werden und der Sourcecode geladen werden:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
css_selectors <- sprintf(
  "#genSearchRes\\:id3df798d58b4bacd9\\:id3df798d58b4bacd9Table\\:%d\\:tableRowAction",
  0:9
)

# Initialisiere ein leeres tibble

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

for (i in css_selectors) {
  rmdr$maxWindowSize()
  #browser()
  # Finde die Kurse auf der Überblicksseite
  kurs <- tryCatch({
    elem <- rmdr$findElement(using = "css selector", i)
    elem$clickElement()
    elem
  }, error = function(e) {
    message("Kurs nicht gefunden: ", i)
    next
  })
  
  # Funktion zum sicheren Extrahieren von Text
safe_get_text <- function(selector) {
  tryCatch({
    # Prüfe, ob das Element existiert
    if (length(rmdr$findElements(using = "css selector", selector)) == 0) {
      return(NA) # NA, wenn kein Element gefunden wird
    }
    
    # Finde das Haupt-Element
    parent_element <- rmdr$findElement(using = "css selector", selector)
    
    # Suche nach <li>-Kind-Elementen
    list_items <- parent_element$findChildElements(using = "css selector", "li")
    
    if (length(list_items) > 0) {
      # Extrahiere den Text von jedem <li>-Element
      sapply(list_items, function(item) {
        item$getElementText()[[1]]
      })
    } else {
      # Extrahiere den Text des Haupt-Elements, wenn keine <li>-Elemente vorhanden sind
      parent_element$getElementText()[[1]]
    }
  }, error = function(e) {
    NA # Gibt NA bei Fehlern zurück
  })
}


  ################################################################
  # 1. Tab: Semesterplanung
  ################################################################
  # Extrahiere die gewünschten Informationen
  titel <- safe_get_text('#\\31 8a8022569d6ced829f833aa855530ce')
  nummer <- safe_get_text('#a6b7089fcf43a67764ca850c1e4661d5')
  organisationseinheit <- safe_get_text('ul.listStyleIconSimple:nth-child(1) > li:nth-child(1)')
  veranstaltungsart <- safe_get_text('#\\34 fc695e29c07ca4ad6b71c515398e8e8')
  angebotshaeufigkeit <- safe_get_text('#\\37 fad543acae49a98047a57220463ecdd')

  feld_1_titel <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > label:nth-child(1)')
  print(feld_1_titel)
  feld_1_wert <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2)')
  print(feld_1_wert)

  feld_2_titel <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > label:nth-child(1)')
  print(feld_2_titel)
  feld_2_wert <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2)')
  print(feld_2_wert)
 
  feld_3_titel <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(3) > label:nth-child(1)')
  print(feld_3_titel)
  feld_3_wert <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(3) > div:nth-child(2)')
  print(feld_3_wert)

  feld_4_titel <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(4) > label:nth-child(1)')
  print(feld_4_titel)
  feld_4_wert <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:parallelGroupSchedule_1\\:basicDataFieldset\\:basicDataFieldset_innerFieldset > div:nth-child(1) > div:nth-child(1) > div:nth-child(4) > div:nth-child(2)')
  print(feld_4_wert)

  tryCatch({
    inhalte_tab <- rmdr$findElement(using = "css selector", '#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:contentsTab')
    inhalte_tab$clickElement()
  }, error = function(e) {
    message("Inhalte-Tab nicht gefunden.")
  })
  
  boxtitel <- safe_get_text('div.box_title:nth-child(2)')
  boxinhalt <- safe_get_text('#detailViewData\\:tabContainer\\:term-planning-container\\:j_id_6m_13_2_0_1\\:collapsiblePanel > div:nth-child(3)')
  
  # Erstelle ein tibble mit den extrahierten Daten
  neue_zeile <- tibble(
    Titel = titel,
    Nummer = nummer,
    Organisationseinheit = organisationseinheit,
    Veranstaltungsart = veranstaltungsart,
    Angebotshaeufigkeit = angebotshaeufigkeit,

    Boxtitel = boxtitel,
    Boxinhalt = boxinhalt

  )

  #Dynamische Spalte nur hinzufügen, wenn feld_1_titel nicht NA ist
  # Liste der Felder (Titel und Werte)
  felder_titel <- list(feld_1_titel, feld_2_titel, feld_3_titel, feld_4_titel)
  felder_wert <- list(feld_1_wert, feld_2_wert, feld_3_wert, feld_4_wert)

  # Dynamische Spalten nur hinzufügen, wenn Titel nicht NA sind
 # Dynamische Spalten nur hinzufügen, wenn Titel nicht NA sind
  for (i in seq_along(felder_titel)) {
    if (!is.na(felder_titel[[i]]) && felder_titel[[i]] != "") {
      # Stelle sicher, dass Werte als Liste eingefügt werden
      neue_zeile <- neue_zeile %>% mutate(!!felder_titel[[i]] := list(felder_wert[[i]]))
    }
  }
    
  # Füge die neue Zeile zum bestehenden tibble hinzu
  ergebnisse <<- bind_rows(ergebnisse, neue_zeile)
  
  # Gehe zurück zur Basisseite
  rmdr$goBack()
  rmdr$goBack()
}



rmdr$close() 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
