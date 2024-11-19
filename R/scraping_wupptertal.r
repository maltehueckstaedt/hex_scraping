#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD PACKAGES -------------------------------------------- 
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, RSelenium)
 
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD DATA ------------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: 
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
#////////////////////////////////////////////////////////////
# START RSELENIUM
#////////////////////////////////////////////////////////////
 
# Starten einer Remote-Sitzung mit Chrome
driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.60", port = 4111L)
 
# Zugriff auf die gestartete Sitzung
rmdr <- driver[["client"]]
 
#////////////////////////////////////////////////////////////
# READ URLS
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
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::s

page_sources <- list()  # Liste zur Speicherung der Seitenquellen
current_index <- 1      # Startindex für die Liste `page_sources`

# Anzahl der Elemente
total_elements <- 2612
elements_per_page <- 10

# Schleife über alle Seiten
for (page in 0:(ceiling(total_elements / elements_per_page) - 1)) {
  
  # Start- und Endindex für die aktuelle Seite berechnen
  start_index <- page * elements_per_page
  end_index <- min(start_index + elements_per_page - 1, total_elements - 1)
  
  # Schleife durch die Elemente auf der aktuellen Seite
  for (i in start_index:end_index) {
    # XPath für das aktuelle Element dynamisch erstellen
    xpath_kurs <- sprintf('//*[@id="genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Table:%d:tableRowAction"]', i)

    tryCatch({
      # Element finden und anklicken
      Kurs <- rmdr$findElement(using = "xpath", xpath_kurs)
      Kurs$clickElement()
      
      # HTML der Seite auslesen und speichern
      page_source <- rmdr$getPageSource()[[1]]
      page_sources[[current_index]] <- page_source
      current_index <- current_index + 1  # Index für gespeicherte Seiten erhöhen
      
      # Auf "Zurück" klicken
      back_button <- rmdr$findElement(using = "xpath", '//*[@id="form:dialogHeader:backButtonTop"]')
      back_button$clickElement()
      Sys.sleep(.5)  # Kurze Pause zwischen Aktionen
    }, error = function(e) {
      # Fehlerbehandlung, wenn ein Element nicht gefunden wird
      message(sprintf("Fehler bei Element %d auf Seite %d: %s", i, page + 1, e$message))
    })
  }

  # Klick auf "Weiter" für die nächste Seite, außer bei der letzten Seite
  if (end_index < total_elements - 1) {
    tryCatch({
      next_button <- rmdr$findElement(using = "xpath", '//*[@id="genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Navi2next"]')
      next_button$clickElement()
      Sys.sleep(1)  # Wartezeit, um die nächste Seite vollständig zu laden
    }, error = function(e) {
      # Fehlerbehandlung beim Klicken auf "Weiter"
      message(sprintf("Fehler beim Klicken auf 'Weiter' auf Seite %d: %s", page + 1, e$message))
    })
  }
}










 
  
driver$server$stop()
rmdr$close()  # Schließt das aktuelle Browserfenster
rmdr$quit()   # Beendet den Selenium-Server und gibt den Port frei