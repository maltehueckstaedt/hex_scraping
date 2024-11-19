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
driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.60", port = 1231L)
 
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
# Gesamte Anzahl der Elemente
total_elements <- 2611

# Anzahl der Elemente pro Seite
elements_per_page <- 10

# Liste zur Speicherung der Seitenquellen
page_sources <- list()

# Äußerer Loop für Seiten
for (page in 0:(ceiling(total_elements / elements_per_page) - 1)) {
  # Innerer Loop für die Elemente auf der aktuellen Seite
  start_index <- page * elements_per_page
  print(start_index)
  end_index <- min(total_elements - 1, start_index + elements_per_page - 1) # Begrenzung auf die letzten Elemente
  print(end_index)
  for (i in start_index:end_index) {
    
    xpath_kurs <- sprintf('//*[@id="genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Table:%d:actionsLeft:show:link"]', i)
    
    # Versuche, das Element zu finden
    if (rmdr$findElements(using = "xpath", xpath_kurs) %>% length > 0) {
      Kurs <- rmdr$findElement(using = "xpath", xpath_kurs)
      Kurs$clickElement()
      
      # HTML der Seite auslesen und speichern
      page_source <- rmdr$getPageSource()[[1]]
      page_sources[[i + 1]] <- page_source
      Sys.sleep(1)
      
      # Versuche, den "Zurück"-Button zu klicken
      click_back_button(rmdr)
      Sys.sleep(1)  # Warten, um sicherzustellen, dass die Seite vollständig geladen ist
    } else {
      message(sprintf("Element mit Index %d wurde nicht gefunden", i))
    }
  }
  
  # "Weiter"-Button klicken, außer bei der letzten Seite
  if (page < ceiling(total_elements / elements_per_page) - 1) {
    next_button <- rmdr$findElement(using = "css selector", "#genSearchRes\\:id3df798d58b4bacd9\\:id3df798d58b4bacd9Navi2next")
    next_button$clickElement()
    Sys.sleep(2)  # Warten, bis die nächste Seite vollständig geladen ist
  }
}

message("Scraping abgeschlossen!")



 
  
driver$server$stop()
 