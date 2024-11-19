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

# Kurs <- rmdr$findElement(using = "xpath", '//*[@id="genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Table:0:row"]/td[3]')
# Kurs$clickElement()

# # Gesamte HTML der Seite auslesen
# page_source <- rmdr$getPageSource()[[1]]




# Vektor, um die HTML-Inhalte zu speichern
page_sources <- list()

# Schleife durch die 10 Elemente (Index von 0 bis 9)
for (i in 0:9) {
  # XPath für das aktuelle Element dynamisch erstellen
  xpath_kurs <- sprintf('//*[@id="genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Table:%d:tableRowAction"]', i)

  # Element finden und anklicken
  Kurs <- rmdr$findElement(using = "xpath", xpath_kurs)
  Kurs$clickElement()
  
  # HTML der Seite auslesen und speichern
  page_source <- rmdr$getPageSource()[[1]]
  page_sources[[i + 1]] <- page_source
  
  # Auf "Zurück" klicken
  back_button <- rmdr$findElement(using = "xpath", '//*[@id="form:dialogHeader:backButtonTop"]')
  back_button$clickElement()
  Sys.sleep(.5)  # 2 Sekunden warten (kann angepasst werden)
}
# Überprüfen der gesammelten HTML-Daten
str(page_sources)











 
# page_source <- rmdr$getPageSource() |> unlist()
 
elements <- rmdr$findElements(using = 'css selector', value = '.linkTable span')
 
 
# Klicke nacheinander auf jedes Element
for (i in seq_along(elements)) {
  elements[[i]]$clickElement()
  # Optional: Warte zwischen den Klicks (z.B. 2 Sekunden, um eine Seite zu laden)
  Sys.sleep(2)
  # Navigiere ggf. wieder zurück, falls du auf eine andere Seite geleitet wirst
  rmdr$goBack()
  # Wiederhole das Finden der Elemente, da sich die Seite nach dem Klick geändert haben könnte
  elements <- rmdr$findElements(using = 'css selector', value = '.linkTable span')
}
 
  
driver$server$stop()
rmdr$close()  # Schließt das aktuelle Browserfenster
rmdr$quit()   # Beendet den Selenium-Server und gibt den Port frei