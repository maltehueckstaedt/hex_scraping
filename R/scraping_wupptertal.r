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
 
# Finde das Feld "Suchbegriffe" über xpath
erweit_such <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_0_1ad08e26bde39c9e4f1833e56dcce9b5:id1ad08e26bde39c9e4f1833e56dcce9b5"]')

# Klicke in das Feld Suchbegriffe
erweit_such$clickElement()

# Drücke Enter im Feld Suchbegriffe, damit alle Vorlesungen angezeigt werden
erweit_such$sendKeysToElement(list(key = "enter"))




















 
# Finde Dropdownmenü für Sem-Auswahl
dropdown <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_3_abb156a1126282e4cf40d48283b4e76d:idabb156a1126282e4cf40d48283b4e76d:termSelect_label"]')
dropdown$clickElement()
# Wähle "Wintersemester 2023"
option <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_3_abb156a1126282e4cf40d48283b4e76d:idabb156a1126282e4cf40d48283b4e76d:termSelect_2"]')
option$clickElement()
 
# Löse Suche aus
suchen <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:buttonsBottom:search"]/span')
suchen$clickElement()
# Lese den HTML-Code ein
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