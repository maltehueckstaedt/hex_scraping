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
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Finde den ersten Kurs über CSS
kurs <- rmdr$findElement(using = "css selector", '#genSearchRes\\:id3df798d58b4bacd9\\:id3df798d58b4bacd9Table\\:0\\:tableRowAction')
kurs$clickElement()


# finde titel
titel <- rmdr$findElement(using = "css selector", '#\\31 8a8022569d6ced829f833aa855530ce')
# ziehe titel
titel <- titel$getElementText()


# finde nummer
nummer <- rmdr$findElement(using = "css selector", '#a6b7089fcf43a67764ca850c1e4661d5')
# ziehe nummer
nummer <- nummer$getElementText()

 
# finde Organisationseinheit
Organisationseinheit <- rmdr$findElement(using = "css selector", 'ul.listStyleIconSimple:nth-child(1) > li:nth-child(1)')
# ziehe Organisationseinheit
Organisationseinheit <- Organisationseinheit$getElementText()


# finde Veranstaltungsart
Veranstaltungsart <- rmdr$findElement(using = "css selector", '#\\34 fc695e29c07ca4ad6b71c515398e8e8')
# ziehe Organisationseinheit
Veranstaltungsart <- Veranstaltungsart$getElementText()


 # finde Angebotshaeufigkeit
Angebotshaeufigkeit <- rmdr$findElement(using = "css selector", '#\\37 fad543acae49a98047a57220463ecdd')
# ziehe Organisationseinheit
Angebotshaeufigkeit <- Angebotshaeufigkeit$getElementText()
 
# Finde "Inhalte"-Tab und klicke darauf
kurs <- rmdr$findElement(using = "css selector", '#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:contentsTab')
kurs$clickElement()

# finde Boxtitel
Boxtitel <- rmdr$findElement(using = "css selector", 'div.box_title:nth-child(2)')
# ziehe Organisationseinheit
Boxtitel <- Boxtitel$getElementText()

# finde Boxinhalt
Boxinhalt <- rmdr$findElement(using = "css selector", '#detailViewData\\:tabContainer\\:term-planning-container\\:j_id_6m_13_2_0_1\\:collapsiblePanel > div:nth-child(3)')
# ziehe Organisationseinheit
Boxinhalt <- Boxinhalt$getElementText()


driver$server$stop()
 