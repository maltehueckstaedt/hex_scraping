library(rvest)
library(tidyverse)
library(chromote) 

# öffne Stamm URL
sess <- read_html_live("https://www.studilöwe.uni-wuppertal.de/qisserver/pages/cm/exa/coursemanagement/basicCourseData.xhtml?_flowId=searchCourseNonStaff-flow&_flowExecutionKey=e1s1")
sess$view()

# wähle dropdown
sess$click('#genericSearchMask\\:search_e4ff321960e251186ac57567bec9f4ce\\:cm_exa_eventprocess_basic_data\\:fieldset\\:inputField_3_abb156a1126282e4cf40d48283b4e76d\\:idabb156a1126282e4cf40d48283b4e76d\\:termSelect_label')

# wähle WS23
sess$click('#genericSearchMask\\:search_e4ff321960e251186ac57567bec9f4ce\\:cm_exa_eventprocess_basic_data\\:fieldset\\:inputField_3_abb156a1126282e4cf40d48283b4e76d\\:idabb156a1126282e4cf40d48283b4e76d\\:termSelect_2')

# Klicke ins Suchbegriffe-Fenster und drücke Enter
sess$press(
  css = "#genericSearchMask\\:search_e4ff321960e251186ac57567bec9f4ce\\:cm_exa_eventprocess_basic_data\\:fieldset\\:inputField_0_1ad08e26bde39c9e4f1833e56dcce9b5\\:id1ad08e26bde39c9e4f1833e56dcce9b5",  # CSS-Selektor des Elements
  key_code = "Enter"                # Schlüssel für die Enter-Taste
)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: 
# Die Vorlesungen werden nun angezeigt. Immer zehn Stück pro Seite
# bei insgesamt ca. 200-300 Seiten. Es muss nun jede Seite
# angeklickt werden und der Sourcecode geladen werden:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sess$click('//*[@id="genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Table:0:tableRowAction"]')

 
 #genSearchRes\:id3df798d58b4bacd9\:id3df798d58b4bacd9Table\:0\:tableRowAction span
 