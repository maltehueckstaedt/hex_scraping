# Funktion zum sicheren Extrahieren von Text
get_element <- function(selector) {
  tryCatch({
    # Prüfe, ob das Element existiert
    if (length(rmdr$findElements(using = "css selector", selector)) == 0) {
      return(NA) # NA, wenn kein Element gefunden wird
    }

    # Finde das Haupt-Element
    parent_element <- rmdr$findElement(using = "css selector", selector)

    # Suche nach <li>-Child-Elementen
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



check_obj_exist <- function(df) {
  # Iteriere über die Spaltennamen
  for (col_name in names(df)) {
    cat("\033[33m>>", col_name, "<<\033[0m", "erfolgreich erhoben\n")
  }
}

#genericSearchMask\:search_e4ff321960e251186ac57567bec9f4ce\:cm_exa_eventprocess_basic_data\:fieldset\:inputField_4_abb156a1126282e4cf40d48283b4e76d\:idabb156a1126282e4cf40d48283b4e76d\:termSelect_11

choose_semester <- function(rmdr, semester_number) {
  # Seite navigieren
  rmdr$navigate("https://www.campo.fau.de/qisserver/pages/startFlow.xhtml?_flowId=searchCourseNonStaff-flow&_flowExecutionKey=e1s1")

  # Finde Dropdown-Menü für Semesterauswahl
  sem_dropdown <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_4_abb156a1126282e4cf40d48283b4e76d:idabb156a1126282e4cf40d48283b4e76d:termSelect_label"]')
  sem_dropdown$clickElement()

  # Wähle das Semester basierend auf der übergebenen Zahl
  semester_selector <- paste0('//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_4_abb156a1126282e4cf40d48283b4e76d:idabb156a1126282e4cf40d48283b4e76d:termSelect_', semester_number, '"]')
  sem <- rmdr$findElement(using = "xpath", semester_selector)
  sem$clickElement()

  # Finde das Feld "Suchbegriffe" über XPath
  Suchbegriffe <- rmdr$findElement(using = "xpath", '//*[@id="genericSearchMask:search_e4ff321960e251186ac57567bec9f4ce:cm_exa_eventprocess_basic_data:fieldset:inputField_0_1ad08e26bde39c9e4f1833e56dcce9b5:id1ad08e26bde39c9e4f1833e56dcce9b5"]')

  # Klicke in das Feld Suchbegriffe und drücke Enter
  Suchbegriffe$clickElement()
  Suchbegriffe$sendKeysToElement(list(key = "enter"))

  rmdr$executeScript("window.scrollTo(0, 0);")

  # finde Seitenanzahl-Feld
  # Finden des Eingabefeldes
  input_field <- rmdr$findElement(using = "css selector", "#genSearchRes\\:id3f3bd34c5d6b1c79\\:id3f3bd34c5d6b1c79Navi2NumRowsInput")

  # Zuerst zweimal die Backspace-Taste drücken
  input_field$sendKeysToElement(list(key = "control", "a", key = "backspace"))

  # Dann die Zahl 300 eingeben und Enter drücken
  input_field$sendKeysToElement(list("300", key = "enter"))

}


wait_for_element_and_click <- function(driver, using, value, action = NULL, timeout = 30, poll_interval = 0.5) {
  element <- NULL
  start_time <- Sys.time()

  while (is.null(element) && as.numeric(Sys.time() - start_time, units = "secs") < timeout) {
    element <- tryCatch(
      {
        driver$findElement(using = using, value = value)
      },
      error = function(e) {
        NULL
      }
    )
    if (is.null(element)) {
      Sys.sleep(poll_interval)  # Pause zwischen den Suchversuchen
    }
  }

  if (is.null(element)) {
    stop("Element konnte nicht innerhalb des Timeouts gefunden werden.")
  }

  if (!is.null(action) && action == "click") {
    element$clickElement()
  }

  return(element)
}
