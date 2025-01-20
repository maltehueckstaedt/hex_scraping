# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# LOAD PACKAGES/FUNCTIONS -----------------------------------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, RSelenium, rlang, janitor)

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


# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# START RSELENIUM -------------------------------------------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


# Starten einer Remote-Sitzung mit Chrome auf Privat-PC
# Chrome options für headless Mode
# chrome_options <- list(
#   chromeOptions = list(
#     args = c("--headless", "--disable-gpu", "--window-size=1280,800")
#   )
# )

# RSelenium Driver starten
driver <- rsDriver(
  browser = "chrome",
  chromever = "125.0.6422.60",
  # extraCapabilities = chrome_options,
  port = 1234L
)

# Starten einer Remote-Sitzung mit Chrome auf Abeits-PC
#driver <- rsDriver(browser = "chrome", chromever = "131.0.6778.85", port = 1234L)

# # Starten einer Remote-Sitzung mit Chrome auf Arbeits-PC
# # Chrome options für headless Mode
# chrome_options <- list(
#   chromeOptions = list(
#     args = c("--headless", "--disable-gpu", "--window-size=1280,800")
#   )
# )

# # RSelenium Driver starten
# driver <- rsDriver(
#   browser = "chrome",
#   chromever = "131.0.6778.85",
#   extraCapabilities = chrome_options,
#   port = 1234L
# )



# Zugriff auf die gestartete Sitzung
rmdr <- driver[["client"]]
rmdr$maxWindowSize() # erzeuge maximale Fenstergröße

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# SCRAPE DATA ----------------------------------------------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Gehe zur Basisseite und wähle gewünschtes Semester
# aus.
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# jüngstens Semester == 0, ältestestes derzeit == 11
# Parameter muss genau über Chrome verifiziert werden
# Für Erlangen-Nuernberg: Scrape: 11 (SS22) - 7 (SS24)
choose_semester(rmdr, 11)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Die Vorlesungen werden angezeigt. Immer zehn Stück
# pro Seite bei insgesamt ca. 200-300 Seiten. Erzeuge
# Selektoren der Links zu den Kursen:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

css_selectors <- sprintf(
  "#genSearchRes\\:id3f3bd34c5d6b1c79\\:id3f3bd34c5d6b1c79Table\\:%d\\:tableRowAction",
  3300:8000
)


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Erzeuge Iteration-Zähler, für das Durchszählen der
# Kurse im Output (für Vereinfacherung möglichen Debuggings)
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Teile die css_selectors in Gruppen von 10 auf
chunks <- split(css_selectors, ceiling(seq_along(css_selectors) / 10))

# Initialisiere Variablen
iteration <- 0
final_results <- tibble()

# Scrape Base-Information
semester <- get_element("#genSearchRes\\:genericSearchResult > div.text_white_searchresult > span")
scraping_datum <- Sys.Date()

sem_scrape_date_df <- tibble(
  semester = semester,
  scraping_datum = scraping_datum
)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Betätigt Kurs-Selektor
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Haupt-Loop durch die Gruppen
for (i in seq_along(chunks)) {

  css_chunk <- chunks[[i]]

  for (i in css_chunk) {
    print(i)
    Sys.sleep(3)

  tryCatch({
    # Finden Sie das Element
    elem <- rmdr$findElement(using = "css selector", i)
    
    # Scrollen Sie das Element ins Zentrum des Browserfensters
    rmdr$executeScript(
      "arguments[0].scrollIntoView({ behavior: 'smooth', block: 'center' });",
      list(elem)
    )
  }, error = function(e) {
    # Nachricht ausgeben, wenn das Element nicht gefunden wurde
    message("Das Element mit dem Selektor '", i, "' wurde für das scrollen nicht gefunden.")
  })
  Sys.sleep(3)

    # Finde die Kurse auf der Überblicksseite
    kurs <- tryCatch({
      elem <- rmdr$findElement(using = "css selector", i)
      elem$clickElement()
      Sys.sleep(3)
    }, error = function(e) {
      message("\033[31m", "css zum Kurs nicht gefunden: ", i, "\033[0m")
      next
    })

    iteration <- iteration + 1 # Aktualisiere Iteration

    # Erzeuge Nachricht, welcher Kurs gescrapet wird:
    titel <- get_element('#\\31 8a8022569d6ced829f833aa855530ce')
    cat("\033[34m", paste0("Start das scraping von Kurs Nr.", iteration, ":"), "\033[0m",
        "\033[32m", titel, "\033[0m\n")

    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # TAB 1: Parallelgruppen/Termine -------------------------
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    cat("\033[31mstarte scraping von Variablen in Tab >>Parallelgruppen/Termine<<\033[0m\n")

    labels <- rmdr$findElements(using = "css selector", ".labelItemLine label")
    label_texts <- sapply(labels, function(el) el$getElementText())
    answers <- rmdr$findElements(using = "css selector", ".labelItemLine .answer")
    answer_texts <- sapply(answers, function(el) el$getElementText())

    base_info_df <- tibble(
      Label = label_texts,
      Answer = answer_texts) %>%
      group_by(Label) %>%
      summarise(Answer = list(unique(Answer)), .groups = "drop") %>%
      pivot_wider(names_from = Label, values_from = Answer)

    # Printe welche Variablen gescrapet wurden
    check_obj_exist(base_info_df)

    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # TAB 2: Inhalte ------------------------------------------
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    # Beginnt das Scraping der Variablen im Tab "Inhalte"
    cat("\033[31mstarte scraping von Variablen in Tab >>Inhalte<<\033[0m\n")

    # Versuch, den Inhalte-Tab zu finden und zu öffnen
    inhalte_tab <- wait_for_element_and_click(
      driver = rmdr,
      using = 'css selector',
      value = "#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:contentsTab",
      action = "click"
    )

    Sys.sleep(3)

    # Generiert XPaths für mögliche Container-IDs
    container_ids <- sprintf(
      '//*[@id="detailViewData:tabContainer:term-planning-container:j_id_6m_13_2_%d_1"]',
      0:20
    )

    # Initialisiert Zähler für gefundene Container
    found_containers <- 0

    # Schleife über die möglichen Container-XPaths
    for (xpath in container_ids) {
      # Sucht Elemente, die zum aktuellen XPath passen
      elements <- rmdr$findElements(using = "xpath", xpath)
      # Falls Elemente gefunden werden, erhöht den Zähler
      if (length(elements) > 0) {
        found_containers <- found_containers + 1
      }
    }

    # Gibt die Anzahl der gefundenen Container aus
    cat("Anzahl der gefundenen Container im Tab Inhalt:", "\033[32m", found_containers, "\033[0m", "\n")

    # Initialisiert Listen für Container-Titel und Inhalte
    container_titles <- list()
    container_contents <- list()

    # Falls Container gefunden wurden
    if (found_containers > 0) {
      cat("\033[32mEs werden folgende Variablen gescrapt:\033[0m\n")
      # Schleife über die gefundenen Container
      for (j in 1:found_containers) {
        # XPath für den Titel des aktuellen Containers
        title_xpath <- sprintf(
          '/html/body/div[1]/div[3]/div/div[1]/div/div/form[3]/div[2]/div/div/fieldset/div[4]/div/div[2]/fieldset/div[%d]/div/div/div/div[1]/div/div[2]/h2',
          j + 3
        )
        # XPath für den Inhalt des aktuellen Containers
        content_xpath <- sprintf(
          '/html/body/div[1]/div[3]/div/div[1]/div/div/form[3]/div[2]/div/div/fieldset/div[4]/div/div[2]/fieldset/div[%d]/div/div/div/div[2]',
          j + 3
        )

        # Extrahiert den Titel des Containers, falls vorhanden
        title_elements <- rmdr$findElements(using = "xpath", title_xpath)
        if (length(title_elements) > 0) {
          container_titles[[j]] <- title_elements[[1]]$getElementText()[[1]]
        } else {
          container_titles[[j]] <- NA
        }

        # Extrahiert den Inhalt des Containers, falls vorhanden
        content_elements <- rmdr$findElements(using = "xpath", content_xpath)
        if (length(content_elements) > 0) {
          container_contents[[j]] <- content_elements[[1]]$getElementText()[[1]]
        } else {
          container_contents[[j]] <- NA
        }
      }
    }

    # Prüft, ob Container gefunden wurden
    if (found_containers > 0) {
      # Falls Anzahl der Titel und Inhalte übereinstimmt
      if (length(container_titles) == length(container_contents)) {
        # Erstellt ein tibble mit den Titeln als Spaltennamen und den Inhalten als Werte
        inhalte_df <- tibble::tibble(
          !!!setNames(container_contents, container_titles)
        )
      } else {
        # Falls die Anzahl nicht übereinstimmt, wird ein Fehler ausgelöst
        stop("Die Anzahl der Titel und Inhalte stimmt nicht überein.")
      }
    } else {
      # Falls keine Container gefunden wurden, wird ein leeres tibble erstellt
      inhalte_df <- tibble()
    }

    # Prüft, ob die DataFrame-Variable existiert
    check_obj_exist(inhalte_df)

    # Bindet die neuen Inhalte an die Basis-Info-DataFrame, falls Inhalte vorhanden sind
    if (found_containers > 0) {
      base_info_inhalte_df <- bind_cols(base_info_df, inhalte_df)
    } else {
      # Falls keine Inhalte gefunden wurden, bleibt die Basis-Info unverändert
      base_info_inhalte_df <- base_info_df
    }


    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # TAB 3.1: Module -----------------------------------------
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    cat("\033[31mstarte scraping von Variablen in Tab >>Module/Studiengänge<<\033[0m\n")

    zugeordnete_module_tibble <- NULL

    module_tab <- wait_for_element_and_click(
      driver = rmdr,
      using = "css selector",
      value = "#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:modulesCourseOfStudiesTab",
      action = "click"
    )

    # Verwende get_element, um sicherzustellen, dass das Element existiert und Texte extrahiert werden können
    zugeordnete_module_text <- get_element("#detailViewData\\:tabContainer\\:term-planning-container\\:modules\\:moduleAssignments\\:moduleAssignmentsTable")

    if (is.na(zugeordnete_module_text)) {
      message("Das gewünschte Element konnte nicht gefunden werden oder hat keine Inhalte.")
    } else {
      # Hier kannst du mit den extrahierten Texten weiterarbeiten
      message("Element gefunden und verarbeitet.")
    }

    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # TAB 3.2: Studiengänge ------------------------------------
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    zugeord_studgaenge_tibble <- NULL

    # Tabelle mit `findElement` abrufen (inkl. Retry-Logik)
    tryCatch({
      zugeordnete_studiengaenge <- rmdr$findElement(using = "css selector",
                                                    "#detailViewData\\:tabContainer\\:term-planning-container\\:courseOfStudies\\:courseOfStudyAssignments\\:courseOfStudyAssignmentsTable")
    }, error = function(e) {
      Sys.sleep(3) # Warten und erneut versuchen
      zugeordnete_studiengaenge <- rmdr$findElement(using = "css selector",
                                                    "#detailViewData\\:tabContainer\\:term-planning-container\\:courseOfStudies\\:courseOfStudyAssignments\\:courseOfStudyAssignmentsTable")
    })

    # Falls die Tabelle gefunden wurde, HTML extrahieren und parsen
    if (!is.null(zugeordnete_studiengaenge)) {
      zugeordnete_studiengaenge_html <- zugeordnete_studiengaenge$getElementAttribute("outerHTML")[[1]]

      zugeord_studgaenge_tibble <- zugeordnete_studiengaenge_html %>%
        read_html() %>%
        html_table(fill = TRUE) %>%
        .[[1]] %>%
        as_tibble() %>%
        rename_with(~ gsub("Aufwärts sortieren", "", .x, fixed = TRUE)) %>%
        mutate(across(everything(), ~ str_remove_all(., paste0("^", cur_column(), "\\s*")))) %>%
        clean_names()

      check_obj_exist(zugeord_studgaenge_tibble)

    }


    module_studiengaeng_df <- tibble(
      zugeordnete_module_tibble = list(zugeordnete_module_tibble %||% NA),
      zugeord_studgaenge_tibble = list(zugeord_studgaenge_tibble %||% NA)
    )

    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # ZUSAMMENFÜGEN DER DATEN ----------------------------------
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    # |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

    interim_results <- bind_cols(base_info_inhalte_df, module_studiengaeng_df, sem_scrape_date_df)
    final_results <- bind_rows(final_results, interim_results)

    zurueck_button <- wait_for_element_and_click(
      driver = rmdr,
      using = "css selector",
      value = "#form\\:dialogHeader\\:backButtonTop",
      action = "click"
    )
  }

  #scroll nach ganz unten:
  rmdr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(4)
  weiter_button <- wait_for_element_and_click(
    driver = rmdr,
    using = "id",
    value = "genSearchRes:id3f3bd34c5d6b1c79:id3f3bd34c5d6b1c79Navi2next",
    action = "click"
  )
  rmdr$executeScript("window.scrollTo(0, 0);")


}


rmdr$close()
system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)
# Kill all java Mac:
# system("killall java", intern=FALSE, ignore.stdout=FALSE)


# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# DATENEXPORT ----------------------------------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

write_rds(final_results, "SS22_Erlangen_2999_2999.rds")
getwd()
