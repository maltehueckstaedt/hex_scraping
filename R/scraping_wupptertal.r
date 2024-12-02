#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# LOAD PACKAGES/FUNCTIONS -----------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, RSelenium,rlang,janitor)

source("R/functions/helper_functions.r")

#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# START RSELENIUM -------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////


# Starten einer Remote-Sitzung mit Chrome auf Privat-PC
driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.60", port = 1234L)

# Starten einer Remote-Sitzung mit Chrome auf Abeits-PC
#driver <- rsDriver(browser = "chrome", chromever = "131.0.6778.85", port = 1234L)

 
# Zugriff auf die gestartete Sitzung
rmdr <- driver[["client"]]
rmdr$maxWindowSize() # erzeuge maximale Fenstergröße

 
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# SCRAPE DATA ----------------------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Gehe zur Basisseite und wähle gewünschtes Semester
# aus.
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 
# jüngstens Semester == 0, ältestestes derzeit == 16
# Parameter muss genau über Chrome verifiziert werden
choose_semester(rmdr, 2)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Die Vorlesungen werden angezeigt. Immer zehn Stück 
# pro Seite bei insgesamt ca. 200-300 Seiten. Erzeuge 
# Selektoren der Links zu den Kursen:
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

css_selectors <- sprintf(
  "#genSearchRes\\:id3df798d58b4bacd9\\:id3df798d58b4bacd9Table\\:%d\\:tableRowAction",
  0:2
)
 
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Erzeuge Iteration-Zähler, für das Durchszählen der 
# Kurse im Output (für Vereinfacherung möglichen Debuggings)
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Teile die css_selectors in Gruppen von 10 auf
chunks <- split(css_selectors, ceiling(seq_along(css_selectors) / 10))

# Initialisiere Variablen
iteration <- 0
ergebnisse <- tibble()

# Scrape Base-Information

semester <- get_element("#genSearchRes\\:genericSearchResult > div.text_white_searchresult > span")
scraping_datum <- Sys.Date()

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Comment: Betätigt Kurs-Selektor
#.:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Haupt-Loop durch die Gruppen
for (i in seq_along(chunks)) {
  Sys.sleep(5)
  css_chunk <- chunks[[i]]
  
  for (i in css_chunk) { 
    print(i)
    # Finde die Kurse auf der Überblicksseite
    kurs <- tryCatch({
      elem <- rmdr$findElement(using = "css selector", i)
      elem$clickElement()
      Sys.sleep(5)
    }, error = function(e) {
      message("\033[31m", "css zum Kurs nicht gefunden: ", i, "\033[0m")
      next
    })
    
    iteration <- iteration + 1 # Aktualisiere Iteration
    
    # Erzeuge Nachricht, welcher Kurs gescrapet wird:
    titel <- get_element('#\\31 8a8022569d6ced829f833aa855530ce')
    cat("\033[34m", paste0("Start das scraping von Kurs Nr.", iteration, ":"), "\033[0m",
        "\033[32m", titel, "\033[0m\n")
    
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    # TAB 1: Parallelgruppen/Termine -------------------------
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    
    cat("\033[31mstarte scraping von Variablen in Tab >>Parallelgruppen/Termine<<\033[0m\n")
    
    labels <- rmdr$findElements(using = "css selector", ".labelItemLine label")
    label_texts <- sapply(labels, function(el) el$getElementText())
    answers <- rmdr$findElements(using = "css selector", ".labelItemLine .answer")
    answer_texts <- sapply(answers, function(el) el$getElementText())
    
    base_info_df <- tibble(
      Label = label_texts,
      Answer = answer_texts
    ) %>%
      group_by(Label) %>%
      summarise(Answer = list(unique(Answer)), .groups = "drop") %>%
      pivot_wider(names_from = Label, values_from = Answer)
    
    
    # Printe welche Variablen gescrapet wurden
    check_obj_exist(base_info_df)
    
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    # TAB 2: Inhalte ------------------------------------------
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    
    cat("\033[31mstarte scraping von Variablen in Tab >>Inhalte<<\033[0m\n")
    
    tryCatch({
      inhalte_tab <- rmdr$findElement(using = "css selector", '#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:contentsTab')
      inhalte_tab$clickElement()
      Sys.sleep(5)
    }, error = function(e) {
      message("Inhalte-Tab nicht gefunden.")
    })
    
    container_ids <- sprintf(
      '//*[@id="detailViewData:tabContainer:term-planning-container:j_id_6m_13_2_%d_1"]',
      0:20
    )
    
    found_containers <- 0
    
    for (xpath in container_ids) {
      elements <- rmdr$findElements(using = "xpath", xpath)
      if (length(elements) > 0) {
        found_containers <- found_containers + 1
      }
    }
    
    cat("Anzahl der gefundenen Container im Tab Inhalt:", "\033[32m", found_containers, "\033[0m", "\n")
    
    container_titles <- list()
    container_contents <- list()
    
    if (found_containers > 0) {
      cat("\033[32mEs werden folgende Variablen gescrapt:\033[0m\n")
      for (j in 1:found_containers) {
        title_xpath <- sprintf(
          '/html/body/div[1]/div[3]/div/div[1]/div/div/form[3]/div[2]/div/div/fieldset/div[4]/div/div[2]/fieldset/div[%d]/div/div/div/div[1]/div/div[2]/h2',
          j + 3
        )
        content_xpath <- sprintf(
          '/html/body/div[1]/div[3]/div/div[1]/div/div/form[3]/div[2]/div/div/fieldset/div[4]/div/div[2]/fieldset/div[%d]/div/div/div/div[2]',
          j + 3
        )
        
        title_elements <- rmdr$findElements(using = "xpath", title_xpath)
        if (length(title_elements) > 0) {
          container_titles[[j]] <- title_elements[[1]]$getElementText()[[1]]
        } else {
          container_titles[[j]] <- NA
        }
        
        content_elements <- rmdr$findElements(using = "xpath", content_xpath)
        if (length(content_elements) > 0) {
          container_contents[[j]] <- content_elements[[1]]$getElementText()[[1]]
        } else {
          container_contents[[j]] <- NA
        }
      }
    }
    
    if (found_containers > 0) {
      if (length(container_titles) == length(container_contents)) {
        inhalte_df <- tibble::tibble(
          !!!setNames(container_contents, container_titles)
        )
      } else {
        stop("Die Anzahl der Titel und Inhalte stimmt nicht überein.")
      }
    } else {
      inhalte_df <- tibble()
    }
    
    check_obj_exist(inhalte_df)
    
    if (found_containers > 0) {
      base_info_inhalte_df <- bind_cols(base_info_df, inhalte_df)
    }
    
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    # TAB 3.1: Module -----------------------------------------
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    
    cat("\033[31mstarte scraping von Variablen in Tab >>Module/Studiengänge<<\033[0m\n")
    
    zugeordnete_module_tibble <- NULL
    
    module_tab <- rmdr$findElement(using = "css selector", '#detailViewData\\:tabContainer\\:term-planning-container\\:tabs\\:modulesCourseOfStudiesTab')
    module_tab$clickElement()
    
    tryCatch({
      # Warten, bis die Seite geladen ist (explizites Warten)
      Sys.sleep(5)
      
      # Tabelle abrufen
      zugeordnete_module <- rmdr$findElement(
        using = "css selector", 
        "#detailViewData\\:tabContainer\\:term-planning-container\\:modules\\:moduleAssignments\\:moduleAssignmentsTable"
      )
      
      # Tabelle extrahieren und parsen
      zugeordnete_module_tibble <- zugeordnete_module$getElementAttribute("outerHTML")[[1]] %>%
        read_html() %>%
        html_table(fill = TRUE) %>%
        .[[1]] %>%
        mutate(across(everything(), ~ str_remove_all(., paste0("^", cur_column(), "\\s*")))) %>% 
        rename_with(~ gsub("Aufwärts sortieren", "", .x, fixed = TRUE)) %>% 
        clean_names %>% 
        as_tibble()
      
      # Tabelle anzeigen
      check_obj_exist(zugeordnete_module_tibble)
      
    }, error = function(e) {
      message("Fehler beim Abrufen der Tabelle: ", e$message)
    })
    
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    # TAB 3.2: Studiengänge ------------------------------------
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    
    zugeordnete_studiengaenge_tibble <- NULL
    
    # Tabelle mit `findElement` abrufen (inkl. Retry-Logik)
    tryCatch({
      zugeordnete_studiengaenge <- rmdr$findElement(using = "css selector", 
                                                    "#detailViewData\\:tabContainer\\:term-planning-container\\:courseOfStudies\\:courseOfStudyAssignments\\:courseOfStudyAssignmentsTable")
    }, error = function(e) {
      Sys.sleep(2) # Warten und erneut versuchen
      zugeordnete_studiengaenge <- rmdr$findElement(using = "css selector", 
                                                    "#detailViewData\\:tabContainer\\:term-planning-container\\:courseOfStudies\\:courseOfStudyAssignments\\:courseOfStudyAssignmentsTable")
    })
    
    # Falls die Tabelle gefunden wurde, HTML extrahieren und parsen
    if (!is.null(zugeordnete_studiengaenge)) {
      zugeordnete_studiengaenge_html <- zugeordnete_studiengaenge$getElementAttribute("outerHTML")[[1]]
      
      zugeordnete_studiengaenge_tibble <- zugeordnete_studiengaenge_html %>%
        read_html() %>%
        html_table(fill = TRUE) %>%
        .[[1]] %>%
        as_tibble() %>%
        rename_with(~ gsub("Aufwärts sortieren", "", .x, fixed = TRUE)) %>%
        mutate(across(everything(), ~ str_remove_all(., paste0("^", cur_column(), "\\s*")))) %>% 
        clean_names()
      
      check_obj_exist(zugeordnete_studiengaenge_tibble)
    }

    module_studiengaeng_df <- tibble(
      zugeordnete_module_tibble = list(zugeordnete_module_tibble %||% NA),
      zugeordnete_studiengaenge_tibble = list(zugeordnete_studiengaenge_tibble %||% NA)
    )
    
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    # ZUSAMMENFÜGEN DER DATEN ----------------------------------
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    #////////////////////////////////////////////////////////////
    
    aktuelle_ergebnisse <- cbind(base_info_inhalte_df, module_studiengaeng_df)
    ergebnisse <- bind_rows(ergebnisse, aktuelle_ergebnisse)
    
    zurück_button <- rmdr$findElement(using = "css selector", "#form\\:dialogHeader\\:backButtonTop")
    zurück_button$clickElement()
    Sys.sleep(5)
  }
  
    tryCatch({
      # Finde und klicke den Weiter-Button
      Sys.sleep(5)
      weiter_button <- rmdr$findElement(using = "id", value = "genSearchRes:id3df798d58b4bacd9:id3df798d58b4bacd9Navi2next")
      #weiter_button <- rmdr$findElement(using = "css selector", "#genSearchRes\\:id3df798d58b4bacd9\\:id3df798d58b4bacd9Navi2next")
      weiter_button$clickElement()
      
      # Prüfen, ob das gewünschte Element vorhanden ist
      Sys.sleep(2)
 
    })

  }


rmdr$close() 
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# Kill all java Mac:
# system("killall java", intern=FALSE, ignore.stdout=FALSE)

#genSearchRes\:id3df798d58b4bacd9\:id3df798d58b4bacd9Navi2next

#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
# DATENEXPORT ----------------------------------
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////
