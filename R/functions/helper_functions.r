# Funktion zum sicheren Extrahieren von Text
get_element <- function(selector) {
tryCatch({
    # Prüfe, ob das Element existiert
    if (length(rmdr$findElements(using = "css selector", selector)) == 0) {
    return(NA) # NA, wenn kein Element gefunden wird
    }
    
    # Finde das Haupt-Element
    parent_element <- rmdr$findElement(using = "css selector", selector)
    
    # Suche nach <li>-Child-Elementsen
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


check_obj_exist <- function(scrape_obj) {
  if (!is.null(scrape_obj) && length(scrape_obj) > 0) {
    cat("\033[33m>>", deparse(substitute(scrape_obj)), "<<\033[0m", "erfolgreich erhoben\n")
  } else {
    cat("\033[31m", ">>", deparse(substitute(scrape_obj)), "<<", "nicht gefunden\n", "\033[0m")
  }
}

check_obj_exist_value <- function(scrape_obj) {
  if (!is.na(scrape_obj)) {
    if (!is.null(scrape_obj) && length(scrape_obj) > 0) {
      cat("\033[33m>>", scrape_obj, "<<\033[0m", "erfolgreich erhoben\n")
    } else {
      cat("\033[31m", ">>", scrape_obj, "<<", "nicht gefunden\n", "\033[0m")
    }
  }
}
