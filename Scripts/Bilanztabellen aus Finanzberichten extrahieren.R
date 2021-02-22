#############################################################################################
# HSG BA: "Benford-Analyse anhand des Fallbeispiels der Wirecard AG"
# Bilanztabellen aus den Finanzberichten (PDF) extrahieren
# 
# S. Tragust, Partschins
# Erste Version: Dezember 10, 2020
#############################################################################################




# VORBEREITUNG -------------------------------------

# installiere packages
install.packages("rJava") # wird für 'tabulizer' package benötigt
install.packages("tabulizer") # PDF scraping / scraping table data
install.packages("tidyverse") # datenmanipulation, -exploration und -visualisierung
install.packages("readr") # lesen von rectangular data (wie 'csv', 'tsv', 'fwf')
install.packages("shiny") # erstellen von interaktiven webanwendungen (wird für 'tabulizer'-package benötigt) 
install.packages("miniUI") # UI-widget zum schreiben von shiny-apps (wird für 'tabulizer'-package benötigt)

# lade packages
library(rJava)
library(tabulizer)
library(tidyverse)
library(readr)
library(shiny)
library(miniUI)

# wissenschaftliche notation/exponentialdarstellung deaktivieren
options(scipen = 999) 






# PROGRAMM -----------------------------------------

# werte definieren
d <- "DE0007472060-JA-2018-EQ-D-02.pdf" # dateiname
v <- c(12,13,15,18,20,56,57,63) # seitenzahlen im PDF, wo sich die tabellen befinden
n <- length(v) # länge von 'v'


# for-loop definieren
for (i in 1:n) {
  s <- v[i]
  print(s)
 
  # extrahiere tabellen mit 'extract_areas' funktion
  tab_raw <- extract_areas(d, pages = s)
  
  # wähle die erste tabelle aus und wandle sie in ein 'tibble' um
  tab_raw <- tab_raw %>%
    pluck(1) %>%
    as_tibble()
  
  # spalte 'V4' entfernen
  tab_raw$V4 <- NULL
  
  # hole spaltennamen aus zeile 1 und benenne sie um
  col_names <- tab_raw %>%
    slice(1) %>%
    pivot_longer(cols = everything()) %>%
    mutate(value = ifelse(is.na(value), "Missing", value)) %>%
    pull(value)
  col_names[1] <- "BEZEICHNUNG"
  col_names[2] <- "THIS_YEAR"
  col_names[3] <- "LAST_YEAR"
  
  # namen überschreiben und zeile 1 entfernen
  tab_cleaned <- tab_raw %>%
    set_names(col_names) %>%
    slice(-1)
  
  # zahlen formatieren
  tab_cleaned$THIS_YEAR <- str_remove_all(tab_cleaned$THIS_YEAR, "[.]") # tausenderpunkt entfernen
  tab_cleaned$THIS_YEAR <- str_remove_all(tab_cleaned$THIS_YEAR, "[â€“]") # minus entfernen (für die benford-analyse spielen negative bzw. positive zahlen keine rolle)
  tab_cleaned$THIS_YEAR <- str_remove_all(tab_cleaned$THIS_YEAR, "[-]") # minus entfernen
  tab_cleaned$THIS_YEAR <- str_replace_all(tab_cleaned$THIS_YEAR, ",", ".") # dezimalkomma mit punkt ersetzen
  tab_cleaned$THIS_YEAR <- as.numeric(tab_cleaned$THIS_YEAR) # character-werte in numeric-werte umwandeln
  
  tab_cleaned$LAST_YEAR <- str_remove_all(tab_cleaned$LAST_YEAR, "[.]")
  tab_cleaned$LAST_YEAR <- str_remove_all(tab_cleaned$LAST_YEAR, "[â€“]")
  tab_cleaned$LAST_YEAR <- str_remove_all(tab_cleaned$LAST_YEAR, "[-]")
  tab_cleaned$LAST_YEAR <- str_replace_all(tab_cleaned$LAST_YEAR, ",", ".")
  tab_cleaned$LAST_YEAR <- as.numeric(tab_cleaned$LAST_YEAR)
  
  str(tab_cleaned)
  View(tab_cleaned)
  
  # tabelle als .csv speichern
  name <- sprintf("%s. Seite Tabelle.csv", s)
  write.csv(tab_cleaned, name, row.names = FALSE)
  
}


