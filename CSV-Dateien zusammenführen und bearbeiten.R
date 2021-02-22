#############################################################################################
# HSG BA: "Benford-Analyse anhand des Fallbeispiels der Wirecard AG"
# CSV-Dateien zusammenführen und bearbeiten
# 
# S. Tragust, Partschins
# Erste Version: Dezember 10, 2020
#############################################################################################




# VORBEREITUNG -------------------------------------

# installiere packages
install.packages("plyr") # daten zerlegen, bearbeiten, zusammensetzen
install.packages("tidyverse") # datenmanipulation, -exploration und -visualisierung
install.packages("gtools") # verschiedene R-tools (strings mit eingebetteten zahlen ordnen/sortieren)

# lade packages
library(plyr)
library(tidyverse)
library(gtools)

# wissenschaftliche notation/exponentialdarstellung deaktivieren
options(scipen = 999) 












# PROGRAMM -----------------------------------------
# (Hinweis: folgende code-zeilen wurden je nach bedarf selektiv verwendet!)

# importiere alle CSV's im working directory -----------------
d <- ldply(mixedsort(list.files()), read_csv)
View(d)

# spalten löschen und umbenennen-----------------
d$LAST_YEAR <- NULL
d$THIS_YEAR <- NULL
names(d)[1] <- "BEZEICHNUNG"
names(d)[2] <- "THIS_YEAR"
names(d)[3] <- "LAST_YEAR"

# zeilen mit NA's löschen -----------------
d <- na.omit(d)
# index-nummern neu ordnen
rownames(d) <- 1:nrow(d)

# zeilen mit spezifischen wörtern/strings löschen -----------------
    #### AKTIVA ####
    d <- d[which( ! d$BEZEICHNUNG %in% c(
                    "in Mio. EUR",
                    "in TEUR",
                    "TEUR",
                    "in EUR",
                    "EUR",
                    "in EUR Erläuterung",
                     "a) Geschäftswerte",
                    "Geschäftswerte",
                    "4. Finanzielle und andere Vermögenswerte / verzinsliche Wertpapiere",
                    "3. Finanzielle und andere Vermögenswerte / verzinsliche Wertpapiere",
                    "verzinsliche Wertpapiere",
                    "Langfristiges Vermögen gesamt",
                    "LANGFRISTIGES VERMÖGEN GESAMT",
                    "Langfristige Vermögensgegenstände, gesamt",
                    "3. Forderungen aus Lieferungen und Leistungen und sonstige Forderungen",
                    "sonstige Forderungen",
                    "Kurzfristiges Vermögen gesamt",
                    "KURZFRISTIGES VERMÖGEN, GESAMT",
                    "Kurzfristige Vermögensgegenstände, gesamt",
                    "Summe Vermögen",
                    "SUMME AKTIVA",
                    "Total",
                    "Gesamt",
                    "GESAMT")),]
        rownames(d) <- 1:nrow(d)
    #### PASSIVA ####
    d <- d[which( ! d$BEZEICHNUNG %in% c(
                    "in Mio. EUR",
                    "in TEUR",
                    "TEUR",
                    "in EUR",
                    "EUR",
                    "Eigenkapital gesamt",
                    "Eigenkapital, gesamt",
                    "EIGENKAPITAL GESAMT",
                    "EIGENKAPITAL, GESAMT",
                    "Sonstige langfristige Verbindlichkeiten",
                    "Langfristige Schulden gesamt",
                    "Langfristige Verbindlichkeiten, gesamt",
                    "Sonstige Verbindlichkeiten",
                    "Kurzfristige Schulden gesamt",
                    "Kurzfristige Verbindlichkeiten, gesamt",
                    "Schulden gesamt",
                    "SCHULDEN, GESAMT",
                    "SCHULDEN GESAMT",
                    "Summe Eigenkapital und Schulden",
                    "SUMME PASSIVA")),]
    rownames(d) <- 1:nrow(d)
    #### CASHFLOW ####
    d <- d[which( ! d$BEZEICHNUNG %in% c(
                    "in Mio. EUR",
                    "in TEUR",
                    "TEUR",
                    "in EUR",
                    "EUR",
                    "Finanzergebnis",
                    "Ertragssteueraufwendungen",
                    "Ertragsteueraufwendungen",
                    "Abschreibungen",
                    "Abschreibungen/Zuschreibungen auf langfristige Vermögenswerte",
                    "Cashflow aus betrieblicher Tätigkeit vor Veränderungen aus dem Bankenbereich",
                    "Cashflow aus betrieblicher Tätigkeit aus dem Bankenbereich",
                    "Cashflow aus betrieblicher Tätigkeit",
                    "Cashflow aus laufender Geschäftstätigkeit",
                    "= Cashflow aus laufender Geschäftstätigkeit",
                    "= Cash Flow aus laufender Geschäftstätigkeit",
                    "Cashflow aus Investitionstätigkeit",
                    "= Cashflow aus Investitionstätigkeit",
                    "= Cash Flow aus Investitionstätigkeit",
                    "Cashflow aus Finanzierungstätigkeit",
                    "= Cashflow aus Finanzierungstätigkeit",
                    "= Cash Flow aus Finanzierungstätigkeit",
                    "Finanzmittelbestand am Anfang der Periode",
                    "+ Finanzmittelfonds am Anfang der Periode",
                    "Finanzmittelfonds am Anfang der Periode",
                    "Finanzmittelbestand am Ende der Periode",
                    "= Finanzmittelbestand am Ende der Periode",
                    "Finanzmittelfonds am Ende der Periode")),]
    rownames(d) <- 1:nrow(d)
    #### GuV ####
    d <- d[which( ! d$BEZEICHNUNG %in% c(
                    "Rohertrag",
                    "II. Bestandsveränderung und andere aktivierte Eigenleistungen",
                    "II. Andere aktivierte Eigenleistungen",
                    "Eigenleistungen",
                    "III. Spezielle betriebliche Aufwendungen",
                    "IV. Sonstige betriebliche Erträge und Aufwendungen",
                    "Aufwendungen",
                    "Sonstige betriebliche Aufwendungen",
                    "2. Sonstige betriebliche Aufwendungen",
                    "Betriebsergebnis",
                    "EBITDA",
                    "Abschreibungen",
                    "EBIT",
                    "EBIT **",
                    "Finanzergebnis",
                    "V. Finanzergebnis",
                    "Ergebnis vor Steuern",
                    "Ergebnis vor Steuern *",
                    "Ergebnis vor Steuern **",
                    "Ergebnis vor Steuern ***",
                    "Ergebnis vor Steuern (und Minderheitanteilen)",
                    "Ergebnis vor Steuern (und Minderheitenanteilen)",
                    "Ergebnis vor Minderheitanteilen",
                    "VI. Ergebnis vor Steuern",
                    "VI. Ergebnis vor Steuern *",
                    "Ergebnis nach Steuern",
                    "Ergebnis nach Steuern *",
                    "Ergebnis nach Steuern **",
                    "Ergebnis nach Steuern ***",
                    "VIII. Ergebnis nach Steuern",
                    "VIII. Ergebnis nach Steuern *",
                    "IX. Gewinnvortrag",
                    "IX. Gewinnvortrag aus dem Vorjahr",
                    "Gewinnvortrag aus dem Vorjahr (Vorquartalen)",
                    "Bilanzgewinn",
                    "X. Bilanzgewinn",
                    "XI. Bilanzgewinn",
                    "verwässert)",
                    "Ergebnis je Aktie (verwässert) in EUR",
                    "Ergebnis je Aktie (verwässert)",
                    "ø im Umlauf befindliche Aktien (unverwässert)",
                    "Durchschnittlich im Umlauf befindliche Aktien (unverwässert)",
                    "ø im Umlauf befindliche Aktien (verwässert)",
                    "Durchschnittlich im Umlauf befindliche Aktien (verwässert)",
                    "EBIT bereinigt *",
                    "EBIT bereinigt **",
                    "EBIT bereinigt ***",
                    "EBIT bereinigt*",
                    "EBIT bereinigt**",
                    "EBIT bereinigt***",
                    "Total",
                    "Gesamt",
                    "in Mio. EUR",
                    "in TEUR",
                    "TEUR",
                    "in EUR",
                    "EUR")),]
    rownames(d) <- 1:nrow(d)
    # zeilen 'Sonstige betriebliche Erträge', 'Sonstige Finanzerträge', 'Finanzaufwand', 'Finanzerträge' löschen
    d <- d[which( ! d$BEZEICHNUNG %in% c(
                    "Sonstige betriebliche Erträge",
                    "1. Sonstige betriebliche Erträge",
                    "Sonstige Finanzerträge",
                    "Finanzaufwand",
                    "Finanzerträge")),]
    rownames(d) <- 1:nrow(d)

# zeile nach spezifischer index-nummer hinzufügen -----------------
z <- data.frame("Ergebnis je Aktive (unverwässert)", 0.11, "NA")      
names(z) <- c("BEZEICHNUNG", "THIS_YEAR", "LAST_YEAR") 
r <- 12 # die oben definierte zeile wird nach der index-nummer '12' hinzugefügt; hat also die index-nummer '13'
d <- rbind(d[1:r,],z,d[-(1:r),])

# zeilen löschen -----------------
d <- d[-c(7,14,17,25), ] # die zeilen mit index-nummern '7', '14', '17', und '25' werden gelöscht
rownames(d) <- 1:nrow(d)

# multipliziere mit million/tausend (je nach angabe im finanzbericht) -----------------
d$THIS_YEAR <- d$THIS_YEAR * 1000000
d$THIS_YEAR <- d$THIS_YEAR * 1000

# werte der spalte 'THIS_YEAR' addieren -----------------
sum(d$THIS_YEAR)

# data.frame untersuchen -----------------
str(d)
summary(d)

# tabelle als .csv speichern -----------------
write.csv(d, "JA_2018.csv", row.names = FALSE, fileEncoding = "UTF-8")

