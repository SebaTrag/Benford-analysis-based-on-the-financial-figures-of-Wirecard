#############################################################################################
# HSG BA: "Benford-Analyse anhand des Fallbeispiels der Wirecard AG"
# Benford - Analyse durchführen
# 
# S. Tragust, Partschins
# Erste Version: Januar 01, 2021
#############################################################################################




# VORBEREITUNG -------------------------------------

# installiere packages
install.packages("ggplot2") # erstellen von grafiken/plots (wird für 'plotly'-package benötigt)
install.packages("plotly") # erstellen von interaktiven grafiken/plots/tabellen aus 'ggplot2'-graphen 
install.packages("tidyverse") # datenmanipulation, -exploration und -visualisierung
install.packages("formattable") # formatieren von vektoren und dataframes
install.packages("benford.analysis") # durchführung von benford-analysen
install.packages("BenfordTests") # statistische tests zur bewertung der konformität mit dem benfordschen gesetz
install.packages("moments") # berechnen der schiefe (skewness)

# lade packages
library(ggplot2)
library(plotly)
library(tidyverse)
library(formattable)
library(benford.analysis)
library(BenfordTests)
library(moments)

# wissenschaftliche notation/exponentialdarstellung deaktivieren
options(scipen = 999)




# PROGRAMM -----------------------------------------

# 2005 - 2012 ###########
# importiere CSV's
JA_2005_2012 <- read_csv("JA_2005-2012.csv")
Q1_2005_2012 <- read_csv("Q1_2005-2012.csv")
Q2_2005_2012 <- read_csv("Q2_2005-2012.csv")
Q3_2005_2012 <- read_csv("Q3_2005-2012.csv")





# 1. ZIFFER - ANALYSE
# benford analyse performen (mithilfe 'benford.analysis' package)
JA = benford(JA_2005_2012$THIS_YEAR, number.of.digits = 1)
Q1 = benford(Q1_2005_2012$THIS_YEAR, number.of.digits = 1)
Q2 = benford(Q2_2005_2012$THIS_YEAR, number.of.digits = 1)
Q3 = benford(Q3_2005_2012$THIS_YEAR, number.of.digits = 1)

# daten in einem data.table speichern
JA_TABLE <- getBfd(JA)
Q1_TABLE <- getBfd(Q1)
Q2_TABLE <- getBfd(Q2)
Q3_TABLE <- getBfd(Q3)

# formatiere die wahrscheinlichkeiten in prozent
JA_WK <- percent(JA_TABLE$data.dist, 1)
Q1_WK <- percent(Q1_TABLE$data.dist, 1)
Q2_WK <- percent(Q2_TABLE$data.dist, 1)
Q3_WK <- percent(Q3_TABLE$data.dist, 1)
BENFORD_WK <- percent(JA_TABLE$benford.dist, 1)
JA_WK <- as.character(JA_WK)
Q1_WK <- as.character(Q1_WK)
Q2_WK <- as.character(Q2_WK)
Q3_WK <- as.character(Q3_WK)
BENFORD_WK <- as.character(BENFORD_WK)

# data.frame mit verwendeten daten erstellen
data_1 <- data.frame(JA_TABLE$digits,
                     JA_TABLE$data.dist,
                     Q1_TABLE$data.dist,
                     Q2_TABLE$data.dist,
                     Q3_TABLE$data.dist,
                     JA_WK,
                     Q1_WK,
                     Q2_WK,
                     Q3_WK,
                     JA_TABLE$benford.dist,
                     BENFORD_WK)


# PLOT mit SUBPLOTS
# JA - subplot
fig_JA <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$JA_TABLE.data.dist,
                  text = data_1$JA_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(11, 80, 184)"),
                  name = "Jahresfinanzbericht",
                  type = "bar")
fig_JA <- fig_JA %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_JA <- fig_JA %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_JA <- fig_JA %>% layout(title = list(text = "Jahresfinanzbericht: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q1 - subplot
fig_Q1 <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$Q1_TABLE.data.dist,
                  text = data_1$Q1_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(219, 20, 20)"),
                  name = "Quartal 1",
                  type = "bar")
fig_Q1 <- fig_Q1 %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q1 <- fig_Q1 %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_Q1 <- fig_Q1 %>% layout(title = list(text = "Quartal 1: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q2 - subplot
fig_Q2 <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$Q2_TABLE.data.dist,
                  text = data_1$Q2_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(71, 128, 3)"),
                  name = "Quartal 2",
                  type = "bar")
fig_Q2 <- fig_Q2 %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q2 <- fig_Q2 %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_Q2 <- fig_Q2 %>% layout(title = list(text = "Quartal 2: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q3 - subplot
fig_Q3 <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$Q3_TABLE.data.dist,
                  text = data_1$Q3_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(61, 61, 61)"),
                  name = "Quartal 3",
                  type = "bar")
fig_Q3 <- fig_Q3 %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = TRUE)
fig_Q3 <- fig_Q3 %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_Q3 <- fig_Q3 %>% layout(title = list(text = "Quartal 3: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# plot definieren
fig <- subplot(fig_JA,
               fig_Q1,
               fig_Q2,
               fig_Q3,
               nrows = 2,
               margin = 0.04,
               shareX = TRUE,
               shareY = TRUE,
               titleX = TRUE,
               titleY = TRUE)
fig <- fig %>% layout(title = list(text = "Benford-Analyse: Wirecard AG 2005 - 2012",
                                   font = list(family = "Garamond", size = 35),
                                   y = 1),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      showlegend = TRUE,
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x= 1.06,
                                    y= 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig



# chi-quadrat-test und MAD durchführen
# allgemeine auswertungen/ergebnisse der analyse anzeigen
print(Q1)
print(Q2)
print(Q3)
print(JA)

# chi-quadrat  
chisq(Q1)
chisq(Q2)
chisq(Q3)
chisq(JA)
# vergelich mit chi-quadrat-test aus "Benford.Tests"  
Q1_data.used <- Q1[["data"]][["data.used"]]
Q2_data.used <- Q2[["data"]][["data.used"]]
Q3_data.used <- Q3[["data"]][["data.used"]]
JA_data.used <- JA[["data"]][["data.used"]]
chisq.benftest(x = Q1_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q2_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q3_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = JA_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)

# MAD
MAD(Q1)
MAD(Q2)
MAD(Q3)
MAD(JA)

# statistiken anzeigen (mean & median)
summary(Q1_2005_2012)
summary(Q2_2005_2012)
summary(Q3_2005_2012)
summary(JA_2005_2012)

# schiefe berechnen
skewness(Q1_2005_2012$THIS_YEAR)
skewness(Q2_2005_2012$THIS_YEAR)
skewness(Q3_2005_2012$THIS_YEAR)
skewness(JA_2005_2012$THIS_YEAR)




















# 2. ZIFFER - ANALYSE
# erstelle ein data.frame mit den ersten zwei ziffern der jeweiligen zaheln
JA <- extract.digits(JA_2005_2012$THIS_YEAR, number.of.digits = 2)
Q1 <- extract.digits(Q1_2005_2012$THIS_YEAR, number.of.digits = 2)
Q2 <- extract.digits(Q2_2005_2012$THIS_YEAR, number.of.digits = 2)
Q3 <- extract.digits(Q3_2005_2012$THIS_YEAR, number.of.digits = 2)

# extrahiere die 2. ziffer und erstelle eine neue spalte 'second_digit'
JA$second_digit <- as.integer(substr(JA$data.digits, 2, 2))
Q1$second_digit <- as.integer(substr(Q1$data.digits, 2, 2))
Q2$second_digit <- as.integer(substr(Q2$data.digits, 2, 2))
Q3$second_digit <- as.integer(substr(Q3$data.digits, 2, 2))

# mit package "benford.analysis" lassen sich nur die relativen häufigkeiten der 1. und 1.&2. ziffer berechnen
# folgend werden die relativen häufigkeiten der 2. ziffer manuell ausgerechnet 

# JA: absolute häufigkeit der ziffern berrechnen
s0 <- sum(JA$second_digit == 0)
s1 <- sum(JA$second_digit == 1)
s2 <- sum(JA$second_digit == 2)
s3 <- sum(JA$second_digit == 3)
s4 <- sum(JA$second_digit == 4)
s5 <- sum(JA$second_digit == 5)
s6 <- sum(JA$second_digit == 6)
s7 <- sum(JA$second_digit == 7)
s8 <- sum(JA$second_digit == 8)
s9 <- sum(JA$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# JA: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
JA_data.dist <- p

# Q1: absolute häufigkeit der ziffern berechenen
s0 <- sum(Q1$second_digit == 0)
s1 <- sum(Q1$second_digit == 1)
s2 <- sum(Q1$second_digit == 2)
s3 <- sum(Q1$second_digit == 3)
s4 <- sum(Q1$second_digit == 4)
s5 <- sum(Q1$second_digit == 5)
s6 <- sum(Q1$second_digit == 6)
s7 <- sum(Q1$second_digit == 7)
s8 <- sum(Q1$second_digit == 8)
s9 <- sum(Q1$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# Q1: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
Q1_data.dist <- p

# Q2: absolute häufigkeit der ziffern berrechnen
s0 <- sum(Q2$second_digit == 0)
s1 <- sum(Q2$second_digit == 1)
s2 <- sum(Q2$second_digit == 2)
s3 <- sum(Q2$second_digit == 3)
s4 <- sum(Q2$second_digit == 4)
s5 <- sum(Q2$second_digit == 5)
s6 <- sum(Q2$second_digit == 6)
s7 <- sum(Q2$second_digit == 7)
s8 <- sum(Q2$second_digit == 8)
s9 <- sum(Q2$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# Q2: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
Q2_data.dist <- p

# Q3: absolute häufigkeit der ziffern berrechnen
s0 <- sum(Q3$second_digit == 0)
s1 <- sum(Q3$second_digit == 1)
s2 <- sum(Q3$second_digit == 2)
s3 <- sum(Q3$second_digit == 3)
s4 <- sum(Q3$second_digit == 4)
s5 <- sum(Q3$second_digit == 5)
s6 <- sum(Q3$second_digit == 6)
s7 <- sum(Q3$second_digit == 7)
s8 <- sum(Q3$second_digit == 8)
s9 <- sum(Q3$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# Q3: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
Q3_data.dist <- p

# ziffern speichern
digits <- c(0:9)

# benford-wahrscheinlichkeiten für die 2. ziffer speichern
benford.dist <- c(0.11967927, 
                  0.11389010, 
                  0.10882150, 
                  0.10432956, 
                  0.10030820, 
                  0.09667724, 
                  0.09337474, 
                  0.09035199, 
                  0.08757005, 
                  0.08499735) 

# formatiere die wahrscheinlichkeiten in prozent
JA_WK <- percent(JA_data.dist, 1)
Q1_WK <- percent(Q1_data.dist, 1)
Q2_WK <- percent(Q2_data.dist, 1)
Q3_WK <- percent(Q3_data.dist, 1)
BENFORD_WK <- percent(benford.dist, 1)
JA_WK <- as.character(JA_WK)
Q1_WK <- as.character(Q1_WK)
Q2_WK <- as.character(Q2_WK)
Q3_WK <- as.character(Q3_WK)
BENFORD_WK <- as.character(BENFORD_WK)

# data.frame mit verwendeten daten erstellen
data_2 <- data.frame(digits,
                     JA_data.dist,
                     Q1_data.dist,
                     Q2_data.dist,
                     Q3_data.dist,
                     JA_WK,
                     Q1_WK,
                     Q2_WK,
                     Q3_WK,
                     benford.dist,
                     BENFORD_WK)




# PLOT mit SUBPLOTS
# JA - subplot
fig_JA <- plot_ly(x = data_2$digits,
                  y = data_2$JA_data.dist,
                  text = data_2$JA_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(11, 80, 184)"),
                  name = "Jahresfinanzbericht",
                  type = "bar")
fig_JA <- fig_JA %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_JA <- fig_JA %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_JA <- fig_JA %>% layout(title = list(text = "Jahresfinanzbericht: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q1 - subplot
fig_Q1 <- plot_ly(x = data_2$digits,
                  y = data_2$Q1_data.dist,
                  text = data_2$Q1_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(219, 20, 20)"),
                  name = "Quartal 1",
                  type = "bar")
fig_Q1 <- fig_Q1 %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q1 <- fig_Q1 %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_Q1 <- fig_Q1 %>% layout(title = list(text = "Quartal 1: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q2 - subplot 
fig_Q2 <- plot_ly(x = data_2$digits,
                  y = data_2$Q2_data.dist,
                  text = data_2$Q2_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(71, 128, 3)"),
                  name = "Quartal 2",
                  type = "bar")
fig_Q2 <- fig_Q2 %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q2 <- fig_Q2 %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_Q2 <- fig_Q2 %>% layout(title = list(text = "Quartal 2: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q3 - subplot 
fig_Q3 <- plot_ly(x = data_2$digits,
                  y = data_2$Q3_data.dist,
                  text = data_2$Q3_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(61, 61, 61)"),
                  name = "Quartal 3",
                  type = "bar")
fig_Q3 <- fig_Q3 %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = TRUE)
fig_Q3 <- fig_Q3 %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_Q3 <- fig_Q3 %>% layout(title = list(text = "Quartal 3: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# plot definieren
fig <- subplot(fig_JA,
               fig_Q1,
               fig_Q2,
               fig_Q3,
               nrows = 2,
               margin = 0.04,
               shareX = TRUE,
               shareY = TRUE,
               titleX = TRUE,
               titleY = TRUE)
fig <- fig %>% layout(title = list(text = "Benford-Analyse: Wirecard AG 2005 - 2012",
                                   font = list(family = "Garamond", size = 35),
                                   y = 1),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      showlegend = TRUE,
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x= 1.06,
                                    y= 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig



# chi-quadrat-test und MAD durchführen
# mit package "benford.analysis" lassen sich nur die Werte der 1. und 1.&2. ziffer berechnen
# folgend werden die Werte der 2. ziffer manuell ausgerechnet

# chi-quadrat  
Q1_chi <- chisq.test(Q1_data.dist, p = benford.dist)
Q1_chi[["statistic"]][["X-squared"]] * nrow(Q1)

Q2_chi <- chisq.test(Q2_data.dist, p = benford.dist)
Q2_chi[["statistic"]][["X-squared"]] * nrow(Q2)

Q3_chi <- chisq.test(Q3_data.dist, p = benford.dist)
Q3_chi[["statistic"]][["X-squared"]] * nrow(Q3)

JA_chi <- chisq.test(JA_data.dist, p = benford.dist)
JA_chi[["statistic"]][["X-squared"]] * nrow(JA)

# MAD
sum(abs(Q1_data.dist - benford.dist)/(length(benford.dist)))
sum(abs(Q2_data.dist - benford.dist)/(length(benford.dist)))
sum(abs(Q3_data.dist - benford.dist)/(length(benford.dist)))
sum(abs(JA_data.dist - benford.dist)/(length(benford.dist)))

# statistiken anzeigen (mean & median)
summary(Q1_2005_2012)
summary(Q2_2005_2012)
summary(Q3_2005_2012)
summary(JA_2005_2012)

# schiefe berechnen
skewness(Q1_2005_2012$THIS_YEAR)
skewness(Q2_2005_2012$THIS_YEAR)
skewness(Q3_2005_2012$THIS_YEAR)
skewness(JA_2005_2012$THIS_YEAR)




















# 1. & 2. ZIFFER - ANALYSE
# benford analyse performen (mithilfe 'benford.analysis' package)
JA = benford(JA_2005_2012$THIS_YEAR, number.of.digits = 2)
Q1 = benford(Q1_2005_2012$THIS_YEAR, number.of.digits = 2)
Q2 = benford(Q2_2005_2012$THIS_YEAR, number.of.digits = 2)
Q3 = benford(Q3_2005_2012$THIS_YEAR, number.of.digits = 2)

# daten in einem data.table speichern
JA_TABLE <- getBfd(JA)
Q1_TABLE <- getBfd(Q1)
Q2_TABLE <- getBfd(Q2)
Q3_TABLE <- getBfd(Q3)

# formatiere die wahrscheinlichkeiten in prozent
JA_WK <- percent(JA_TABLE$data.dist, 1)
Q1_WK <- percent(Q1_TABLE$data.dist, 1)
Q2_WK <- percent(Q2_TABLE$data.dist, 1)
Q3_WK <- percent(Q3_TABLE$data.dist, 1)
BENFORD_WK <- percent(JA_TABLE$benford.dist, 1)
JA_WK <- as.character(JA_WK)
Q1_WK <- as.character(Q1_WK)
Q2_WK <- as.character(Q2_WK)
Q3_WK <- as.character(Q3_WK)
BENFORD_WK <- as.character(BENFORD_WK)

# data.frame mit verwendeten daten erstellen
data_12 <- data.frame(JA_TABLE$digits,
                      JA_TABLE$data.dist,
                      Q1_TABLE$data.dist,
                      Q2_TABLE$data.dist,
                      Q3_TABLE$data.dist,
                      JA_WK,
                      Q1_WK,
                      Q2_WK,
                      Q3_WK,
                      JA_TABLE$benford.dist,
                      BENFORD_WK)



# PLOT mit SUBPLOTS
# JA - subplot
fig_JA <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$JA_TABLE.data.dist,
                  marker = list(color = "rgb(11, 80, 184)"),
                  name = "Jahresfinanzbericht",
                  type = "bar")
fig_JA <- fig_JA %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_JA <- fig_JA %>% layout(title = list(text = "Jahresfinanzbericht: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q1 - subplot
fig_Q1 <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$Q1_TABLE.data.dist,
                  marker = list(color = "rgb(219, 20, 20)"),
                  name = "Quartal 1",
                  type = "bar")
fig_Q1 <- fig_Q1 %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q1 <- fig_Q1 %>% layout(title = list(text = "Quartal 1: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q2 - subplot
fig_Q2 <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$Q2_TABLE.data.dist,
                  marker = list(color = "rgb(71, 128, 3)"),
                  name = "Quartal 2",
                  type = "bar")
fig_Q2 <- fig_Q2 %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q2 <- fig_Q2 %>% layout(title = list(text = "Quartal 2: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q3 - subplot
fig_Q3 <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$Q3_TABLE.data.dist,
                  marker = list(color = "rgb(61, 61, 61)"),
                  name = "Quartal 3",
                  type = "bar")
fig_Q3 <- fig_Q3 %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = TRUE)
fig_Q3 <- fig_Q3 %>% layout(title = list(text = "Quartal 3: 2005 - 2012",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# plot definieren
fig <- subplot(fig_JA,
               fig_Q1,
               fig_Q2,
               fig_Q3,
               nrows = 2,
               margin = 0.04,
               shareX = TRUE,
               shareY = TRUE,
               titleX = TRUE,
               titleY = TRUE)
fig <- fig %>% layout(title = list(text = "Benford-Analyse: Wirecard AG 2005 - 2012",
                                   font = list(family = "Garamond", size = 35),
                                   y = 1),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      showlegend = TRUE,
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x= 1.06,
                                    y= 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig



# chi-quadrat-test und MAD durchführen
# allgemeine auswertungen/ergebnisse der analyse anzeigen
print(Q1)
print(Q2)
print(Q3)
print(JA)

# chi-quadrat  
chisq(Q1)
chisq(Q2)
chisq(Q3)
chisq(JA)
# vergelich mit chi-quadrat-test aus "Benford.Tests"  
Q1_data.used <- Q1[["data"]][["data.used"]]
Q2_data.used <- Q2[["data"]][["data.used"]]
Q3_data.used <- Q3[["data"]][["data.used"]]
JA_data.used <- JA[["data"]][["data.used"]]
chisq.benftest(x = Q1_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q2_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q3_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = JA_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)

# MAD
MAD(Q1)
MAD(Q2)
MAD(Q3)
MAD(JA)

# statistiken anzeigen (mean & median)
summary(Q1_2005_2012)
summary(Q2_2005_2012)
summary(Q3_2005_2012)
summary(JA_2005_2012)

# schiefe berechnen
skewness(Q1_2005_2012$THIS_YEAR)
skewness(Q2_2005_2012$THIS_YEAR)
skewness(Q3_2005_2012$THIS_YEAR)
skewness(JA_2005_2012$THIS_YEAR)
































# 2013 - 2019 ###########
# importiere CSV's
JA_2013_2018 <- read_csv("JA_2013-2018.csv")
Q1_2013_2019 <- read_csv("Q1_2013-2019.csv")
Q2_2013_2019 <- read_csv("Q2_2013-2019.csv")
Q3_2013_2019 <- read_csv("Q3_2013-2019.csv")





# 1. ZIFFER - ANALYSE
# benford analyse performen (mithilfe 'benford.analysis' package)
JA = benford(JA_2013_2018$THIS_YEAR, number.of.digits = 1)
Q1 = benford(Q1_2013_2019$THIS_YEAR, number.of.digits = 1)
Q2 = benford(Q2_2013_2019$THIS_YEAR, number.of.digits = 1)
Q3 = benford(Q3_2013_2019$THIS_YEAR, number.of.digits = 1)

# daten in einem data.table speichern
JA_TABLE <- getBfd(JA)
Q1_TABLE <- getBfd(Q1)
Q2_TABLE <- getBfd(Q2)
Q3_TABLE <- getBfd(Q3)

# formatiere die wahrscheinlichkeiten in prozent
JA_WK <- percent(JA_TABLE$data.dist, 1)
Q1_WK <- percent(Q1_TABLE$data.dist, 1)
Q2_WK <- percent(Q2_TABLE$data.dist, 1)
Q3_WK <- percent(Q3_TABLE$data.dist, 1)
BENFORD_WK <- percent(JA_TABLE$benford.dist, 1)
JA_WK <- as.character(JA_WK)
Q1_WK <- as.character(Q1_WK)
Q2_WK <- as.character(Q2_WK)
Q3_WK <- as.character(Q3_WK)
BENFORD_WK <- as.character(BENFORD_WK)

# data.frame mit verwendeten daten erstellen
data_1 <- data.frame(JA_TABLE$digits,
                     JA_TABLE$data.dist,
                     Q1_TABLE$data.dist,
                     Q2_TABLE$data.dist,
                     Q3_TABLE$data.dist,
                     JA_WK,
                     Q1_WK,
                     Q2_WK,
                     Q3_WK,
                     JA_TABLE$benford.dist,
                     BENFORD_WK)


# PLOT mit SUBPLOTS
# JA - subplot
fig_JA <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$JA_TABLE.data.dist,
                  text = data_1$JA_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(11, 80, 184)"),
                  name = "Jahresfinanzbericht",
                  type = "bar")
fig_JA <- fig_JA %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_JA <- fig_JA %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_JA <- fig_JA %>% layout(title = list(text = "Jahresfinanzbericht: 2013 - 2018",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q1 - subplot
fig_Q1 <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$Q1_TABLE.data.dist,
                  text = data_1$Q1_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(219, 20, 20)"),
                  name = "Quartal 1",
                  type = "bar")
fig_Q1 <- fig_Q1 %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q1 <- fig_Q1 %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_Q1 <- fig_Q1 %>% layout(title = list(text = "Quartal 1: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q2 - subplot
fig_Q2 <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$Q2_TABLE.data.dist,
                  text = data_1$Q2_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(71, 128, 3)"),
                  name = "Quartal 2",
                  type = "bar")
fig_Q2 <- fig_Q2 %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q2 <- fig_Q2 %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_Q2 <- fig_Q2 %>% layout(title = list(text = "Quartal 2: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q3 - subplot
fig_Q3 <- plot_ly(x = data_1$JA_TABLE.digits,
                  y = data_1$Q3_TABLE.data.dist,
                  text = data_1$Q3_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(61, 61, 61)"),
                  name = "Quartal 3",
                  type = "bar")
fig_Q3 <- fig_Q3 %>% add_trace(y = data_1$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_1$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = TRUE)
fig_Q3 <- fig_Q3 %>% add_annotations(x = data_1$JA_TABLE.digits,
                                     y = data_1$JA_TABLE.benford.dist,
                                     text = data_1$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 30,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 17, color = "rgb(0, 0, 0)"))
fig_Q3 <- fig_Q3 %>% layout(title = list(text = "Quartal 3: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "10%", "20%", "30%"),
                                         tickvals = list(0, 0.1, 0.2, 0.3),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste Ziffer",
                                         ticktext = list("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# plot definieren
fig <- subplot(fig_JA,
               fig_Q1,
               fig_Q2,
               fig_Q3,
               nrows = 2,
               margin = 0.04,
               shareX = TRUE,
               shareY = TRUE,
               titleX = TRUE,
               titleY = TRUE)
fig <- fig %>% layout(title = list(text = "Benford-Analyse: Wirecard AG 2013 - 2019",
                                   font = list(family = "Garamond", size = 35),
                                   y = 1),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      showlegend = TRUE,
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x= 1.06,
                                    y= 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig



# chi-quadrat-test und MAD durchführen
# allgemeine auswertungen/ergebnisse der analyse anzeigen
print(Q1)
print(Q2)
print(Q3)
print(JA)

# chi-quadrat  
chisq(Q1)
chisq(Q2)
chisq(Q3)
chisq(JA)
# vergelich mit chi-quadrat-test aus "Benford.Tests"  
Q1_data.used <- Q1[["data"]][["data.used"]]
Q2_data.used <- Q2[["data"]][["data.used"]]
Q3_data.used <- Q3[["data"]][["data.used"]]
JA_data.used <- JA[["data"]][["data.used"]]
chisq.benftest(x = Q1_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q2_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q3_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = JA_data.used,
               digits = 1,
               pvalmethod = "asymptotic",
               pvalsims = 10000)

# MAD
MAD(Q1)
MAD(Q2)
MAD(Q3)
MAD(JA)

# statistiken anzeigen (mean & median)
summary(Q1_2013_2019)
summary(Q2_2013_2019)
summary(Q3_2013_2019)
summary(JA_2013_2018)

# schiefe berechnen
skewness(Q1_2013_2019$THIS_YEAR)
skewness(Q2_2013_2019$THIS_YEAR)
skewness(Q3_2013_2019$THIS_YEAR)
skewness(JA_2013_2018$THIS_YEAR)















# 2. ZIFFER - ANALYSE
# erstelle ein data.frame mit den ersten zwei ziffern der jeweiligen zaheln
JA <- extract.digits(JA_2013_2018$THIS_YEAR, number.of.digits = 2)
Q1 <- extract.digits(Q1_2013_2019$THIS_YEAR, number.of.digits = 2)
Q2 <- extract.digits(Q2_2013_2019$THIS_YEAR, number.of.digits = 2)
Q3 <- extract.digits(Q3_2013_2019$THIS_YEAR, number.of.digits = 2)

# extrahiere die 2. ziffer und erstelle eine neue spalte 'second_digit'
JA$second_digit <- as.integer(substr(JA$data.digits, 2, 2))
Q1$second_digit <- as.integer(substr(Q1$data.digits, 2, 2))
Q2$second_digit <- as.integer(substr(Q2$data.digits, 2, 2))
Q3$second_digit <- as.integer(substr(Q3$data.digits, 2, 2))

# mit package "benford.analysis" lassen sich nur die relativen häufigkeiten der 1. und 1.&2. ziffer berechnen
# folgend werden die relativen häufigkeiten der 2. ziffer manuell ausgerechnet 

# JA: absolute häufigkeit der ziffern berrechnen
s0 <- sum(JA$second_digit == 0)
s1 <- sum(JA$second_digit == 1)
s2 <- sum(JA$second_digit == 2)
s3 <- sum(JA$second_digit == 3)
s4 <- sum(JA$second_digit == 4)
s5 <- sum(JA$second_digit == 5)
s6 <- sum(JA$second_digit == 6)
s7 <- sum(JA$second_digit == 7)
s8 <- sum(JA$second_digit == 8)
s9 <- sum(JA$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# JA: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
JA_data.dist <- p

# Q1: absolute häufigkeit der ziffern berechenen
s0 <- sum(Q1$second_digit == 0)
s1 <- sum(Q1$second_digit == 1)
s2 <- sum(Q1$second_digit == 2)
s3 <- sum(Q1$second_digit == 3)
s4 <- sum(Q1$second_digit == 4)
s5 <- sum(Q1$second_digit == 5)
s6 <- sum(Q1$second_digit == 6)
s7 <- sum(Q1$second_digit == 7)
s8 <- sum(Q1$second_digit == 8)
s9 <- sum(Q1$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# Q1: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
Q1_data.dist <- p

# Q2: absolute häufigkeit der ziffern berrechnen
s0 <- sum(Q2$second_digit == 0)
s1 <- sum(Q2$second_digit == 1)
s2 <- sum(Q2$second_digit == 2)
s3 <- sum(Q2$second_digit == 3)
s4 <- sum(Q2$second_digit == 4)
s5 <- sum(Q2$second_digit == 5)
s6 <- sum(Q2$second_digit == 6)
s7 <- sum(Q2$second_digit == 7)
s8 <- sum(Q2$second_digit == 8)
s9 <- sum(Q2$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# Q2: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
Q2_data.dist <- p

# Q3: absolute häufigkeit der ziffern berrechnen
s0 <- sum(Q3$second_digit == 0)
s1 <- sum(Q3$second_digit == 1)
s2 <- sum(Q3$second_digit == 2)
s3 <- sum(Q3$second_digit == 3)
s4 <- sum(Q3$second_digit == 4)
s5 <- sum(Q3$second_digit == 5)
s6 <- sum(Q3$second_digit == 6)
s7 <- sum(Q3$second_digit == 7)
s8 <- sum(Q3$second_digit == 8)
s9 <- sum(Q3$second_digit == 9)
s <- s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9
# Q3: relative häufigkeit der ziffern berrechnen
p0 <- s0/s
p1 <- s1/s
p2 <- s2/s
p3 <- s3/s
p4 <- s4/s
p5 <- s5/s
p6 <- s6/s
p7 <- s7/s
p8 <- s8/s
p9 <- s9/s
p <- c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
Q3_data.dist <- p

# ziffern speichern
digits <- c(0:9)

# benford-wahrscheinlichkeiten für die 2. ziffer speichern
benford.dist <- c(0.11967927, 
                  0.11389010, 
                  0.10882150, 
                  0.10432956, 
                  0.10030820, 
                  0.09667724, 
                  0.09337474, 
                  0.09035199, 
                  0.08757005, 
                  0.08499735) 

# formatiere die wahrscheinlichkeiten in prozent
JA_WK <- percent(JA_data.dist, 1)
Q1_WK <- percent(Q1_data.dist, 1)
Q2_WK <- percent(Q2_data.dist, 1)
Q3_WK <- percent(Q3_data.dist, 1)
BENFORD_WK <- percent(benford.dist, 1)
JA_WK <- as.character(JA_WK)
Q1_WK <- as.character(Q1_WK)
Q2_WK <- as.character(Q2_WK)
Q3_WK <- as.character(Q3_WK)
BENFORD_WK <- as.character(BENFORD_WK)

# data.frame mit verwendeten daten erstellen
data_2 <- data.frame(digits,
                     JA_data.dist,
                     Q1_data.dist,
                     Q2_data.dist,
                     Q3_data.dist,
                     JA_WK,
                     Q1_WK,
                     Q2_WK,
                     Q3_WK,
                     benford.dist,
                     BENFORD_WK)



# PLOT mit SUBPLOTS
# JA - subplot
fig_JA <- plot_ly(x = data_2$digits,
                  y = data_2$JA_data.dist,
                  text = data_2$JA_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(11, 80, 184)"),
                  name = "Jahresfinanzbericht",
                  type = "bar")
fig_JA <- fig_JA %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_JA <- fig_JA %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_JA <- fig_JA %>% layout(title = list(text = "Jahresfinanzbericht: 2013 - 2018",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q1 - subplot
fig_Q1 <- plot_ly(x = data_2$digits,
                  y = data_2$Q1_data.dist,
                  text = data_2$Q1_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(219, 20, 20)"),
                  name = "Quartal 1",
                  type = "bar")
fig_Q1 <- fig_Q1 %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q1 <- fig_Q1 %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_Q1 <- fig_Q1 %>% layout(title = list(text = "Quartal 1: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q2 - subplot 
fig_Q2 <- plot_ly(x = data_2$digits,
                  y = data_2$Q2_data.dist,
                  text = data_2$Q2_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(71, 128, 3)"),
                  name = "Quartal 2",
                  type = "bar")
fig_Q2 <- fig_Q2 %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q2 <- fig_Q2 %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_Q2 <- fig_Q2 %>% layout(title = list(text = "Quartal 2: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q3 - subplot 
fig_Q3 <- plot_ly(x = data_2$digits,
                  y = data_2$Q3_data.dist,
                  text = data_2$Q3_WK, textposition = "inside", insidetextanchor = "start",
                  marker = list(color = "rgb(61, 61, 61)"),
                  name = "Quartal 3",
                  type = "bar")
fig_Q3 <- fig_Q3 %>% add_trace(y = data_2$benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_2$BENFORD_WK,
                               line = list(width = 6, color = "rgba(0, 0, 0, 0.8)"),
                               marker = list(size = 9, color = "rgba(173, 166, 168, 0.6)", 
                                             line = list(width = 2, color = "rgba(0, 0, 0, 0.8)")),
                               name = "Benford-Verteilung",
                               showlegend = TRUE)
fig_Q3 <- fig_Q3 %>% add_annotations(x = data_2$digits,
                                     y = data_2$benford.dist,
                                     text = data_2$BENFORD_WK,
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 4,
                                     arrowsize = 1,
                                     arrowwidth = 1.2,
                                     ax = 25,
                                     ay = -40,
                                     font = list(family = "Garamond", size = 14, color = "rgb(0, 0, 0)"))
fig_Q3 <- fig_Q3 %>% layout(title = list(text = "Quartal 3: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "5%", "10%", "15%"),
                                         tickvals = list(0, 0.05, 0.1, 0.15),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Zweite Ziffer",
                                         ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                         tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# plot definieren
fig <- subplot(fig_JA,
               fig_Q1,
               fig_Q2,
               fig_Q3,
               nrows = 2,
               margin = 0.04,
               shareX = TRUE,
               shareY = TRUE,
               titleX = TRUE,
               titleY = TRUE)
fig <- fig %>% layout(title = list(text = "Benford-Analyse: Wirecard AG 2013 - 2019",
                                   font = list(family = "Garamond", size = 35),
                                   y = 1),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      showlegend = TRUE,
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x= 1.06,
                                    y= 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig



# chi-quadrat-test und MAD durchführen
# mit package "benford.analysis" lassen sich nur die Werte der 1. und 1.&2. ziffer berechnen
# folgend werden die Werte der 2. ziffer manuell ausgerechnet

# chi-quadrat  
Q1_chi <- chisq.test(Q1_data.dist, p = benford.dist)
Q1_chi[["statistic"]][["X-squared"]] * nrow(Q1)

Q2_chi <- chisq.test(Q2_data.dist, p = benford.dist)
Q2_chi[["statistic"]][["X-squared"]] * nrow(Q2)

Q3_chi <- chisq.test(Q3_data.dist, p = benford.dist)
Q3_chi[["statistic"]][["X-squared"]] * nrow(Q3)

JA_chi <- chisq.test(JA_data.dist, p = benford.dist)
JA_chi[["statistic"]][["X-squared"]] * nrow(JA)

# MAD
sum(abs(Q1_data.dist - benford.dist)/(length(benford.dist)))
sum(abs(Q2_data.dist - benford.dist)/(length(benford.dist)))
sum(abs(Q3_data.dist - benford.dist)/(length(benford.dist)))
sum(abs(JA_data.dist - benford.dist)/(length(benford.dist)))

# statistiken anzeigen (mean & median)
summary(Q1_2013_2019)
summary(Q2_2013_2019)
summary(Q3_2013_2019)
summary(JA_2013_2018)

# schiefe berechnen
skewness(Q1_2013_2019$THIS_YEAR)
skewness(Q2_2013_2019$THIS_YEAR)
skewness(Q3_2013_2019$THIS_YEAR)
skewness(JA_2013_2018$THIS_YEAR)













# 1. & 2. ZIFFER - ANALYSE
# benford analyse performen (mithilfe 'benford.analysis' package)
JA = benford(JA_2013_2018$THIS_YEAR, number.of.digits = 2)
Q1 = benford(Q1_2013_2019$THIS_YEAR, number.of.digits = 2)
Q2 = benford(Q2_2013_2019$THIS_YEAR, number.of.digits = 2)
Q3 = benford(Q3_2013_2019$THIS_YEAR, number.of.digits = 2)

# daten in einem data.table speichern
JA_TABLE <- getBfd(JA)
Q1_TABLE <- getBfd(Q1)
Q2_TABLE <- getBfd(Q2)
Q3_TABLE <- getBfd(Q3)

# formatiere die wahrscheinlichkeiten in prozent
JA_WK <- percent(JA_TABLE$data.dist, 1)
Q1_WK <- percent(Q1_TABLE$data.dist, 1)
Q2_WK <- percent(Q2_TABLE$data.dist, 1)
Q3_WK <- percent(Q3_TABLE$data.dist, 1)
BENFORD_WK <- percent(JA_TABLE$benford.dist, 1)
JA_WK <- as.character(JA_WK)
Q1_WK <- as.character(Q1_WK)
Q2_WK <- as.character(Q2_WK)
Q3_WK <- as.character(Q3_WK)
BENFORD_WK <- as.character(BENFORD_WK)

# data.frame mit verwendeten daten erstellen
data_12 <- data.frame(JA_TABLE$digits,
                      JA_TABLE$data.dist,
                      Q1_TABLE$data.dist,
                      Q2_TABLE$data.dist,
                      Q3_TABLE$data.dist,
                      JA_WK,
                      Q1_WK,
                      Q2_WK,
                      Q3_WK,
                      JA_TABLE$benford.dist,
                      BENFORD_WK)



# PLOT mit SUBPLOTS
# JA - subplot
fig_JA <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$JA_TABLE.data.dist,
                  marker = list(color = "rgb(11, 80, 184)"),
                  name = "Jahresfinanzbericht",
                  type = "bar")
fig_JA <- fig_JA %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_JA <- fig_JA %>% layout(title = list(text = "Jahresfinanzbericht: 2013 - 2018",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q1 - subplot
fig_Q1 <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$Q1_TABLE.data.dist,
                  marker = list(color = "rgb(219, 20, 20)"),
                  name = "Quartal 1",
                  type = "bar")
fig_Q1 <- fig_Q1 %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q1 <- fig_Q1 %>% layout(title = list(text = "Quartal 1: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q2 - subplot
fig_Q2 <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$Q2_TABLE.data.dist,
                  marker = list(color = "rgb(71, 128, 3)"),
                  name = "Quartal 2",
                  type = "bar")
fig_Q2 <- fig_Q2 %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = FALSE)
fig_Q2 <- fig_Q2 %>% layout(title = list(text = "Quartal 2: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# Q3 - subplot
fig_Q3 <- plot_ly(x = data_12$JA_TABLE.digits,
                  y = data_12$Q3_TABLE.data.dist,
                  marker = list(color = "rgb(61, 61, 61)"),
                  name = "Quartal 3",
                  type = "bar")
fig_Q3 <- fig_Q3 %>% add_trace(y = data_12$JA_TABLE.benford.dist,
                               type = "scatter",
                               mode = "lines+markers",
                               text = data_12$BENFORD_WK,
                               line = list(width = 3, color = "rgb(0, 0, 0)"),
                               marker = list(size = 0.1, color = "rgb(0, 0, 0)"),
                               name = "Benford-Verteilung",
                               showlegend = TRUE)
fig_Q3 <- fig_Q3 %>% layout(title = list(text = "Quartal 3: 2013 - 2019",
                                         font = list(family = "Garamond", size = 28),
                                         y = 0.995),
                            yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                         ticktext = list("0%", "1%", "2%", "3%", "4%", "5%", "6%", "7%"),
                                         tickvals = list(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            xaxis = list(title = "Erste & Zweite Ziffer",
                                         titlefont = list(family = "Garamond", size = 30),
                                         tickfont = list(family = "Garamond", size = 25)),
                            legend = list(font = list(family = "Garamond", size = 17)))

# plot definieren
fig <- subplot(fig_JA,
               fig_Q1,
               fig_Q2,
               fig_Q3,
               nrows = 2,
               margin = 0.04,
               shareX = TRUE,
               shareY = TRUE,
               titleX = TRUE,
               titleY = TRUE)
fig <- fig %>% layout(title = list(text = "Benford-Analyse: Wirecard AG 2013 - 2019",
                                   font = list(family = "Garamond", size = 35),
                                   y = 1),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      showlegend = TRUE,
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x= 1.06,
                                    y= 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig



# chi-quadrat-test und MAD durchführen
# allgemeine auswertungen/ergebnisse der analyse anzeigen
print(Q1)
print(Q2)
print(Q3)
print(JA)

# chi-quadrat  
chisq(Q1)
chisq(Q2)
chisq(Q3)
chisq(JA)
# vergelich mit chi-quadrat-test aus "Benford.Tests"  
Q1_data.used <- Q1[["data"]][["data.used"]]
Q2_data.used <- Q2[["data"]][["data.used"]]
Q3_data.used <- Q3[["data"]][["data.used"]]
JA_data.used <- JA[["data"]][["data.used"]]
chisq.benftest(x = Q1_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q2_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = Q3_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)
chisq.benftest(x = JA_data.used,
               digits = 2,
               pvalmethod = "asymptotic",
               pvalsims = 10000)

# MAD
MAD(Q1)
MAD(Q2)
MAD(Q3)
MAD(JA)

# statistiken anzeigen (mean & median)
summary(Q1_2013_2019)
summary(Q2_2013_2019)
summary(Q3_2013_2019)
summary(JA_2013_2018)

# schiefe berechnen
skewness(Q1_2013_2019$THIS_YEAR)
skewness(Q2_2013_2019$THIS_YEAR)
skewness(Q3_2013_2019$THIS_YEAR)
skewness(JA_2013_2018$THIS_YEAR)











