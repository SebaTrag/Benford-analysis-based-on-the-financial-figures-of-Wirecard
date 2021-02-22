#############################################################################################
# HSG BA: "Benford-Analyse anhand des Fallbeispiels der Wirecard AG"
# Relative Häufigkeiten der ersten, zweiten & dritten Ziffer plotten
# 
# S. Tragust, Partschins
# Erste Version: Februar 11, 2021
#############################################################################################




# VORBEREITUNG -------------------------------------

# installiere packages
install.packages("ggplot2") # erstellen von grafiken/plots (wird für 'plotly'-package benötigt)
install.packages("plotly") # erstellen von interaktiven grafiken/plots/tabellen aus 'ggplot2'-graphen 

# lade packages
library(ggplot2)
library(plotly)

















# PROGRAMM -----------------------------------------

d <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
D1 <- c("NA", 0.3010,	0.1761,	0.1249,	0.0969,	0.0792,	0.0669,	0.0580,	0.0512,	0.0458)
D2 <- c(0.1197,	0.1139,	0.1088,	0.1043,	0.1003,	0.0967,	0.0934,	0.0904,	0.0876,	0.0850)
D3 <- c(0.1018,	0.1014,	0.1010,	0.1006,	0.1002,	0.0998,	0.0994,	0.0990,	0.0986,	0.0983)



fig <- plot_ly(x = d,
               y = D1,
               name = "Erste signifikante Ziffer\n n = 1",
               type = "scatter",
               mode = "lines",
               line = list(color = "rgb(3, 176, 0)", width = 5))
fig <- fig %>% add_trace(y = D2,
                         name = "Zweite signifikante Ziffer\n n = 2",
                         line = list(color = "rgb(0, 145, 255)", width = 6, dash = "dot"))
fig <- fig %>% add_trace(y = D3, 
                         name = "Dritte signifikante Ziffer\n n = 3",
                         line = list(color = "rgb(255, 77, 0)", width = 6, dash = "dash"))
fig <- fig %>% layout(yaxis = list(title = list(text = "Relative Häufigkeit", standoff = 30L),
                                   ticktext = list("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%"),
                                   tickvals = list(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35),
                                   titlefont = list(family = "Garamond", size = 32),
                                   tickfont = list(family = "Garamond", size = 28),
                                   rangemode = "tozero",
                                   showline = TRUE,
                                   showgrid = TRUE),
                      xaxis = list(title = "Ziffer",
                                   ticktext = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                   tickvals = list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                   titlefont = list(family = "Garamond", size = 32),
                                   tickfont = list(family = "Garamond", size = 28),
                                   rangemode = "tozero",
                                   showline = TRUE,
                                   showgrid = FALSE),
                      legend = list(font = list(family = "Garamond", size = 25)),
                      images = list(source = "https://www.unisg.ch/-/media/d5b9607a44064498a9c920ca5c2cef52.jpg",
                                    xref = "paper",
                                    yref = "paper",
                                    x = 1.14,
                                    y = 0.01,
                                    sizex = 0.30,
                                    sizey = 0.25))
fig




