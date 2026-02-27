#--------------------------------------------

# Plots in Pdf speichern
pdf("KIPlots.pdf", width = 10, height = 5)
par("mar" = c(5, 4, 1.5, 2))
par(cex.lab = 1.3, cex.axis = 1.3)
#-----------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

#Einlesen der ersten Tabelle
daten101 <- read_csv("Fragenauswertung - 101 Wer darf wählen_.csv")
#Einlesen der weiteren Tabllen 
daten102 <- read_csv("Fragenauswertung - 102 Wo kann ich wählen_.csv")
daten103 <- read_csv("Fragenauswertung - 103 Was wird gewählt_.csv")
daten104 <- read_csv("Fragenauswertung - 104 Briefwahl.csv")
daten105 <- read_csv("Fragenauswertung - 105 Informieren.csv")
daten201 <- read_csv("Fragenauswertung - 201 Zusammenfassung.csv")
daten202 <- read_csv("Fragenauswertung - 202 Wirtschaft.csv")
daten203 <- read_csv("Fragenauswertung - 203 Bildung.csv")
daten204 <- read_csv("Fragenauswertung - 204 Zuwanderung.csv")
daten205 <- read_csv("Fragenauswertung - 205 Lebensqualität.csv")


#Faktorisieren von character Variablen für Frage 101
daten101 <- daten101 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Faktorisieren von character Variablen für Frage 102
daten102 <- daten102 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Faktorisieren von character Variablen für Frage 103
daten103 <- daten103 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 104
daten104 <- daten104 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 105
daten105 <- daten105 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Erste Zeile löschen da keine Inhaltlichen Daten
daten201 <- daten201[-1, ]
#Faktorisieren von character Variablen für Frage 201
daten201 <- daten201 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum ,
      as.factor
    )
  )
#Numeric aus Ausführlichkeits Variablen machen daraus machen
daten201 <- daten201 %>%
  mutate(
    across(
      c(
        `Ausführlichkeit  2.1`,
        `Ausführlichkeit  2.2`,
        `Ausführlichkeit  2.3`,
        `Ausführlichkeit  2.4`,
        `Ausführlichkeit  2.5`,
        `Ausführlichkeit  2.6`,
        `Ausführlichkeit  2.7`,
        `Ausführlichkeit  2.8`,
        `Ausführlichkeit  3`
      ),
      ~ as.numeric(as.character(.x))
    )
  )

#Faktorisieren von character Variablen für Frage 202
daten202 <- daten202 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Faktorisieren von character Variablen für Frage 203
daten203 <- daten203 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 204
daten204 <- daten204 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 205
daten205 <- daten205 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Verkürzung der Variablennamen 
names(daten101)[names(daten101) == "Ausführlichkeit 4"] <- "Ausf"
names(daten101)[names(daten101) == "KI-Modell"] <- "KI"
names(daten102)[names(daten102) == "Ausführlichkeit 4"] <- "Ausf"
names(daten102)[names(daten102) == "KI-Modell"] <- "KI"
names(daten103)[names(daten103) == "KI-Modell"] <- "KI"
names(daten104)[names(daten104) == "Ausführlichkeit 3"] <- "Ausf"
names(daten104)[names(daten104) == "KI-Modell"] <- "KI"
names(daten105)[names(daten105) == "Ausführlichkeit 5"] <- "Ausf"
names(daten105)[names(daten105) == "KI-Modell"] <- "KI"
#Eindeutiger Bennenung der ausführlichkeit bei 201
names(daten201)[names(daten201) == "Ausführlichkeit  2.1"] <- "Ausf_CDU"
names(daten201)[names(daten201) == "Ausführlichkeit  2.2"] <- "Ausf_SPD"
names(daten201)[names(daten201) == "Ausführlichkeit  2.3"] <- "Ausf_AFD"
names(daten201)[names(daten201) == "Ausführlichkeit  2.4"] <- "Ausf_Gruene"
names(daten201)[names(daten201) == "Ausführlichkeit  2.5"] <- "Ausf_Linke"
names(daten201)[names(daten201) == "Ausführlichkeit  2.6"] <- "Ausf_FDP"
names(daten201)[names(daten201) == "Ausführlichkeit  2.7"] <- "Ausf_FW"
names(daten201)[names(daten201) == "Ausführlichkeit  2.8"] <- "Ausf_sonstige"
names(daten201)[names(daten201) == "Ausführlichkeit  3"] <- "Ausf_insgesamt"
names(daten201)[names(daten201) == "KI-Modell"] <- "KI"
#Weitere Verkürzung der Variablennamen 
names(daten202)[names(daten202) == "Ausführlichkeit 4"] <- "Ausf"
names(daten202)[names(daten202) == "KI-Modell"] <- "KI"
names(daten203)[names(daten203) == "Ausführlichkeit 4"] <- "Ausf"
names(daten203)[names(daten203) == "KI-Modell"] <- "KI"
names(daten204)[names(daten204) == "Ausführlichkeit 4"] <- "Ausf"
names(daten204)[names(daten204) == "KI-Modell"] <- "KI"
names(daten205)[names(daten205) == "Ausführlichkeit 4"] <- "Ausf"
names(daten205)[names(daten205) == "KI-Modell"] <- "KI"
#Extra Variable für Parteinnamen
parteinamen <- c("CDU","SPD","AfD","Grüne","Linke","FDP","Freie Wähler","Sonstige")

#=========== Frage 101 ===============
#Durchschnittliche Ausführlichkeit pro Modell und Land
aggregate(Ausf ~ KI + Land,
          data = daten101,
          FUN = mean)
#             KI              Land     Ausf
# 1 Chat-GPT 5.1 Baden-Württenberg 1466.400
# 2  Gemini FAST Baden-Württenberg 1580.750
# 3 Chat-GPT 5.1   Rheinland-Pfalz 1474.200
# 4  Gemini FAST   Rheinland-Pfalz 1527.167

# Häufigkeit korrekter Antworten
table(daten101$KI, daten101$'Korrekteit 2.1')

#              1 - diese Antwort ist richtig  2 - diese Antwort ist falsch
# Chat-GPT 5.1                             8                            2
# Gemini FAST                             10                            0

# Zusammenhang zwischen Modell und Korrektheit
mosaicplot(~ KI + `Korrekteit 2.1`,
           data=daten101,
           color=TRUE,
           main="Zusammenhang zwischen Modell und Korrektheit - Frage 101")

#Antwortqualität nach Modell und Land
ggplot(daten101,
       aes(x=`Korrekteit 2.1`, fill=KI)) +
  geom_bar(position="dodge") +
  facet_wrap(~Land) +
  labs(title="Antwortqualität nach Modell und Land - Frage 101",
       x="Bewertung der Antwort", y="Anzahl")

# Zusammenhang zwischen Korrektheit und Ausführlichkeit
boxplot(Ausf ~ `Korrekteit 2.1`,
        data = daten101,
        xlab="Korrektheit",
        ylab="Zeichenanzahl",
        main="Antwortlänge nach Richtigkeit")

daten101$Korr_num <- ifelse(daten101$`Korrekteit 2.1` == 
                              "1 - diese Antwort ist richtig", 1, 0)
cor(daten101$Ausf, daten101$Korr_num)
# [1] 0.3493144
#-> Längere Antworten sind tendenziell korrekter.

#Häufigkeit genannter Kontexte
for (var in c("Kontext 3.1","Kontext 3.2","Kontext 3.3",
              "Kontext 3.4","Kontext 3.5")) {
  cat("\n", var, "\n")
  print(table(daten101[[var]]))
}


#Boxplot Ausführlichkeit nach Ländern für Frage 101
boxplot(
  Ausf ~ Land,
  data = daten101,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 101 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 101
boxplot(
  Ausf ~ KI,
  data = daten101,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 101 nach KI-Modell sortiert "
)


#Histogramm der Ausführlichkeit von Frage 101
hist(daten101$Ausf,
     ylab = "Häufigkeit",
     xlab = "Anzahl der Zeichen inkl. Leerzeichen",
     main ="Histogramm der Ausführlichkeit bei 'Wer darf wählen'")

#=========== Frage 102 ===============
# Häufigkeit korrekter Antworten
korrektheit102 <- daten102 %>%
  group_by(KI, `Korrektheit 2`) %>%
  summarise(Anzahl = n(), .groups = "drop") %>%
  group_by(KI) %>%
  mutate(Prozent = Anzahl / sum(Anzahl) * 100)

#korrektheit102
#KI           `Korrektheit 2`                         Anzahl Prozent
# 1 Chat-GPT 5.1 1 - diese Antwort ist richtig & vollst…     8      80
#2 Chat-GPT 5.1 2 - diese Antwort ist richtig & unvoll…      2      20
#3 Gemini FAST  1 - diese Antwort ist richtig & vollst…     10     100

#Herausfilter wo die unvollstädigkeit gelegen hat
unvollstaendig102 <- daten102 %>%
  filter(grepl("2", `Korrektheit 2`)) %>%
  select(KI, `Antwort 1.1`)

#unvollstaendig102
#KI           `Antwort 1.1` 
#1 Chat-GPT 5.1 1- im Wahlbüro
#2 Chat-GPT 5.1 1- im Wahlbüro

#Boxplot Ausführlichkeit nach Ländern für Frage 102
boxplot(
  Ausf ~ Land,
  data = daten102,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 102 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 102
boxplot(
  Ausf ~ KI,
  data = daten102,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 102 nach KI-Modell sortiert "
)
#=========== Frage 103 ===============

korrektheit103 <- daten103 %>%
  filter(!is.na(`Korrektheit 1.2`)) %>%
  group_by(KI, `Korrektheit 1.2`) %>%
  summarise(Anzahl = n(),.groups = "drop") %>%
  group_by(KI) %>%
  mutate(Prozent = Anzahl / sum(Anzahl) * 100)
korrektheit103
#  KI           `Korrektheit 1.2`             Anzahl Prozent
#1 Chat-GPT 5.1 1 - diese Antwort ist richtig      9     100
#2 Gemini FAST  1 - diese Antwort ist richtig     10     100


#Boxplot Ausführlichkeit nach Ländern für Frage 103
boxplot(
  Ausführlichkeit ~ Land,
  data = daten103,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 103 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 103
boxplot(
  Ausführlichkeit ~ KI,
  data = daten103,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 103 nach KI-Modell sortiert "
)

#=========== Frage 104 ===============

#Boxplot Ausführlichkeit nach Ländern für Frage 104
boxplot(
  Ausf ~ Land,
  data = daten104,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 104 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 104
boxplot(
  Ausf ~ KI,
  data = daten104,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 104 nach KI-Modell sortiert "
)

#=========== Frage 105 ===============
#Boxplot Ausführlichkeit nach Ländern für Frage 105
boxplot(
  Ausf ~ Land,
  data = daten105,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 105 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 105
boxplot(
  Ausf ~ KI,
  data = daten105,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 105 nach KI-Modell sortiert "
)


quelle_vars <- c("Quelle 2.1","Quelle 2.2","Quelle 2.3","Quelle 2.4")

haeufigkeiten_quellen <- sapply(quelle_vars,
                                function(q) sum(grepl("1", daten105[[q]]))
)

context_vars <- c("Kontext 3.1","Kontext 3.2","Kontext 3.3")
haeufigkeiten_kontext <- sapply(context_vars,
                                function(k) sum(grepl("1", daten105[[k]]))
)
barplot(haeufigkeiten_quellen,
        names.arg = c("ÖR Medien","Vereine/Verbände",
                      "Parteiseiten","Offizielle Seiten \n (Bund/Land)"),
        main = "Häufigkeit genannter Quellen (Frage 105)",
        xlab = "Quellenart", ylab = "Anzahl der Nennungen",
        col = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252")) 

barplot(haeufigkeiten_kontext,
        names.arg=c("Unabhängigkeit betont",
                    "Selbst informieren",
                    "Weitere Tipps zur Wahl"),
        main="Häufigkeit zusätzlicher Kontexte (Frage 105)",
        xlab="Aspekt", ylab="Nennungen",
        col=c("#d9d9d9",  "#969696", "#525252"))
      
        
bias_vars <- c("Bias 4.1","Bias 4.2","Bias 4.3","Bias 4.4")
bias_counts <- sapply(bias_vars,
                      function(b) sum(grepl("1", daten105[[b]])))

barplot(bias_counts,
        names.arg=c("Wertung vorhanden",
                    "ÖR vertrauenswürdiger dargestellt",
                    "Keine Wertung vorhanden",
                    "Explizit verlässlich dargestellt"),
        main="Darstellung von Vertrauenswürdigkeit (Frage 105)",
        xlab="Bewertungsaspekt", ylab="Anzahl der Fälle",
        col=c("red3","seagreen","grey80","steelblue"))
#=========== Frage 201 ===============
parteifarben <- c(
  "grey30",        # CDU
  "red",          # SPD
  "deepskyblue3", # AfD
  "#64A12D",    # Grüne
  "purple",       # Linke
  "gold",         # FDP
  "orange",       # Freie Wähler
  "grey60"        # Sonstige
)
# wie oft jede Partei erwähnt wird:
haeufigkeiten <- sapply(parteinamen,
                        function(p) sum(grepl("1", daten201[[paste0("Antwort 1.", which(parteinamen==p))]])))
barplot(haeufigkeiten, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen",
        xlab="Partei", ylab="Anzahl der Nennungen", col=parteifarben)


#Boxplot Ausführlichkeit der Partein
boxplot(
  daten201[, c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
               "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
               "Ausf_FW", "Ausf_sonstige")],
  las=1,
  names = parteinamen,
  col = parteifarben,
  main = "Vergleich der Antwortlänge je nach Partei",
  ylab = "Zeichenanzahl der Antwort"
)

#===========


#Aufteilung nach Bundesländern
daten_long <- daten201 |>
  pivot_longer(
    cols = c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
             "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
             "Ausf_FW", "Ausf_sonstige"),
    names_to = "Partei",
    values_to = "Antwortlaenge"
  )

# Schöne Parteinamen
daten_long$Partei <- factor(
  daten_long$Partei,
  levels = c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
             "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
             "Ausf_FW", "Ausf_sonstige"),
  labels = parteinamen
)

par(mar = c(4, 5, 6, 1))   # unten, links, oben, rechts

bp <-boxplot(
  Antwortlaenge ~ interaction(Partei, Land, lex.order = TRUE),
  data = daten_long,
  col = rep(parteifarben, each = 2),
  border = "black",
  las = 1,
  xaxt = "n",
  main = "Antwortlänge nach Partei und Land",
  ylab = "Zeichenanzahl der Antwort",
  xlab = ""
)

# Neue Labels definieren: abwechselnd BaWü / RP für jede Partei
axis(1,
     at = seq_along(bp$names),
     labels = rep(c("BaWü","RP"), times=length(unique(daten_long$Partei))),
     las=0.7,
     tick = FALSE)
# 
# # Reihenfolge der Boxen entspricht bp$names
# laender <- sub(".*\\.", "", bp$names)  # extrahiert Land aus "Partei.Land"
# axis(
#   side = 1,
#   at = 1:length(bp$names),
#   labels = laender,
#   tick = FALSE,
#   cex.axis = 0.71
# )
# Parteinamen extrahieren
parteien <- sub("\\..*", "", bp$names)

# Mittelpunkte der Zweierpaare
unique_parteien <- unique(parteien)
mitten <- sapply(unique_parteien, function(p) mean(which(parteien == p)))

axis(
  side = 3,
  at = mitten,
  labels = unique_parteien,
  tick = FALSE,
  cex.axis = 0.9
)

#Zurücksetzen der Grafikeinstellungen 
par("mar" = c(5, 4, 1.5, 2))
par(cex.lab = 1.3, cex.axis = 1.3)
#===========
#Boxplot Ausführlichkeit nach Ländern für Frage 201
boxplot(
  Ausf_insgesamt ~ Land,
  data = daten201,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 201 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 201
boxplot(
  Ausf_insgesamt ~ KI,
  data = daten201,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 201 nach KI-Modell sortiert "
)
#=========== Frage 202 ===============
#Barplot wie oft jede Partei genannt wird
haeufigkeiten202 <- sapply(parteinamen,
 function(p) sum(grepl("1", daten202[[paste0("Antwort Erwähnung 1.",
                                             which(parteinamen==p))]])))
barplot(haeufigkeiten202, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen beim Thema Wirtschaft",
        xlab="Partei", ylab="Anzahl der Nennungen", col=parteifarben)


#Boxplot Ausführlichkeit nach Ländern für Frage 202
boxplot(
  Ausf ~ Land,
  data = daten202,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 202 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 202
boxplot(
  Ausf ~ KI,
  data = daten202,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 202 nach KI-Modell sortiert "
)

#Barplot mit Haltungen 

# Matrix erstellen: Zeilen = Haltung, Spalten = Partei
haeufigkeiten202_matrix <- sapply(1:8, function(i) {
  spalte <- paste0("Antwort Wertung 2.", i)
  werte <- daten202[[spalte]]
  
  # Zählen mit grepl, wie in deinem ursprünglichen Ansatz
  c(
    positiv = sum(grepl("1", werte), na.rm = TRUE),
    neutral  = sum(grepl("3", werte), na.rm = TRUE),
    negativ  = sum(grepl("2", werte), na.rm = TRUE)
  )
})

# Zeilen- und Spaltennamen setzen
colnames(haeufigkeiten202_matrix) <- parteinamen

# Gestapelter Barplot
barplot(haeufigkeiten202_matrix,
        beside = FALSE,
        col = c("green4","grey","red3"),
        main = "Parteiempflungen zum Thema Wirtschaft",
        xlab = "Partei",
        ylab = "Anzahl der Nennungen")
legend("top", legend = rownames(haeufigkeiten202_matrix),
       fill = c("green4","grey","red3"), bty = "n", cex = 0.9)

#=========== Frage 203 ===============
#Barplot wie oft jede Partei genannt wird
haeufigkeiten203 <- sapply(parteinamen,
                           function(p) sum(grepl("1", daten203[[paste0("Antwort Erwähnung 1.",
                                                                       which(parteinamen==p))]])))
barplot(haeufigkeiten203, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen beim Thema Bildung",
        xlab="Partei", ylab="Anzahl der Nennungen", col= parteifarben)

#Boxplot Ausführlichkeit nach Ländern für Frage 203
boxplot(
  Ausf ~ Land,
  data = daten203,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 203 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 203
boxplot(
  Ausf ~ KI,
  data = daten203,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 203 nach KI-Modell sortiert "
)

#Barplot mit Haltungen 

# Matrix erstellen: Zeilen = Haltung, Spalten = Partei
haeufigkeiten203_matrix <- sapply(1:8, function(i) {
  spalte <- paste0("Antwort Wertung 2.", i)
  werte <- daten203[[spalte]]
  
  # Zählen mit grepl, wie in deinem ursprünglichen Ansatz
  c(
    positiv = sum(grepl("1", werte), na.rm = TRUE),
    neutral  = sum(grepl("3", werte), na.rm = TRUE),
    negativ  = sum(grepl("2", werte), na.rm = TRUE)
  )
})

# Zeilen- und Spaltennamen setzen
colnames(haeufigkeiten203_matrix) <- parteinamen

# Gestapelter Barplot
barplot(haeufigkeiten203_matrix,
        beside = FALSE,
        col = c("green4","grey","red3"),
        main = "Parteiempflungen zum Thema Bildung",
        xlab = "Partei",
        ylab = "Anzahl der Nennungen")
legend("topleft", legend = rownames(haeufigkeiten203_matrix),
       fill = c("green4","grey","red3"), bty = "n", cex = 0.9)

#=========== Frage 204 ===============
#Barplot wie oft jede Partei genannt wird
haeufigkeiten204 <- sapply(parteinamen,
                           function(p) sum(grepl("1", daten204[[paste0("Antwort Erwähnung 1.",
                                                                       which(parteinamen==p))]])))
barplot(haeufigkeiten204, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen beim Thema Zuwanderung",
        xlab="Partei", ylab="Anzahl der Nennungen", col= parteifarben)

#Boxplot Ausführlichkeit nach Ländern für Frage 204
boxplot(
  Ausf ~ Land,
  data = daten204,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 204 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 204
boxplot(
  Ausf ~ KI,
  data = daten204,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 204 nach KI-Modell sortiert "
)

#Barplot mit Haltungen 

# Matrix erstellen: Zeilen = Haltung, Spalten = Partei
haeufigkeiten204_matrix <- sapply(1:8, function(i) {
  spalte <- paste0("Antwort Wertung 2.", i)
  werte <- daten204[[spalte]]
  
  # Zählen mit grepl, wie in deinem ursprünglichen Ansatz
  c(
    positiv = sum(grepl("1", werte), na.rm = TRUE),
    neutral  = sum(grepl("3", werte), na.rm = TRUE),
    negativ  = sum(grepl("2", werte), na.rm = TRUE)
  )
})

# Zeilen- und Spaltennamen setzen
colnames(haeufigkeiten204_matrix) <- parteinamen

# Gestapelter Barplot
barplot(haeufigkeiten204_matrix,
        beside = FALSE,
        col = c("green4","grey","red3"),
        main = "Parteiempflungen zum Thema Zuwanderung",
        xlab = "Partei",
        ylab = "Anzahl der Nennungen")
legend("topleft", legend = rownames(haeufigkeiten204_matrix),
       fill = c("green4","grey","red3"), bty = "n", cex = 0.9)

#=========== Frage 205 ===============
#Barplot wie oft jede Partei genannt wird
haeufigkeiten205 <- sapply(parteinamen,
                           function(p) sum(grepl("1", daten205[[paste0("Antwort Erwähnung 1.",
                                                                       which(parteinamen==p))]])))
barplot(haeufigkeiten205, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen beim Thema Lebensqualität",
        xlab="Partei", ylab="Anzahl der Nennungen", col= parteifarben)

#Boxplot Ausführlichkeit nach Ländern für Frage 205
boxplot(
  Ausf ~ Land,
  data = daten205,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 205 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 205
boxplot(
  Ausf ~ KI,
  data = daten205,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 205 nach KI-Modell sortiert "
)

#Barplot mit Haltungen 

#Barplot mit Haltungen 

# Matrix erstellen: Zeilen = Haltung, Spalten = Partei
haeufigkeiten205_matrix <- sapply(1:8, function(i) {
  spalte <- paste0("Antwort Wertung 2.", i)
  werte <- daten205[[spalte]]
  
  # Zählen mit grepl, wie in deinem ursprünglichen Ansatz
  c(
    positiv = sum(grepl("1", werte), na.rm = TRUE),
    neutral  = sum(grepl("3", werte), na.rm = TRUE),
    negativ  = sum(grepl("2", werte), na.rm = TRUE)
  )
})

# Zeilen- und Spaltennamen setzen
colnames(haeufigkeiten205_matrix) <- parteinamen

# Gestapelter Barplot
barplot(haeufigkeiten205_matrix,
        beside = FALSE,
        col =c("green4","grey","red3"),
        main = "Parteiempflungen zum Thema Lebensqualität",
        xlab = "Partei",
        ylab = "Anzahl der Nennungen")
legend("topleft", legend = rownames(haeufigkeiten205_matrix),
       fill = c("green4","grey","red3"), bty = "n", cex = 0.9)


#############Probieren Barplots der verschiedenen Themen zu stapeln:
haeufigkeiten_kombiniert <- rbind(
  # 1) ALLE POSITIVEN (unten)
  Wirtschaft_positiv      = haeufigkeiten202_matrix["positiv", ],
  Bildung_positiv         = haeufigkeiten203_matrix["positiv", ],
  Zuwanderung_positiv     = haeufigkeiten204_matrix["positiv", ],
  Lebensqualitaet_positiv = haeufigkeiten205_matrix["positiv", ],
   # 2) ALLE NEGATIVEN (mitte)
  Wirtschaft_negativ      = haeufigkeiten202_matrix["negativ", ],
  Bildung_negativ         = haeufigkeiten203_matrix["negativ", ],
  Zuwanderung_negativ     = haeufigkeiten204_matrix["negativ", ],
  Lebensqualitaet_negativ = haeufigkeiten205_matrix["negativ", ],
  # 3) ALLE NEUTRALEN (oben)
  Wirtschaft_neutral      = haeufigkeiten202_matrix["neutral", ],
  Bildung_neutral         = haeufigkeiten203_matrix["neutral", ],
  Zuwanderung_neutral     = haeufigkeiten204_matrix["neutral", ],
  Lebensqualitaet_neutral = haeufigkeiten205_matrix["neutral", ]
)

farben_stapel <- c(
  # positiv
  "#006400",  # Wirtschaft
  "#008000",  # Bildung
  "#00A000",  # Zuwanderung
  "#00C000",  # Lebensqualität
  
  # negativ
  "#8B0000",  # Wirtschaft
  "#B00000",  # Bildung
  "#D00000",  # Zuwanderung
  "#FF0000",  # Lebensqualität
  
  # neutral
  "#4F6D7A",  # Wirtschaft (blaugrau)
  "#6C8EA4",  # Bildung
  "#8FAFBF",  # Zuwanderung
  "#B0C4DE"   # Lebensqualität
)

# Ränder setzen: unten, links, oben, rechts
par(mar = c(5, 13, 4, 3))  # rechter Rand stark vergrößert
#Barplot erstellen 
barplot(
  haeufigkeiten_kombiniert,
  beside = FALSE,
  col = farben_stapel,
  main = "Parteinennungen nach Haltung und Thema",
  xlab = "Partei"
)
#Legende für Barplot
legend(
  "bottomleft",
  inset = c(-0.3, 0),
  legend = rownames(haeufigkeiten_kombiniert),
  fill = farben_stapel,
  bty = "n",
  cex = 0.78,
  xpd = TRUE
)
#------------------------------
dev.off() #Graphik-Fenster schließen (wichtig für PDF)