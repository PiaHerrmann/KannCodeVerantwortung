# Vorbereitung der Daten 
# Pakete laden
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(reshape2)
library(forcats)
library(scales)

print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
#Einlesen der Tabelle/ Daten
daten <- read_csv("Fragenauswertung - Parteiempfehlungen100.csv", skip =2) 

#Faktorisieren von character Variablen für Frage 
daten <- daten %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum & !Auffälliges ,
      as.factor
    )
  )

##############Verkürzung der Variablennamen 
# Ausprägungen Personas
levels(daten$Persona) <- c("Allgemein","Jan","Peter","Anna","Sabine","Lukas","Thomas","Mia")
# Erwähnungen
names(daten)[names(daten) == "Erwähnung 1.1 CDU"] <- "Erw_CDU"
names(daten)[names(daten) == "Erwähnung 1.2 SPD"] <- "Erw_SPD"
names(daten)[names(daten) == "Erwähnung 1.3 AFD"] <- "Erw_AFD"
names(daten)[names(daten) == "Erwähnung 1.4 Grüne"] <- "Erw_Gruene"
names(daten)[names(daten) == "Erwähnung 1.5 Linke"] <- "Erw_Linke"
names(daten)[names(daten) == "Erwähnung 1.6 FDP"] <- "Erw_FDP"
names(daten)[names(daten) == "Erwähnung 1.7 Freie Wähler"] <- "Erw_FW"
names(daten)[names(daten) == "Erwähnung 1.8 weitere"] <- "Erw_Weitere"
# Empfehlungen
names(daten)[names(daten) == "Empfehlung 2.1 CDU"] <- "Empf_CDU"
names(daten)[names(daten) == "Empfehlung 2.2 SPD"] <- "Empf_SPD"
names(daten)[names(daten) == "Empfehlung 2.3 AFD"] <- "Empf_AFD"
names(daten)[names(daten) == "Empfehlung 2.4 Grüne"] <- "Empf_Gruene"
names(daten)[names(daten) == "Empfehlung 2.5 Linke"] <- "Empf_Linke"
names(daten)[names(daten) == "Empfehlung 2.6 FDP"] <- "Empf_FDP"
names(daten)[names(daten) == "Empfehlung 2.7 Freie Wähler"] <- "Empf_FW"
names(daten)[names(daten) == "Empfehlung 2.8 weitere"] <- "Empf_Weitere"
names(daten)[names(daten) == "Empfehlung 2.9 Anzahl"] <- "Empf_Anzahl"
# Ausprägungen Empfehlung
levels(daten$Empf_Anzahl)= c("mehrere Parteien ",
                             "eine Partei",
                             "keine Partei",
                             "wenn-dann-Konstrukt",
                             "verweigert  Aussage",
                             "Antwort uneindeutig" )
tmp= c("empfohlen",
       "keine Einordnung",
       "nicht genannt",
       "wenn-dann-Konstrukt",
       "neutral genannt",
       "Antwort uneindeutig" )
levels(daten$Empf_CDU)=tmp
levels(daten$Empf_SPD)= c("empfohlen",
                          "keine Einordnung",
                          "nicht genannt",
                          "wenn-dann-Konstrukt",
                          "neutral genannt",
                          "kritisch eingeordnet",
                          "Antwort uneindeutig" )
levels(daten$Empf_AFD)=c("empfohlen",
                         "explizit nicht empfohlen",
                         "keine Einordnung",
                         "nicht genannt",
                         "wenn-dann-Konstrukt",
                         "neutral genannt",
                         "kritisch eingeordnet",
                         "Antwort uneindeutig" )
levels(daten$Empf_Gruene)= c("empfohlen",
                             "keine Einordnung",
                             "nicht genannt",
                             "wenn-dann-Konstrukt",
                             "neutral genannt",
                             "kritisch eingeordnet",
                             "Antwort uneindeutig" )
levels(daten$Empf_Linke)=tmp
levels(daten$Empf_FDP)=tmp
levels(daten$Empf_FW)=tmp

# Konstruktionen
names(daten)[names(daten) == "Konstruktion 3.1 CDU"] <- "K_CDU"
names(daten)[names(daten) == "Konstruktion 3.2 SPD"] <- "K_SPD"
names(daten)[names(daten) == "Konstruktion 3.3. AFD"] <- "K_AfD"
names(daten)[names(daten) == "Konstruktion 3.4 Grüne"] <- "K_Gruene"
names(daten)[names(daten) == "Konstruktion 3.5 Linke"] <- "K_Linke"
names(daten)[names(daten) == "Konstruktion 3.6 FDP"] <- "K_FDP"
names(daten)[names(daten) == "Konstruktion 3.7 Freie Wähler"] <- "K_FW"
names(daten)[names(daten) == "Konstruktion 3.8 weitere"] <- "K_Weitere"
#Nennungen
names(daten)[names(daten) == "Nennung 1"] <- "N1"
names(daten)[names(daten) == "Nennung 2"] <- "N2"
names(daten)[names(daten) == "Nennung 3"] <- "N3"
names(daten)[names(daten) == "Nennung 4"] <- "N4"
names(daten)[names(daten) == "Nennung 5"] <- "N5"
names(daten)[names(daten) == "Nennung 6"] <- "N6"
names(daten)[names(daten) == "Nennung 7"] <- "N7"
names(daten)[names(daten) == "Nennung 8"] <- "N8"
#Kontext
names(daten)[names(daten) == "Kontext 4.1"] <- "Kontext_1"
names(daten)[names(daten) == "Kontext 4.2"] <- "Kontext_2"
#Ausführlichkeit
names(daten)[names(daten) == "Ausführlichkeit  5.0 Gesamt"] <- "Ausf"
names(daten)[names(daten) == "Ausführlichkeit  5.1 CDU"] <- "Ausf_CDU"
names(daten)[names(daten) == "Ausführlichkeit  5.2 SPD"] <- "Ausf_SPD"
names(daten)[names(daten) == "Ausführlichkeit  5.3 AFD"] <- "Ausf_AFD"
names(daten)[names(daten) == "Ausführlichkeit  5.4 Grüne"] <- "Ausf_Gruene"
names(daten)[names(daten) == "Ausführlichkeit  5.5 Linke"] <- "Ausf_Linke"
names(daten)[names(daten) == "Ausführlichkeit  5.6 FDP"] <- "Ausf_FDP"
names(daten)[names(daten) == "Ausführlichkeit  5.7 Freie Wähler"] <- "Ausf_FW"
names(daten)[names(daten) == "Ausführlichkeit  5.8 Weitere"] <- "Ausf_Weiter"
#Weiteres
names(daten)[names(daten) == "KI-Modell"] <- "KI"

#Entfernen von leeren Zeilen wo weder Land noch KI angegeben sind
daten <- daten %>%
  filter(!(is.na(Land) & is.na(KI)))

#Entfernen von noch nicht codierten Zeilen
daten <- daten %>% filter(!(is.na(Codierung)))

#Frabzuordnung Partein
parteifarben <- c(
  "CDU"="grey20",        # CDU
  "SPD" ="#E3000F",          # SPD
  "AFD"= "#0489DB", # AfD  
  "Gruene"=  "#1AA037",    # Grüne
  "Linke"="#BE3075", #Linke
  "FDP"= "#FFEF00",         # FDP
  "FW"="#EF8108",       # Freie Wähler
  "sonstige"="grey60"        # Sonstige
)
#Extra Variable für Parteinnamen
parteinamen <- c("CDU","SPD","AfD","Grüne","Linke","FDP","Freie Wähler","Sonstige")