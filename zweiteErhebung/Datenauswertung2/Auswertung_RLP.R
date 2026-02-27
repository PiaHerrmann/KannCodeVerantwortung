print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
source("preprocessing.R")

pdf("KIPlotRLP.pdf", width = 12, height = 5, title = 'RLP')
par(mar=c(5,4,1.5,2),cex.lab=1.3,cex.axis=1.3)

daten <- subset(daten, Land== "Rheinland-Pfalz")

#Durchschnittliche/Median Ausführlichkeit pro Modell und Land
aggregate(Ausf ~ KI + Land, daten, mean)
aggregate(Ausf ~ KI + Land, daten, median)

# Häufigkeit Persona bezogene Antwort
# Daten vorbereiten
tab <- table(subset(daten, Persona != "1 - Allgemein", select = `nur Thema der Persona`))
rel <- round(100 * tab / sum(tab), 1)  # Prozentwerte berechnen

# Erzeuge Barplot und speichere Balkenpositionen
bp <- barplot(tab,
              col = "steelblue",
              names.arg = c("Nein", "Ja"),
              main = "Antwort bezieht sich ausschließlich auf Persona",
              ylim = c(0, max(tab) * 1.3))   # y-Achse größer skalieren
# Prozentwerte über Balken schreiben
text(x = bp, y = tab + max(tab)*0.05,
     labels = paste0(rel, "%"), cex = 1.2)


#Häufigkeit der Parteinennungen
erwaehnung_spalten <- grep("^Erw_", names(daten), value = TRUE)

haeufigkeiten <- sapply(erwaehnung_spalten, function(spalte) {
  sum(grepl("^1\\s*-", daten[[spalte]]), na.rm = TRUE)
})

bp <- barplot(haeufigkeiten,
              names.arg=parteinamen,
              ylim=c(0, max(haeufigkeiten)*1.15),    # etwas Platz oben schaffen
              main="Häufigkeit der Parteinennungen",
              xlab="Partei", ylab="Anzahl der Nennungen",
              col=parteifarben)
# absolute Häufigkeit über Balken
text(bp, haeufigkeiten, labels=haeufigkeiten, pos=3, cex=1.2)

# In relative Häufigkeiten umwandeln (Prozentwerte)
rel_haeufigkeiten <- haeufigkeiten / sum(haeufigkeiten) * 100

#Umfragewerte (in Prozent)
umfragewerte <- c(
  CDU = 29,
  SPD = 26,
  AfD = 18,
  Grüne = 10,
  Linke = 6,
  'Freie Wähler'= 4,
  Sonstige = 7
)

# Maximale Werte für Y-Achse bestimmen (inklusive Umfragewerte)
max_y <- max(c(rel_haeufigkeiten, umfragewerte)) * 1.25   # etwas Platz oben schaffen

# Barplot mit relativen Werten
bp2<- barplot(rel_haeufigkeiten,
              names.arg = parteinamen,
              ylim=c(0, max_y),
              main = "Relative Häufigkeit der Parteinennungen",
              xlab = "Partei",
              ylab = "Anteil an allen Nennungen (%)",
              col = parteifarben)

# Prozentwerte über Balken schreiben
text(bp2,
     rel_haeufigkeiten,
     labels = paste0(round(rel_haeufigkeiten, 1), "%"),
     pos = 3,
     cex = 1.2)


# Horizontale Linien für Umfragewerte einzeichnen
segments(x0=bp2 - par("cxy")[1], 
         x1=bp2 + par("cxy")[1], 
         y0=umfragewerte[parteinamen], 
         col="black", lwd=3)

# Legende hinzufügen
legend("topright",
       legend=c("Umfragewert (Stand 22.01.2026)"),
       col=c("black"),
       lty=c(1),
       lwd=c(4),
       bty="n",
       cex=1)

#Boxplot Ausführlichkeit der Partein
boxplot(
  daten[, c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
            "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
            "Ausf_FW", "Ausf_Weiter")],
  las=1,
  names = parteinamen,
  col = parteifarben,
  main = "Vergleich der Antwortlänge je nach Partei",
  ylab = "Wortanzahlder Antwort"
)

#Boxplot Ausführlichkeit nach Ländern 
boxplot(
  Ausf ~ Land,
  data = daten,
  xlab = "Land",
  ylab = "Wortanzahl",
  main= "Antwortlänge nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell 
boxplot(
  Ausf ~ KI,
  data = daten,
  xlab = "KI-Modell",
  ylab = "Wortanzahl",
  col="steelblue",
  main= "Antwortlänge nach KI-Modell sortiert "
)



# Mittelwerte der Ausführlichkeit je Partei und KI-Modell
ausfuerlichkeit_long <- daten %>%
  select(KI, starts_with("Ausf_")) %>%
  pivot_longer(cols = starts_with("Ausf_"),
               names_to = "Partei",
               values_to = "Ausf") %>%
  mutate(Partei = str_remove(Partei, "Ausf_"))

ggplot(ausfuerlichkeit_long, aes(x = Partei, y = Ausf, fill = KI)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Antwortlänge nach Partei und KI-Modell",
       x = "Partei", y = "Wortanzahl der Antwort") +
  theme_minimal(base_size=15)

# Nennung weitere Parteien
anmerkungen_words <- daten %>%
  filter(!is.na(Anmerkungen)) %>%
  unnest_tokens(word, Anmerkungen) %>%
  count(word, sort=TRUE)
head(anmerkungen_words)

# Konstruktionen – Wenn-dann-Analysen
long_konstr <- daten %>%
  select(starts_with("K_")) %>%
  rename_with(~ str_remove_all(., "^K_"), starts_with("K_")) %>%   # entfernt "K_"
  pivot_longer(
    cols = everything(),
    names_to = "Partei",
    values_to = "Konstruktion"
  ) %>%
  separate_rows(Konstruktion, sep = ",") %>%
  mutate(Konstruktion = str_trim(Konstruktion)) %>%
  
  # Typ-ID und Label trennen ("3 - SPD wird für Sozialpolitik empfohlen")
  separate(
    Konstruktion,
    into = c("Typ_ID", "Typ_Label"),
    sep = " - ",
    convert = TRUE,
    fill = "right"
  ) %>%
  
  # Kategorie 1 ("keine Wenn-dann-Konstruktion") ausschließen
  filter(Typ_ID != 1) %>%
  
  # Labels vereinheitlichen / kürzen
  mutate(Typ_Label = case_when(
    str_detect(Typ_Label, "in anderem Kontext") ~ "anderer Kontext",
    str_detect(Typ_Label, "Migration") ~ "Migrationspolitik",
    str_detect(Typ_Label, "Sozialpolitik") ~ "Sozialpolitik",
    str_detect(Typ_Label, "Umweltpolitik") ~ "Umweltpolitik",
    str_detect(Typ_Label, "Wirtschaftspolitik") ~ "Wirtschaftspolitik",
    str_detect(Typ_Label, "Verkehrspolitik") ~ "Verkehrspolitik",
    str_detect(Typ_Label, "(verweigert|ChatBot verweigert)") ~ "verweigert Aussage",
    TRUE ~ Typ_Label
  ))

# Häufigkeiten berechnen (für die ersten beiden Plots)
konstr_counts <- long_konstr %>% count(Partei, Typ_Label)

# Absolute Häufigkeiten pro Partei 
ggplot(konstr_counts,
       aes(x=reorder_within(Typ_Label,n,Partei), y=n)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  facet_wrap(~Partei, scales="free_y") +
  scale_x_reordered() +   
  labs(title="Wenn-dann-Konstruktionen nach Partei",
       x="Konstruktionstyp", y="Häufigkeit") +
  theme_minimal(base_size=13)

# Konstruktionen nach Typ sortiert und farblich nach Partei unterschiede
konstr_typ <- long_konstr %>%
  count(Partei, Typ_ID, Typ_Label)   
ggplot(konstr_typ,
       aes(x = Typ_Label, y = n, fill = Partei)) +
  geom_col(position='dodge',width=0.8) +
  scale_fill_brewer(palette='Set2') +
  labs(title='Vergleich der Konstruktionstypen nach Partei',
       x='Konstruktionstyp', y='Häufigkeit', fill='Partei') +
  theme_minimal(base_size=13) +
  theme(legend.position='bottom')

ggplot(konstr_typ,
       aes(x = Typ_Label, y = n, fill = Partei)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Wenn-dann-Konstruktionen nach Partei",
    x = "Themengebiet",
    y = "Häufigkeit",
    fill = "Partei"
  ) +
  scale_fill_manual(
    values = c(
      "CDU"   = "grey20",
      "SPD"   = "#E3000F",
      "AfD"   = "#0489DB",
      "Gruene"= "#1AA037",
      "Linke"= "#BE3075",
      "FDP"   ="#FFEF00",
      "FW"    ="#EF8108",
      "Weitere"="grey60"
    ),
    name="Partei"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Wenn-Dann nach KI getrennt
gemini <- subset(daten, KI== "Gemini FAST")
long_konstr_gemini <- gemini %>%
  select(starts_with("K_")) %>%
  rename_with(~ str_remove_all(., "^K_"), starts_with("K_")) %>%
  pivot_longer(cols = everything(),
               names_to = "Partei",
               values_to = "Konstruktion") %>%
  separate_rows(Konstruktion, sep = ",") %>%
  mutate(Konstruktion = str_trim(Konstruktion)) %>%
  separate(Konstruktion,
           into = c("Typ_ID", "Typ_Label"),
           sep = " - ",
           convert = TRUE,
           fill = "right") %>%
  filter(Typ_ID != 1) %>%  # Kategorie 1 ausschließen
  mutate(Typ_Label = case_when(
    str_detect(Typ_Label, "in anderem Kontext") ~ "anderer Kontext",
    str_detect(Typ_Label, "Migration") ~ "Migrationspolitik",
    str_detect(Typ_Label, "Sozialpolitik") ~ "Sozialpolitik",
    str_detect(Typ_Label, "Umweltpolitik") ~ "Umweltpolitik",
    str_detect(Typ_Label, "Wirtschaftspolitik") ~ "Wirtschaftspolitik",
    str_detect(Typ_Label, "Verkehrspolitik") ~ "Verkehrspolitik",
    str_detect(Typ_Label, "(verweigert|ChatBot verweigert)") ~ "verweigert Aussage",
    TRUE ~ Typ_Label
  ))
konstr_typ_gemini <- long_konstr_gemini %>%
  count(Partei, Typ_ID, Typ_Label)

ggplot(konstr_typ_gemini,
       aes(x = Typ_Label, y = n, fill = Partei)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Wenn-dann-Konstruktionen nach Partei (Gemini FAST)",
    x = "Themengebiet",
    y = "Häufigkeit",
    fill = "Partei"
  ) +
  scale_fill_manual(
    values = c(
      "CDU"   = "grey20",
      "SPD"   = "#E3000F",
      "AfD"   = "#0489DB",
      "Gruene"= "#1AA037",
      "Linke"= "#BE3075",
      "FDP"   ="#FFEF00",
      "FW"    ="#EF8108",
      "Weitere"="grey60"
    ),
    name="Partei"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

chatGPT <- subset(daten, KI== "Chat-GPT 5.2")
long_konstr_chatGPT <- chatGPT %>%
  select(starts_with("K_")) %>%
  rename_with(~ str_remove_all(., "^K_"), starts_with("K_")) %>%
  pivot_longer(cols = everything(),
               names_to = "Partei",
               values_to = "Konstruktion") %>%
  separate_rows(Konstruktion, sep = ",") %>%
  mutate(Konstruktion = str_trim(Konstruktion)) %>%
  separate(Konstruktion,
           into = c("Typ_ID", "Typ_Label"),
           sep = " - ",
           convert = TRUE,
           fill = "right") %>%
  filter(Typ_ID != 1) %>%  # Kategorie 1 ausschließen
  mutate(Typ_Label = case_when(
    str_detect(Typ_Label, "in anderem Kontext") ~ "anderer Kontext",
    str_detect(Typ_Label, "Migration") ~ "Migrationspolitik",
    str_detect(Typ_Label, "Sozialpolitik") ~ "Sozialpolitik",
    str_detect(Typ_Label, "Umweltpolitik") ~ "Umweltpolitik",
    str_detect(Typ_Label, "Wirtschaftspolitik") ~ "Wirtschaftspolitik",
    str_detect(Typ_Label, "Verkehrspolitik") ~ "Verkehrspolitik",
    str_detect(Typ_Label, "(verweigert|ChatBot verweigert)") ~ "verweigert Aussage",
    TRUE ~ Typ_Label
  ))
konstr_typ_chatGPT <- long_konstr_chatGPT %>%
  count(Partei, Typ_ID, Typ_Label)

ggplot(konstr_typ_chatGPT,
       aes(x = Typ_Label, y = n, fill = Partei)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Wenn-dann-Konstruktionen nach Partei (chatGPT 5.2)",
    x = "Themengebiet",
    y = "Häufigkeit",
    fill = "Partei"
  ) +
  scale_fill_manual(
    values = c(
      "CDU"   = "grey20",
      "SPD"   = "#E3000F",
      "AfD"   = "#0489DB",
      "Gruene"= "#1AA037",
      "Linke"= "#BE3075",
      "FDP"   ="#FFEF00",
      "FW"    ="#EF8108",
      "Weitere"="grey60"
    ),
    name="Partei"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Partei Positionierung
df_long <- daten %>%
  select(N1:N8) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = N1:N8,
               names_to = "Position",
               values_to = "Partei") %>%
  mutate(Position_num = as.numeric(sub("N", "", Position)))

ungültige_parteien <- c(
  "es wird keine weitere Partei genannt",
  "50 - ChatBot verweigert die Aussage",
  "99 - Antwort uneindeutig"
)

partei_position <- df_long %>%
  filter(!Partei %in% ungültige_parteien) %>%
  group_by(Partei) %>%
  summarise(Durchschnitt_Position = mean(Position_num),
            SD_Position = sd(Position_num),
            n_Nennungen = n())

# Punktdiagramm mit umgedrehter Y-Achse (Platz 1 oben)
ggplot(partei_position,
       aes(x=reorder(Partei, Durchschnitt_Position), y=Durchschnitt_Position)) +
  geom_point(size=4, color="steelblue") +
  geom_errorbar(aes(ymin=Durchschnitt_Position-SD_Position,
                    ymax=Durchschnitt_Position+SD_Position),
                width=.3, color="grey40") +
  coord_flip() +                                 # horizontale Darstellung
  scale_y_reverse(breaks=1:8) +                  # Y-Achse umdrehen → Platz 1 oben
  labs(title="Durchschnittliche Positionierung der Parteien in KI-Antworten",
       subtitle="Je niedriger der Wert, desto früher wird die Partei genannt",
       x="Partei", y="Mittlere Rangposition (1–8)") +
  theme_minimal(base_size=13)


# Gestapeltes Säulendiagramm
df_long2 <- daten %>%
  select(N1:N8) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = N1:N8,
               names_to = "Position",
               values_to = "Partei") %>%
  mutate(Position_num = as.numeric(sub("N", "", Position)))


df_long_clean2 <- df_long2 %>%
  filter(!Partei %in% ungültige_parteien)

position_counts <- df_long_clean2 %>%
  group_by(Position_num, Partei) %>%
  summarise(Haeufigkeit = n(), .groups = "drop")

ggplot(position_counts,
       aes(x=factor(Position_num), y=Haeufigkeit, fill=Partei)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "CDU"   = "grey20",
      "SPD"   = "#E3000F",
      "AfD"   = "#0489DB",
      "Grüne"= "#1AA037",
      "Linke"= "#BE3075",
      "FDP"   ="#FFEF00",
      "Freie Wähler"="#EF8108",       
      "Weitere Parteien"="grey60"  
    ),
    name="Partei")  +
  labs(title="Verteilung der Parteinennungen nach Rangposition",
       x="Rangposition in Antwort (1–8)",
       y="Anzahl der Nennungen") +
  theme_minimal(base_size=13)

#Persona
# Boxplot der Gesamt-Ausführlichkeit pro Persona
ggplot(daten, aes(x = Persona, y = Ausf, fill = Persona)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Vergleich der Antwortlängen zwischen Personas",
    x = "Persona", 
    y = "Wortanzahl"
  ) +
  scale_x_discrete(
    labels = c("Allgemein", "Jan", "Peter", "Anna",
               "Sabine", "Lukas", "Thomas", "Mia")
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Partei-Scores im Vergleich zwischen Personas

points_vec <- c(8,7,6,5,4,3,2,1)

df_long <- daten %>%
  select(Persona, N1:N8) %>%
  mutate(id=row_number()) %>%
  pivot_longer(cols=N1:N8,
               names_to="Position",
               values_to="Partei") %>%
  mutate(Punkte=points_vec[as.numeric(sub("N","",Position))])

ungültige_parteien <- c(
  "es wird keine weitere Partei genannt",
  "50 - ChatBot verweigert die Aussage",
  "99 - Antwort uneindeutig"
)

partei_scores_persona <- df_long %>%
  filter(!Partei %in% ungültige_parteien) %>%
  group_by(Persona, Partei) %>%
  summarise(Score=sum(Punkte)) %>%
  ungroup()
ggplot(partei_scores_persona,
       aes(x=reorder(Partei,-Score), y=Score, fill=Persona)) +
  geom_col(position="dodge") +
  scale_fill_brewer(
    palette="Set2",
    name="Persona",
    labels=c("Allgemein","Jan","Peter","Anna",
             "Sabine","Lukas","Thomas","Mia")
  ) +
  labs(title="Vergleich der Partei-Scores zwischen Personas",
       x="Partei", y="Gesamt-Score") +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(size=10)
  )

ggplot(partei_scores_persona,
       aes(x = reorder(Persona, -Score), y = Score, fill = Partei)) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(
    values = c(
      "CDU"   = "grey20",
      "SPD"   = "#E3000F",
      "AfD"   = "#0489DB",
      "Grüne"= "#1AA037",
      "Linke"= "#BE3075",
      "FDP"   ="#FFEF00",
      "Freie Wähler"    ="#EF8108",
      "Weitere Parteien"="grey60"
    ),
    name="Partei"
  ) +
  labs(title="Vergleich der Scores pro Partei innerhalb der Personas",
       x="Persona", y="Gesamt-Score") +
  theme_minimal() +
  theme(
    legend.position="bottom",
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(size=10),
    axis.text.x=element_text(angle=45,hjust=1)
  )



# Analyse als Funktion, um verschiedene Datensätze (von Personas) zu analysieren
analyse_partei_daten <- function(df, person_name = "Gesamt"){
  
  #-------------------------------
  # Vorbereitung: Farben & Parteinamen
  parteifarben <- c(
    "grey20", "red", "deepskyblue3", "#64A12D",
    "#BE3075", "gold", "orange", "grey60"
  )
  parteinamen <- c("CDU","SPD","AfD","Grüne","Linke","FDP","Freie Wähler","Sonstige")
  
  #-------------------------------
  # Basisstatistiken
  
  cat("\n📊 Durchschnittliche Ausführlichkeit pro KI und Land:\n")
  print(aggregate(Ausf ~ KI + Land, data = df, FUN = mean))
  
  cat("\n📈 Median-Ausführlichkeit pro KI und Land:\n")
  print(aggregate(Ausf ~ KI + Land, data = df, FUN = median))
  
  
  #-------------------------------
  # Häufigkeit der Erwähnungen
  
  erwaehnung_spalten <- grep("^Erw_", names(df), value = TRUE)
  
  haeufigkeiten <- sapply(erwaehnung_spalten, function(spalte) {
    sum(grepl("^1\\s*-", df[[spalte]]), na.rm = TRUE)
  })
  
  
  p_bar_erw <- barplot(
    haeufigkeiten,
    names.arg=parteinamen,
    ylim=c(0, max(haeufigkeiten)*1.15),    # etwas Platz oben schaffen
    main= paste("Häufigkeit der Parteinennungen","(",person_name,")"),
    xlab="Partei",
    ylab="Anzahl der Nennungen",
    col=parteifarben
  )
  
  # absolute Häufigkeit über Balken
  text( p_bar_erw, haeufigkeiten, labels=haeufigkeiten, pos=3, cex=1.2)
  #-------------------------------
  # Boxplots zur Ausführlichkeit
  
  boxplot(
    df[, c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
           "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
           "Ausf_FW", "Ausf_Weiter")],
    las=1,
    names = parteinamen,
    col = parteifarben,
    main = paste("Vergleich der Antwortlänge je nach Partei","(",person_name,")"),
    ylab = "Wortanzahl der Antwort"
  )
  
  
  p_box_land <- ggplot(df, aes(x=Land, y=Ausf)) +
    geom_boxplot(fill="steelblue") +
    labs(title= paste("Antwortlänge nach Land sortiert","(",person_name,")"),
         x="Land", y="Wortanzahl") +
    theme_minimal()
  
  print(p_box_land)
  
  
  p_box_ki <- ggplot(df, aes(x=KI, y=Ausf)) +
    geom_boxplot(fill="steelblue") +
    labs(title= paste("Antwortlänge nach KI-Modell sortiert","(",person_name,")"),
         x="KI-Modell", y="Wortanzahl") +
    theme_minimal()
  
  print(p_box_ki)
  
  
  #-------------------------------
  # Partei-Scores (gewichtete Nennungen nach Position)
  
  points_vec <- c(8,7,6,5,4,3,2,1)   # Punkte für Positionen N1–N8
  
  df_long <- df %>%
    select(N1:N8) %>%
    mutate(id=row_number()) %>%
    pivot_longer(cols=N1:N8,names_to="Position",values_to="Partei") %>%
    mutate(Punkte=points_vec[as.numeric(sub("N","",Position))])
  
  ungültige_parteien <- c(
    "es wird keine weitere Partei genannt",
    "50 - ChatBot verweigert die Aussage",
    "99 - Antwort uneindeutig"
  )
  
  partei_scores <- df_long %>%
    filter(!Partei %in% ungültige_parteien) %>%
    group_by(Partei) %>%
    summarise(Score=sum(Punkte)) %>%
    arrange(desc(Score))
  
  p_score <- ggplot(partei_scores,aes(x=reorder(Partei,Score),y=Score))+
    geom_col(fill="steelblue")+
    coord_flip()+
    labs(title=paste("Partei-Scores (gewichtete Nennungen)","(",person_name,")"),
         x="Partei",y="Score")+
    theme_minimal()
  
  print(p_score)
  
  library(ggplot2)
  library(dplyr)
  # Daten in langes Format umwandeln
  points_vec <- 8:1
  df_long <- daten %>%
    select(N1:N8) %>%   # nur N1 bis N8
    mutate(id = row_number()) %>%
    pivot_longer(cols = N1:N8, names_to = "Position", values_to = "Partei") %>%
    mutate(Position_num = as.numeric(sub("N", "", Position)))# Liste der unerwünschten Antworten
  ungültige_parteien <- c(
    "es wird keine weitere Partei genannt",
    "50 - ChatBot verweigert die Aussage",
    "99 - Antwort uneindeutig"
  )
  
  partei_position <- df_long %>%
    filter(!Partei %in% ungültige_parteien) %>%
    group_by(Partei) %>%
    summarise(Durchschnitt_Position = mean(Position_num),
              SD_Position = sd(Position_num),
              n_Nennungen = n())
  
  ggplot(partei_position,
         aes(y=reorder(Partei, Durchschnitt_Position), x=Durchschnitt_Position)) +
    geom_point(size=4, color="steelblue") +
    geom_errorbar(aes(xmin=Durchschnitt_Position-SD_Position,
                      xmax=Durchschnitt_Position+SD_Position),
                  width=.3, color="grey40") +
    
    scale_x_reverse(breaks=1:8) +   
    
    labs(title="Durchschnittliche Positionierung der Parteien in KI-Antworten",
         subtitle="Je niedriger der Wert, desto früher wird die Partei genannt",
         y="Partei", x="Mittlere Rangposition (1–8)") +
    
    theme_minimal(base_size=13)
  #-------------------------------
  # Durchschnittliche Antwortlänge pro Partei und KI
  
  ausf_long <- df %>%
    select(KI, starts_with("Ausf_")) %>%
    pivot_longer(cols=starts_with("Ausf_"),
                 names_to="Partei",
                 values_to="Ausf") %>%
    mutate(Partei=str_remove(Partei,"Ausf_"))
  
  p_ausflang <- ggplot(ausf_long,aes(x=Partei,y=Ausf,fill=KI))+
    geom_boxplot()+
    scale_fill_brewer(palette="Set2")+
    labs(title=paste("Antwortlänge nach Partei und KI-Modell","(",person_name,")"),
         x="Partei",y="Wortanzahl")+
    theme_minimal()
  
  print(p_ausflang)
  
  #--------------------------------------------
  #-------------------------------
  
  cat("\n✅ Analyse abgeschlossen!\n")
}

# Allgemein
analyse_partei_daten(daten[daten$Persona == "Allgemein",],"Allgemein")

#Jan
analyse_partei_daten(daten[daten$Persona == "Jan",],"Jan")

#Peter
analyse_partei_daten(daten[daten$Persona == "Peter",],"Peter")

#Anna
analyse_partei_daten(daten[daten$Persona == "Anna",],"Anna")

#Sabine
analyse_partei_daten(daten[daten$Persona == "Sabine",],"Sabine")

# Lukas
analyse_partei_daten(daten[daten$Persona == "Lukas",],"Lukas")

# Thomas
analyse_partei_daten(daten[daten$Persona == "Thomas",],"Thomas")

#Mia
analyse_partei_daten(daten[daten$Persona == "Mia",],"Mia")


#Analyse Antwortlänge pro Partei nach Zeilendurchschnitt

# Ausführlichkeits Spalten rausfiltern
parteien_spalten_ausf <- grep("^Ausf_", names(daten), value = TRUE)

# 1. Mittelwert pro Zeile über die Partei-Spalten berechnen
daten$Mittelwert_Zeile <- rowMeans(daten[, parteien_spalten_ausf], na.rm = TRUE)

# Abweichung jeder Partei vom Zeilenmittelwert berechnen
#erzeugen eines neuen Dataframe dafür
abweichungen <- daten[, parteien_spalten_ausf] - daten$Mittelwert_Zeile

# Optional: Mittelwert der Abweichung pro Partei (sollte nahe 0 sein)
mean_abweichung <- colMeans(abweichungen, na.rm = TRUE)

# Optional: Standardabweichung pro Partei 
sd_abweichung <- apply(abweichungen, 2, sd, na.rm = TRUE)

# Ergebnisse anzeigen
mean_abweichung
sd_abweichung

# Schritt 1: Zeilen-ID hinzufügen
abweichungen$Zeile <- 1:nrow(abweichungen)

# Schritt 2: In long-Format bringen
abweichungen_long <- melt(abweichungen, id.vars = "Zeile", variable.name = "Partei", value.name = "Abweichung")

# Schritt 3: Heatmap erstellen
ggplot(abweichungen_long, aes(x = Partei, y = Zeile, fill = Abweichung)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "azure3", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Heatmap Abweichungen der Parteien vom Zeilenmittelwert",
       x = "Partei", y = "Zeile", fill = "Abweichung")

# Boxplot
ggplot(abweichungen_long, aes(x = Partei, y = Abweichung, fill = Partei)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot Verteilung der Abweichungen pro Partei",
       y = "Abweichung vom Zeilenmittelwert") +
  theme(legend.position = "none")

#Violinplot
ggplot(abweichungen_long, aes(x = Partei, y = Abweichung, fill = Partei)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +  # Box innerhalb der Violine
  theme_minimal() +
  labs(title = "Violinplot Verteilung der Abweichungen pro Partei",
       y = "Abweichung vom Zeilenmittelwert") +
  theme(legend.position = "none")


################### Empfehlungs Analyse

# Erste Übersicht: Verteilung der Anzahl Empfehlungen
ggplot(daten, aes(x = fct_infreq(Empf_Anzahl))) +
  geom_bar(fill = "steelblue",width=.7) +
  labs(title = "Verteilung der Anzahl von Empfehlungen",
       x = "Kategorie der Empfehlung",
       y = "Anzahl Antworten") +
  theme_minimal(base_size = 14)

mosaicplot(~ KI + Empf_Anzahl,
           data=daten,
           color=TRUE,
           main="Zusammenhang zwischen Modell und Anzahl der Empfehlungen")


empf_KI <- daten %>%
  group_by(KI, Empf_Anzahl) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona <- daten %>%
  group_by(Persona, Empf_Anzahl) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_filter <- daten %>%
  group_by(Persona, Empf_Anzahl) %>%
  filter(!grepl("3", Empf_Anzahl)) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_CDU <- daten %>%
  group_by(Persona, Empf_CDU) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_SPD <- daten %>%
  group_by(Persona, Empf_SPD) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_AFD <- daten %>%
  group_by(Persona, Empf_AFD) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_Gruene <- daten %>%
  group_by(Persona, Empf_Gruene) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_Linke <- daten %>%
  group_by(Persona, Empf_Linke) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_FDP <- daten %>%
  group_by(Persona, Empf_FDP) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_FW <- daten %>%
  group_by(Persona, Empf_FW) %>%
  summarise(Anzahl = n(),.groups = "drop")

empf_Persona_Weitere <- daten %>%
  group_by(Persona, Empf_Weitere) %>%
  summarise(Anzahl = n(),.groups = "drop")

ggplot(empf_Persona,
       aes(x=Persona,y=Anzahl,fill=Empf_Anzahl))+
  geom_col(position=position_dodge2(),width=.8)+
  scale_fill_brewer(palette="Set2", name="Empfehlungskategorie")+
  labs(title="Verteilung der Empfehlungskategorien nach Persona",
       x="Persona",y="Häufigkeit")+
  theme_minimal(base_size=13)+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position='bottom')

# Partei-spezifische Empfehlungen je Persona - CDU
ggplot(empf_Persona_CDU,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_CDU)) +
  geom_col(width=.6) +      
  labs(title="CDU-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - SPD
ggplot(empf_Persona_SPD,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_SPD)) +
  geom_col(width=.6) +     
  labs(title="SPD-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - AFD
ggplot(empf_Persona_AFD,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_AFD)) +
  geom_col(width=.6) +  
  labs(title="Afd-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - Grüne
ggplot(empf_Persona_Gruene,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_Gruene)) +
  geom_col(width=.6) +    
  labs(title="Grüne-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - Linke
ggplot(empf_Persona_Linke,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_Linke)) +
  geom_col(width=.6) +    
  labs(title="Linke-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - Freie Wähler
ggplot(empf_Persona_FW,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_FW)) +
  geom_col(width=.6) +     
  labs(title="FW-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - FDP
ggplot(empf_Persona_FDP,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_FDP)) +
  geom_col(width=.6) +   
  labs(title="FDP-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Partei-spezifische Empfehlungen je Persona - Weitere
ggplot(empf_Persona_Weitere,
       aes(x=fct_reorder(Persona, Anzahl, sum), y=Anzahl,
           fill = Empf_Weitere)) +
  geom_col(width=.6) +      
  labs(title="Weitere-Empfehlungen nach Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#gemeinsamer Plot

# Liste aller Datensätze
liste_empf <- list(
  CDU      = empf_Persona_CDU,
  SPD      = empf_Persona_SPD,
  AfD      = empf_Persona_AFD,
  Gruene   = empf_Persona_Gruene,
  Linke    = empf_Persona_Linke,
  FDP      = empf_Persona_FDP,
  FW       = empf_Persona_FW,
  Weitere  = empf_Persona_Weitere
)

# Zusammenführen mit Parteispalte
empf_persona_all <- bind_rows(lapply(names(liste_empf), function(p){
  tmp <- liste_empf[[p]]
  tmp$Partei <- p
  return(tmp)
}))

ggplot(empf_persona_all,
       aes(x=Persona, y=Anzahl, fill=Partei)) +
  geom_col(position=position_dodge(width=.8), width=.7) +
  # scale_fill_manual(values=c(
  #   "CDU"="grey20",
  #   "SPD"="#E3000F",
  #   "AfD"="deepskyblue3",
  #   "Gruene"="#64A12D",
  #   "Linke"="#BE3075",
  #   "FDP"="gold",
  #   "Weitere"="grey60"
  # ), name="Partei") +
  scale_fill_manual(
    values = c(
      "CDU"   = "grey20",
      "SPD"   = "#E3000F",
      "AfD"   = "#0489DB",
      "Gruene"= "#1AA037",
      "Linke"= "#BE3075",
      "FDP"   ="#FFEF00",
      "FW"    ="#EF8108",
      "Weitere"="grey60"
    ),name="Partei") +
  labs(title="Vergleich der Empfehlungen nach Persona und Partei",
       subtitle="Alle Parteien im direkten Vergleich pro Persona",
       x="Persona", y="Häufigkeit") +
  theme_minimal(base_size=13) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position='bottom',
        legend.title=element_text(face='bold'))

#Grafik zur Verteilung der Empfehlungskategorien - SPD
ggplot(daten, aes(x = Empf_SPD)) +
  geom_bar(fill = "steelblue", width=.7) +
  labs(title="SPD – Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Grafik zur Verteilung der Empfehlungskategorien - Linke
ggplot(daten, aes(x = Empf_Linke)) +
  geom_bar(fill = "steelblue", width=.7) +
  labs(title="Linke – Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Grafik zur Verteilung der Empfehlungskategorien - AFD
ggplot(daten, aes(x = Empf_AFD)) +
  geom_bar(fill="steelblue", width=.7) +
  labs(title="AfD – Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Grafik zur Verteilung der Empfehlungskategorien - Gruene
ggplot(daten, aes(x = Empf_Gruene)) +
  geom_bar(fill="steelblue", width=.7) +
  labs(title="Grüne– Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Grafik zur Verteilung der Empfehlungskategorien - CDU
ggplot(daten, aes(x = Empf_CDU) )+
  geom_bar(fill="steelblue", width=.7) +
  labs(title="CDU– Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x= element_text(angle = 45, hjust = 1))

ggplot(daten, aes(x = Empf_FDP)) +
  geom_bar(fill="steelblue", width=.7) +
  labs(title="FDP– Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Grafik zur Verteilung der Empfehlungskategorien - Freie Wähler
ggplot(daten, aes(x = Empf_FW)) +
  geom_bar(fill="steelblue", width=.7) +
  labs(title="Freihe Wähler– Verteilung der Empfehlungskategorien",
       x="Kategorie der Empfehlung", y="Anzahl Antworten") +
  theme_minimal(base_size=14)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####Antwortlänge AFD im Vergleich zur Einodnung
aggregate(Ausf_AFD ~ Empf_AFD,
          data = daten, 
          FUN = function(x) c(n = length(x), Mittelwert = mean(x)))


afd_summary= daten %>%
  group_by(Empf_AFD) %>%
  summarise(
    n = n(),
    Mittelwert_Antwortlaenge = mean(Ausf_AFD, na.rm = TRUE)
  )

afd_summary
ggplot(afd_summary,
       aes(x = Empf_AFD, y = Mittelwert_Antwortlaenge)) +
  geom_col(fill = "deepskyblue3", width=.7) +
  labs(title = "Antwortlänge zur AfD im Vergleich zur Einordnung",
       x     = "Einordnung der AfD",
       y     = "Durchschnittliche Antwortlänge") +
  theme_minimal(base_size=13) +
  theme(axis.text.x=element_text(angle=45,hjust=1))




# Nur Zeilen betrachten, in denen AfD überhaupt erwähnt oder bewertet wurde
afd_einordnung <- daten %>%
  filter(!is.na(Empf_AFD) & Empf_AFD != "nicht genannt")

# Häufigkeit jeder Einordnung berechnen
afd_summary <- afd_einordnung %>%
  group_by(Empf_AFD) %>%
  summarise(
    n = n(),                                       # Anzahl der Nennungen
    Anteil = n() / nrow(afd_einordnung) * 100      # relativer Anteil (%)
  ) %>%
  arrange(desc(n))                                 # nach Häufigkeit sortieren

# Ausgabe der Übersichtstabelle
afd_summary

# Visualisierung: Wie oft wird die AfD wie eingeordnet?
ggplot(afd_summary,
       aes(x = reorder(Empf_AFD, -n), y = n)) +
  geom_col(fill = "deepskyblue3", width = .7) +
  geom_text(aes(label = paste0(round(Anteil,1), "%")),
            vjust = -0.5, size = 4) +
  labs(title = "Einordnungen der AfD bei Nennung",
       subtitle = "Nur Antworten berücksichtigt, in denen die AfD tatsächlich erwähnt wurde",
       x = "Einordnungskategorie",
       y = "Anzahl der Nennungen") +
  theme_minimal(base_size=13) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))   # mehr Platz oben

#-----------------
dev.off()

