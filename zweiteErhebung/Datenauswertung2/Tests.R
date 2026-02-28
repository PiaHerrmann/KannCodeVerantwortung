print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
source("preprocessing.R")
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

#Daten vorbereiten
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
  
  # Typ-ID und Label trennen 
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



# chi^2- Test
# H0 :Die Verteilung der Wenn‑Dann‑Konstruktionen ist unabhängig von der Partei.
konstr_counts <- long_konstr %>% count(Partei, Typ_Label)
tab <- table(long_konstr$Partei, long_konstr$Typ_Label)
chi_test <- chisq.test(tab)
chi_test
# 
# 	Pearson's Chi-squared test
# 
# data:  tab
# X-squared = 437.26, df = 49, p-value < 2.2e-16
#-> H_0 wird abgelehnt

# Kategorien zusammenfassen
long_konstr_reduced <- long_konstr %>%
  mutate(Typ_Label = case_when(
    str_detect(Typ_Label, "uneindeutig") ~ "Sonstige",
    str_detect(Typ_Label, "verweigert") ~ "Sonstige",
    str_detect(Typ_Label, "anderer Kontext") ~ "Sonstige",
    str_detect(Typ_Label, "Verkehrspolitik") ~ "Sonstige",
    TRUE ~ Typ_Label                      # alle anderen bleiben gleich
  ))
tab_reduced <- table(long_konstr_reduced$Partei,
                     long_konstr_reduced$Typ_Label)

chi_test_reduced <- chisq.test(tab_reduced)
chi_test_reduced

# Erwartete Häufigkeiten sind zu klein

# Monte Carlo chi^2- Test
# H0 :Die Verteilung der Wenn‑Dann‑Konstruktionen ist unabhängig von der Partei.
set.seed(123)
chi_test=chisq.test(tab, simulate.p.value=TRUE, B=50000)
chi_test$p.value
# p-value = 1.99996e-05
#-> H_0 wird abgelehnt
#-> Die Verteilung der Wenn‑Dann‑Konstruktionen
# unterscheidet sich signifikant zwischen den Parteien.

resid <- chi_test$stdres
resid_long <- melt(resid)
ggplot(resid_long, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Standardisierte Residuen des Chi²-Tests",
       x="Partei", y="Wenn-Dann-Konstruktion", fill="Abweichung") +
  theme_minimal()

# testen nach Person
long_konstr_persona <- daten %>%
  select(Persona, starts_with("K_")) %>%
  pivot_longer(cols = starts_with("K_"),
               names_to = "Partei",
               values_to = "Konstruktion") %>%
  separate_rows(Konstruktion, sep = ",") %>%
  mutate(Konstruktion = str_trim(Konstruktion)) %>%
  separate(Konstruktion,
           into = c("Typ_ID", "Typ_Label"),
           sep = " - ",
           convert = TRUE,
           fill = "right") %>%
  filter(Typ_ID != 1) 
tab_persona <- table(long_konstr_persona$Persona, long_konstr_persona$Typ_Label)
chi_test_persona <- chisq.test(tab_persona)
chi_test_persona

# 	Pearson's Chi-squared test
# 
# data:  tab_persona
# X-squared = 241.88, df = 224, p-value = 0.1964

chisq.test(tab_persona, simulate.p.value=TRUE, B=50000)

# 	Pearson's Chi-squared test with
# 	simulated p-value (based on 50000
# 	replicates)
# 
# data:  tab_persona
# X-squared = 241.88, df = NA,
# p-value = 0.2338

