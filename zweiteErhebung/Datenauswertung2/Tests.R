print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
source("Gesamtauswertung.R")
library(dplyr)
library(stringr)

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

# Permuationversion chi^2- Test
# H0 :Die Verteilung der Wenn‑Dann‑Konstruktionen ist unabhängig von der Partei.
set.seed(123)
chisq.test(tab, simulate.p.value=TRUE, B=50000)

# 1.99996e-05
#-> H_0 wird abgelehnt
#-> Die Verteilung der Wenn‑Dann‑Konstruktionen
# unterscheidet sich signifikant zwischen den Parteien.

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

