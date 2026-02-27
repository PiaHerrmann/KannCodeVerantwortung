print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
source("Gesamtauswertung.R")
library(reshape2)
library(ggplot2)


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
#-> H_0 wird abgelehnt: 
#--> Die Verteilung der Wenn‑Dann‑Konstruktionen
# unterscheidet sich signifikant zwischen den Parteien.

resid <- chi_test$stdres   # standardisierte Residuen pro Zelle
round(resid, 2)
resid_long <- melt(resid)
ggplot(resid_long,
       aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  labs(title="Abweichungen im Chi²-Test: Wenn-Dann-Konstruktionen nach Partei",
       x="Partei", y="Konstruktionstyp", fill="Abweichung") +
  theme_minimal(base_size=13) +
  theme(axis.text.x=element_text(angle=45,hjust=1))

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
# H_0 kann nicht abgelehnt werden

#ANOVA
anova_model <- aov(Ausf ~ KI + Land + Persona, data=daten)
summary(anova_model)

