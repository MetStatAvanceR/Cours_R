#===============================================================================
#SOLUTION EXERCICE THÈME 4
#===============================================================================

#  Charger les packages
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(rstatix)


# Importer les données
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")
t4 = read.csv("Exercice_t4.csv", sep = ";", header = TRUE)

# Nettoyer les variables
t4$stade = as.factor(t4$stade)
t4$delai = as.factor(t4$delai)
t4$eval = as.numeric(gsub(",", ".", t4$eval))

# ==============================================================================
# 1- Statistiques descriptives et illustrations graphiques des moyennes 

# ----  Pour la variable Temps

#Statistiques descriptives
desc_temps = t4 %>%
  group_by(delai, stade) %>%
  summarise(Moyenne = mean(temps), N = n(), EcartType = sd(temps))
print(desc_temps)

# Représentation graphique
ggplot(t4, aes(x = delai, y = temps, color = stade, group = stade)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Temps estimé selon le type de délai et le stade",
       x = "Type de délai", y = "Temps (minutes)") +
  theme_minimal()

# ---- Pour la variable Eval

#Statistiques descriptives
desc_eval = t4 %>%
  group_by(delai, stade) %>%
  summarise(Moyenne = mean(eval), N = n(), EcartType = sd(eval))
print(desc_eval)

# Représentation graphique
ggplot(t4, aes(x = delai, y = eval, color = stade, group = stade)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Évaluation du service selon le type de délai et le stade",
       x = "Type de délai", y = "Évaluation (score standardisé)") +
  theme_minimal()



# ==============================================================================
# 2ANOVA à deux facteurs contrôlés


#---Pour la variable Temps

options(contrasts = c("contr.sum", "contr.poly"))
modele_temps = lm(temps ~ delai * stade, data = t4)
anova_temps = Anova(modele_temps, type = 3)
anova_temps



# Hypothèse d'interaction :
# H₀ : l'interaction entre les deux facteurs n'est pas significative
# H₁ : l'interaction entre les deux facteurs est significative

# L'interaction n'étant pas significative (p = 0.36), on analyse les effets globaux.


# ---- Analyse des effets globaux :


# Pour le facteur stade:

#Soit µᵢ  le temps marginal pour le stade #i. On veut tester
# H₀ : µ₁ = µ₂
# H₁ : µ₁ ≠ µ₂

#Le facteur « stade » est globalement significatif pour expliquer le temps (p-value=0.00).


emmeans_stade =emmeans(modele_temps, ~ stade)
pairwise_stade=pairs(emmeans_stade, adjust = "none")
summary(emmeans_stade)
summary(pairwise_stade, infer = TRUE)

#On estime que le temps moyen quand le stade d’avancement « près du but » est plus élevé 
#de 0.973 à 3.09 minutes que celui quand le stade d’avancement 
#est « loin du but », au niveau de confiance 95%.


# --- Pour le facteur délai

#Soit µᵢ  le temps marginal pour le stade #i. On veut tester
#H₀ : µ₁ = µ₂ = µ₃
#H₁ : au moins deux moyennes sont différentes.
#Le facteur « délai » est globalement significatif pour expliquer le temps
#(p-value=0.00).

emmeans_delai=emmeans(modele_temps, ~ delai)
pairwise_delai= pairs(emmeans_delai, adjust = "none")
summary(emmeans_delai)
summary(pairwise_delai, infer=TRUE)

#On observe une différence significative entre toutes les paires de moyennes.
#En effet, on estime, au niveau de confiance 95%, que :
#  - Le temps moyen quand le délai est « procédural » est plus élevé de 2.33 à 4.89 minutes 
#  que celui quand le délai est « correctionnel ».
#  - Le temps moyen quand le délai est « procédural » est plus élevé de 0.16 à 2.77 minutes 
#que celui quand le délai est « inconnu ».
#  - Le temps moyen quand le délai est « inconnu » est plus élevé de 0.85 à 3.45 minutes 
#que celui quand le délai est « correctionnel ».


# --- Pour la variable Eval

options(contrasts = c("contr.sum", "contr.poly"))
modele_eval = lm(eval ~ delai * stade, data = t4)
anova_eval = Anova(modele_eval, type = 3)
anova_eval



#Tester l’interaction entre les deux facteurs :
#  H₀ : l’interaction entre les deux facteurs n’est pas significative.
#  H₁ : l’interaction entre les deux facteurs est significative.

#L’interaction entre les deux facteurs est significative (p-value = 0.00). On doit donc évaluer les effets simples des facteurs.



#----Analyse des effets simples

#Le facteur « délai » comporte 3 niveaux et le facteur « stade » comporte 2 niveaux. 
#On va donc créer 6 groupes (2*3) en utilisant la syntaxe suivante:

t4$groupe = with(t4, ifelse(delai == 1 & stade == 1, 1,
                             ifelse(delai == 1 & stade == 2, 2,
                                    ifelse(delai == 2 & stade == 1, 3,
                                           ifelse(delai == 2 & stade == 2, 4,
                                                  ifelse(delai == 3 & stade == 1, 5,
                                                         ifelse(delai == 3 & stade == 2, 6, NA)))))))

t4$groupe = as.factor(t4$groupe)

modele_groupe = lm(eval ~ groupe, data = t4)
# Vérifier l'homogénéité des variances
leveneTest(modele_groupe)

# ANOVA sur groupe

aov(modele_groupe)

# Comparaisons multiples
emmeans_groupe=emmeans(modele_groupe, ~ groupe)
pairwise_groupe=pairs(emmeans_groupe, adjust = "none")
summary(emmeans_groupe)
summary(pairwise_groupe, infer=TRUE)

#Le test d’égalité des variances nous permet de supposer l’égalité des variances (p-value = 0.281), 
#on peut donc utiliser les comparaisons multiples LSD pour comparer les moyennes.

#Effets simples pour le facteur stade:
#Lorsque « délai = procédural » :

# H₀ : µ₁ = µ₂ vs H₁ : µ₁ ≠ µ₂
#Les moyennes sont significativement différentes (p-value=0.00). 
#Lorsque « délai = procédural », on estime que l’évaluation moyenne quand 
#le stade est « près du but » est moins élevée entre 0.46 et 1.06 que celle 
#quand le stade est « loin du but », au niveau de confiance 95%. 

#Lorsque « délai = correctionnel » :

# H₀ : µ₃ = µ₄ vs H₁ : µ₃ ≠ µ₄
#Les moyennes sont significativement différentes (p-value=0.00). 
#Lorsque « délai = correctionnel », on estime que l’évaluation moyenne 
#quand le stade est « près du but » est plus élevée entre 0.98 et 1.57 que celle 
#quand le stade est « loin du but », au niveau de confiance 95%. 

#Lorsque « délai = inconnu » :
# H₀ : µ₅ = µ₆ vs H₁ : µ₅ ≠ µ₆
#Les moyennes ne sont pas significativement différentes (p-value=0.126). 
#On estime que l’évaluation moyenne ne diffère pas selon le stade, quand le délai est « inconnu ».


# Effets simples pour le facteur délai :


#CUSTOM HYPOTHESIS 1: STADE = PRÈS DU BUT

# Filtrer les données pour enfant == 1
t4_stade1 = subset(t4, stade == 1)

# Ajuster un modèle linéaire
mod_e1 = lm(eval ~ delai, data = t4_stade1)

# Effectuer une ANOVA
anova_stade1 = aov(mod_e1)
summary(anova_stade1)


#CUSTOM HYPOTHESIS 2: STADE = LOIN DU BUT

# Filtrer les données pour stade == 2
t4_stade2 = subset(t4, stade == 2)
# Ajuster un modèle linéaire
mod_e2 = lm(eval ~ delai, data = t4_stade2)

#Effectuer une ANOVA
anova_stade2 = aov(mod_e2)
summary(anova_stade2)



#Lorsque « stade = près du but » :

# H₀ : µ₁ = µ₃ = µ₅ vs H₁ : au moins deux moyennes sont différente

#On rejette H₀, l’effet du facteur délai sur l’évaluation est significatif lorsque
#stade est égal à « près du but ». On se base sur les comparaisons multiples pour 
#obtenir les intervalles de confiance. On conclut que, au niveau de confiance 95% :

# -L’évaluation moyenne lorsque le délai est « procédural » est moins élevée de 
#0.63 à 1.22 que celle lorsque le délai est « correctionnel », et de 0.17 à 0.75 
#que celle lorsque le délai est « inconnu ». 

# -L’évaluation moyenne lorsque le délai est « correctionnel » est plus élevée de
#0.17 à 0.76 que celle lorsque le délai est « inconnu ».

# H₀ : µ₂ = µ₄ = µ₆ vs H₁ : au moins deux moyennes sont différentes
#On rejette H₀, l’effet du facteur délai sur l’évaluation est significatif lorsque 
#stade est égal à « loin du but ». On se base sur les comparaisons multiples pour 
#obtenir les intervalles de confiance. On conclut que, au niveau de confiance 95% :

# L’évaluation moyenne lorsque le délai est « procédural » est plus élevée de 
#0.81 à 1.41 que celle lorsque le délai est « correctionnel ».

#L’évaluation moyenne lorsque le délai est « correctionnel » est moins élevée de 
#0.74 à 1.35 que celle lorsque le délai est « inconnu ».

#Il n’y a pas de différence significative entre l’évaluation moyenne lorsque 
#le délai est « procédural » et celle lorsque le délai est « inconnu ».

#===============================================================================
#3- Recommandations d’affaires :

#- Réduire le temps d’attente lorsque le stade est « près du but ».

#- Réduire le temps d’attente lorsque le délai est « procédural ».

#- Quand le délai est « procédural », il faut améliorer l’expérience des participants 
#pour lesquels le stade est « près du but » afin d’améliorer le score d’évaluation.

#- Quand le délai est « correctionnel », il faut améliorer l’expérience des participants 
#pour lesquels le stade est « loin du but » afin d’améliorer le score d’évaluation.

#- Quand le stade est « près du but », essayer d’éviter le délai « procédural » et 
#« correctionnel » puisque cela réduit l’évaluation moyenne.

#- Quand le stade est « loin du but », essayer d’éviter le délai « correctionnel » car
#c’est celui qui donne l’évaluation moyenne la moins élevée.
