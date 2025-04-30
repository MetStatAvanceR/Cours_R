#===============================================================================
# Mise en situation thème 4
#===============================================================================

# Packages requis
library(dplyr)
library(emmeans)
library(ggplot2)
library(agricolae)  

# Importer les données
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")
conso = read.csv("Consommation.csv", sep = ";", header = TRUE)

conso$conduite = as.factor(conso$conduite)
conso$marque = as.factor(conso$marque)
conso$Cons = gsub(",", ".", conso$Cons)    
conso$Cons = as.numeric(conso$Cons)

#===============================================================================
# Analysez, selon ce qui est pertinent, les effets globaux ou les effets simples 
# des facteurs conduite et marque sur la consommation en carburant.

# Statistiques descriptives
desc_conso = conso %>%
  group_by(conduite, marque) %>%
  summarise(Moyenne = mean(Cons), N = n(), EcartType = sd(Cons))
print(desc_conso)

# Créer le graphique des moyennes marginales par conduite
ggplot(desc_conso, aes(x = conduite, y = Moyenne, group = marque, color = factor(marque))) +
  geom_point(size = 3) +   
  geom_line(size = 1) +    
  labs(
    title = "Moyenne marginale estimée de la variable Cons",
    x = "Conduite",
    y = "Moyenne marginale estimée",
    color = "Marque"
  ) +
  theme_minimal()

# Créer le graphique des moyennes marginales par marque
ggplot(desc_conso, aes(x = marque, y = Moyenne, group = conduite, color = factor(conduite))) +
  geom_point(size = 3) +   
  geom_line(size = 1) +    
  labs(
    title = "Moyenne marginale estimée de la variable Cons",
    x = "Marque",
    y = "Moyenne marginale estimée",
    color = "Conduite"
  ) +
  theme_minimal()

# On observe que les tailles d’échantillon par cellule sont déséquilibrées
# Les graphiques d'interaction suggèrent une interaction potentielle entre 
# la marque et le type de conduite : la pente des lignes varie selon les niveaux 
# de l’autre facteur, en particulier pour la marque 3 qui montre une forte hausse 
# de la consommation en conduite 2.

#==============================================================================
# Interaction entre les variables Marque et Conduite
anova_model = aov(Cons ~ marque * conduite, data = conso)
summary(anova_model)

# H0 : Il n'y a pas d'interaction entre les deux facteurs
# H1 : Il y a une interaction entre les deux facteurs
# p-value < 0, on rejette H0 → il y a une interaction entre la marque et la conduite

# On va donc maintenant s'intéresser aux effets simples des facteurs marque et conduite

# Créer une variable groupe pour représenter les combinaisons conduite x marque
conso$groupe = with(conso, ifelse(conduite == 1 & marque == 1, 1,
                                   ifelse(conduite == 1 & marque == 2, 2,
                                          ifelse(conduite == 1 & marque == 3, 3,
                                                 ifelse(conduite == 2 & marque == 1, 4,
                                                        ifelse(conduite == 2 & marque == 2, 5,
                                                               ifelse(conduite == 2 & marque == 3, 6, NA)))))))
conso$groupe = as.factor(conso$groupe)

anova_groupe = aov(Cons ~ groupe, data = conso)
summary(anova_groupe)

# Comparaisons par paires des moyennes 
pairwise_groupe = pairs(emmeans(anova_groupe, ~ groupe), adjust = "none")

# Afficher les résultats
summary(pairwise_groupe, infer = TRUE)

# Les moyennes de consommation selon le facteur conduite, et quel que soit la marque
# du véhicule, sont différemment significatives.
# En effet, avec un niveau de confiance de 95 % :

# La moyenne de consommation lorsque la conduite est économique est de 0,64 à 1,18 
# inférieure à celle lorsque la conduite est non économique, quand la marque de voiture = 1.

# La moyenne de consommation lorsque la conduite est économique est de 0,18 à 0,67
# inférieure à celle observée en conduite non économique, lorsque la marque du 
# véhicule est 2.

# La moyenne de consommation lorsque la conduite est économique est de 1,98 à 2,51 
# inférieure à celle observée en conduite non économique, lorsque la marque du 
# véhicule est 3.

#===============================================================================
# POUR CONDUITE == 1
conso_c1 = subset(conso, conduite == 1)
mod_c1 = lm(Cons ~ marque, data = conso_c1)
anova_c1 = aov(mod_c1)
summary(anova_c1)

# Moyennes marginales estimées
emm_c1 = emmeans(mod_c1, ~ marque)

# Contrastes personnalisés
contrastes_marque = contrast(emm_c1, method = list(
  "Marque1_vs_Marque2" = c(1, -1, 0),
  "Marque2_vs_Marque3" = c(0, 1, -1),
  "Marque1_vs_Marque3" = c(1, 0, -1)
))

summary(contrastes_marque, infer = c(TRUE, TRUE))

# Avec un niveau de confiance de 95 %, lorsque la conduite est économique, 
# la moyenne de consommation de la voiture 2 est plus élevée de 0,138 à 0,605 
# que celle de la voiture 1.
# Il n'y a pas de différence significative entre les consommations moyennes 
# des marques 2 et 3.

#===============================================================================
# POUR CONDUITE == 2
conso_c2 = subset(conso, conduite == 2)
mod_c2 = lm(Cons ~ marque, data = conso_c2)
anova_c2 = aov(mod_c2)
summary(anova_c2)

# Moyennes marginales estimées
emm_c2 = emmeans(mod_c2, ~ marque)

# Contrastes personnalisés
contrastes_marque_c2 = contrast(emm_c2, method = list(
  "Marque1_vs_Marque2" = c(1, -1, 0),
  "Marque2_vs_Marque3" = c(0, 1, -1),
  "Marque1_vs_Marque3" = c(1, 0, -1)
))

summary(contrastes_marque_c2, infer = c(TRUE, TRUE))

# Il existe au moins une différence significative entre 2 moyennes.
# Il n’y a pas de différence significative pour les marques 1 et 2 lorsque 
# la conduite est non économique.
# Avec un niveau de confiance de 95 %, la moyenne de consommation de la marque 2
# est inférieure, entre 1,75 et 2,3, à celle de la marque 3 lorsque la conduite est 
# non économique.
