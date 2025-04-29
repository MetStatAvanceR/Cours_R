#===============================================================================
# Mise en situation thème 6
#===============================================================================

# Packages requis
library(dplyr)
library(ResourceSelection)
library(MASS)
library(pscl)

# Importer les données
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")
faillite <- read.csv("Faillite.csv", sep = ";", header = TRUE)

# Nettoyage des données 
faillite$X1 <- as.numeric(gsub(",", ".", faillite$X1))
faillite$X2 <- as.numeric(gsub(",", ".", faillite$X2))
faillite$X3 <- as.numeric(gsub(",", ".", faillite$X3))
faillite$X4 <- as.numeric(gsub(",", ".", faillite$X4))

#===============================================================================
# Question 1 : Modèles logistiques simples et interprétation des odds ratios


mod_X1 <- glm(Y ~ X1, data = faillite, family = "binomial")
mod_X2 <- glm(Y ~ X2, data = faillite, family = "binomial")
mod_X3 <- glm(Y ~ X3, data = faillite, family = "binomial")
mod_X4 <- glm(Y ~ X4, data = faillite, family = "binomial")

# Odds ratios
exp(coef(mod_X1))
exp(coef(mod_X2))
exp(coef(mod_X3))
exp(coef(mod_X4))

# Interprétations:
# X1 : odds ratio ≈ 0.0005 < 1
# Lorsque le ratio flux de trésorerie / dette totale augmente, la probabilité de faillite diminue fortement.
# Elle diminue de 1 - 0.0005 ≈ 99.95 %.

# X2 : odds ratio ≈ 0.000000005 < 1
# Lorsque le ratio résultat net / actif augmente, la probabilité de faillite diminue de près de 100 %.
# Ce ratio est donc très discriminant.

# X3 : odds ratio ≈ 0.034 < 1
# Lorsque le ratio actif à court terme / dette à court terme augmente, la probabilité de faillite diminue de 1 - 0.034 = 96.6 %.

# X4 : odds ratio ≈ 1.38 > 1 mais non significatif (p = 0.84)
# Lorsque le ratio actif à court terme / ventes augmente, l’effet sur la probabilité de faillite n’est pas significatif.

# Pseudo R² pour comparaison
pR2(mod_X1)
pR2(mod_X2)
pR2(mod_X3)
pR2(mod_X4)

# Classement des modèles selon la performance (pseudo R²) :
# 1. mod_X3  
# 2. mod_X1  
# 3. mod_X2  
# 4. mod_X4

#===============================================================================
# Question 2 : Régression logistique multiple et odds ratios
#===============================================================================

mod_mult <- glm(Y ~ X1 + X2 + X3 + X4, data = faillite, family = "binomial")
summary(mod_mult)
exp(coef(mod_mult))

# Équation estimée : log(P / (1 - P)) = 5.32 - 7.14*X1 + 3.70*X2 - 3.42*X3 + 2.97*X4

# Interprétation odds ratio :
#X1 : odds ratio ≈ 0.0008 < 1
#Lorsque le ratio flux de trésorerie / dette totale augmente, la probabilité de faillite diminue fortement.
#Elle diminue de 1 - 0.0008 ≈ 99.92 %.

#X2 : odds ratio ≈ 40.58 > 1
#Lorsque le ratio résultat net / actif augmente, la probabilité de faillite augmente fortement.
#La cote est multipliée par environ 40.

#X3 : odds ratio ≈ 0.033 < 1
#Lorsque le ratio actif à court terme / dette à court terme augmente, la probabilité de faillite diminue fortement.
#Elle diminue de 1 - 0.033 ≈ 96.7 %.

#X4 : odds ratio ≈ 19.46 > 1
#Lorsque le ratio actif à court terme / ventes augmente, la probabilité de faillite augmente fortement.
#La cote est multipliée par environ 19.

#===============================================================================
# Question 3 : Variables significatives au seuil de 5 %

# Seule la variable X3 est significative (p = 0,00455)

#===============================================================================
# Question 4 : Test de significativité globale


mod_simple <- glm(Y ~ 1, data = faillite, family = "binomial")
anova(mod_simple, mod_mult, test = "Chisq")

# p-value = 2.92e-07 < 5 %, donc le modèle complet est globalement significatif.

#===============================================================================
# Question 5 : Multicolinéarité et interprétation contradictoire


vif(mod_mult)
# Les interprétations dans le modèle multiple contredisent celles des modèles simples,
# ce qui s'explique par la forte corrélation entre les ratios financiers.

#===============================================================================
# Question 6 : Proportion de variabilité expliquée (pseudo R²)


pR2(mod_mult)

# Le modèle explique entre 54,3 % et 72,5 % de la variabilité observée.

#===============================================================================
# Question 7 : Ajustement du modèle


hoslem.test(faillite$Y, fitted(mod_mult))

# p-value = 0.353 > 5 %, donc pas d'évidence que le modèle ajuste mal les données.

#===============================================================================
# Question 8 : Pouvoir de discrimination


proba <- predict(mod_mult, type = "response")
pred_class <- ifelse(proba >= 0.5, 1, 0)
table(Observé = faillite$Y, Prédit = pred_class)
mean(faillite$Y == pred_class) 
# Taux de bonne classification = 91.3 %

#===============================================================================
# Question 9 : Sélection de variables (stepwise)


stepwise_model <- stepAIC(mod_mult, direction = "both")
summary(stepwise_model)

# Variables retenues : X1 et X3

#===============================================================================
# Question 10 : Table de classification du modèle retenu


probs <- predict(stepwise_model, type = "response")
prediction <- ifelse(probs > 0.5, 1, 0)
table(Prediction = prediction, Réel = faillite$Y)
mean(prediction == faillite$Y)  
# Taux de bonne classification = 91.3 %
