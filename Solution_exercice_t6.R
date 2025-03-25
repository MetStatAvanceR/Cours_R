#===============================================================================
# SOLUTION EXERCICE THÈME 6
#===============================================================================

# Charger les librairies
library(dplyr)
library(ggplot2)
library(ResourceSelection)
library(pscl)

# Définir le répertoire de travail
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")

# Charger le jeu de données
credit_data <- read.csv("AcceptationCredit.csv", sep=";", header=TRUE)

# Convertir la variable cible AcceptationCredit en facteur
credit_data$AcceptationCredit <- as.factor(credit_data$AcceptationCredit)

# Convertir la variable Infraction bancaire en variable binaire (1 = Oui, 0 = Non)
credit_data$Infractionbancaire <- ifelse(credit_data$Infractionbancaire == "Oui", 1, 0)

# Convertir la variable Revenu en valeur numérique
credit_data$Revenu <- as.numeric(gsub(",", ".", credit_data$Revenu))

#===============================================================================
# Question a

# Ajuster un modèle avec toutes les variables
modele_credit <- glm(AcceptationCredit ~ ., 
                     data = credit_data, 
                     family = binomial(link = "logit"))

# Résumé du modèle
summary(modele_credit)

# Calculer les odds ratios
odds_ratios <- exp(coef(modele_credit))
odds_ratios

# L’équation estimée : p(y) = 1 / (1 + exp⁡[-(2.745 - 1.929 * Infraction + 0.217 * Revenu - 0.062 * Âge)])

# On estime que les chances de se faire approuver le crédit diminuent de 6.1% (1 - 0.939) 
# avec chaque année supplémentaire d’âge, toute chose étant égale par ailleurs.

# On estime que les chances de se faire approuver le crédit augmentent de 24.2% 
# avec chaque 1000$ supplémentaire de revenu par tête dans le ménage, toute chose étant égale par ailleurs.

# On estime que les chances de se faire approuver le crédit pour un individu ayant 
# eu un problème avec l’établissement bancaire diminuent de 85.5% par rapport à celui n’ayant eu aucun problème.

#===============================================================================
# Question b

# Ajuster le modèle nul (sans variables explicatives)
modele_nul <- glm(AcceptationCredit ~ 1, data = credit_data, family = binomial)

# Test du rapport de vraisemblance 
anova(modele_nul, modele_credit, test = "Chisq")

# Le p-value = 0.001 < 5% donc on rejette H0 et on peut conclure que le modèle de régression
# est globalement significatif au seuil de signification de 5%.

#===============================================================================
# Question c

summary(modele_credit)$coefficients

# Au seuil de signification de 5%, seule la variable Infraction bancaire est significative 
# dans le modèle (p-value = 0.001 < 5%).

#===============================================================================
# Question d

# Calcul des pseudo R²
pR2(modele_credit)

# Le modèle explique de 14.8% (R² ML - Cox & Snell) à 21.5% (R² CU - Nagelkerke) de la variabilité observée quant à l’acceptation 
# ou non du crédit.

#===============================================================================
# Question e


# Test de Hosmer-Lemeshow
hoslem.test(modele_credit$y, fitted(modele_credit), g = 10)

# Le p-value = 0.814 > 5% donc on ne rejette pas H0. Au seuil de 5%, on ne peut pas dire 
# que le modèle ne s’ajuste pas bien aux données.

#===============================================================================
# Question f

# Prédictions du modèle 
credit_data$prob_pred <- predict(modele_credit, type = "response")

# Définir un seuil de 0.5 pour classifier en 0 ou 1
credit_data$pred_class <- ifelse(credit_data$prob_pred > 0.5, 1, 0)

# Créer une table de confusion
table(credit_data$AcceptationCredit, credit_data$pred_class)

# % Bonne classification
(68 + 8) / 100

# % Sensibilité
68 / (68 + 5)

# % Spécificité
8 / (8 + 19)

# Le taux de bonne classification est de 76%. Il est à noter que le modèle classe 
# mieux les positifs que les négatifs (29% de vrais négatifs contre 93.2% de vrais positifs).

#===============================================================================
# Question g

# Créer un nouveau profil client qui correspond aux critères
nouveau_client <- data.frame(Age = 38, Revenu = 1.7, Infractionbancaire = 0)

# Prédire la probabilité d'acceptation
prob_nouveau_client <- predict(modele_credit, newdata = nouveau_client, type = "response")
prob_nouveau_client

# Pour un individu ayant 38 ans, 1700$ comme revenu par tête dans le ménage et pas d’infraction bancaire,
# on estime que la demande de crédit sera acceptée (0.6809 > 0.5).
