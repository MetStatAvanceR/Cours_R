#===============================================================================
# SOLUTION EXERCICE THÈME 7
#===============================================================================

# Charger les librairies
library(ggplot2)
library(survival)

# Définir le répertoire de travail
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")

# On importe le jeu de données
telco <- read.csv("telco.csv", sep=";", header=TRUE)

#===============================================================================
# Question 1:

# Ajuster le modèle KM
fit_km <- survfit(Surv(tenure, churn) ~ 1, data = telco)

# Afficher la table de survie
summary(fit_km)

# Tracer la courbe de survie
plot(fit_km)

# Le plus grand nombre et la proportion d'événements terminaux se produisent dans la première 
# année, ce qui suggère que les clients devraient être suivis de plus près au cours de leur 
# première année pour être sûrs de leur satisfaction à l'égard du service de l'entreprise.

#===============================================================================
# Question 2:

# Ajuster un modèle KM selon la variable custcat
fit_custcat <- survfit(Surv(tenure, churn) ~ custcat, data = telco)

# Afficher la table de survie
summary(fit_custcat)

# Tracer la courbe de survie
plot(fit_custcat)

# Tester l'égalité des courbes de survie
survdiff(Surv(tenure, churn) ~ custcat, data = telco)

# La fonction de survie diffère selon la catégorie du client. Au seuil 5%, seules les catégories
# 1 et 4 ne présentent pas de différence significative.
# Les clients des catégories "basic" et "total" semblent quitter la compagnie plus vite. 
# Les efforts de fidélisation devront cibler ces clients plus que les autres.

#===============================================================================
# Question 3:

# Ajuster un modèle KM selon la variable gender
fit_gender <- survfit(Surv(tenure, churn) ~ gender, data = telco)

# Afficher la table de survie
summary(fit_gender)

# Tracer la courbe de survie
plot(fit_gender)

# Tester l'égalité des courbes de survie
survdiff(Surv(tenure, churn) ~ gender, data = telco)

#-----------------

# Ajuster un modèle KM selon la variable region
fit_region <- survfit(Surv(tenure, churn) ~ region, data = telco)

# Tracer la courbe de survie
plot(fit_region)

# Tester l'égalité des courbes de survie
survdiff(Surv(tenure, churn) ~ region, data = telco)

# Au seuil 5%, il n’y a pas de différence significative de la fonction de survie du 
# temps d’abonnement selon la région ou le genre.

#===============================================================================
# Question 4:

# On utilise le modèle de Cox. On procède à une sélection backward de 
# variables parmi les variables démographiques. On retient le modèle suivant :

# Ajuster un modèle de Cox avec toutes les variables démographiques
modele_cox <- coxph(Surv(tenure, churn) ~ age + gender + region + custcat + marital+ employ + address + ed + retire , data = telco)

#Appliquer une séléction backward
modele_cox_backward <- step(modele_cox, direction = "backward")
summary(modele_cox_backward)


