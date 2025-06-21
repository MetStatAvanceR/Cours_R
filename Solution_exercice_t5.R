#===============================================================================
#SOLUTION EXERCICE THÈME 5
#===============================================================================


# Charger les librairies
library(dplyr)
library(car)
library(stats)

# Définir le répertoire de travail
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")

# Charger les données
ex = read.csv("Ex_theme5.csv ", sep=";",header=TRUE)

#===============================================================================
# Question 1: Statistiques descriptives

# Calculer les statistiques descriptives

stat_descriptive = data.frame(
  Moyenne = sapply(ex, mean, na.rm = TRUE),
  Ecart_Type = sapply(ex, sd, na.rm = TRUE),
  Minimum = sapply(ex, min, na.rm = TRUE),
  Maximum = sapply(ex, max, na.rm = TRUE)
)

print(stat_descriptive)


#===============================================================================
# Question 2: Matrice de corrélation

#Calculer la matrice de corrélation

matrice_cor = cor(ex, method = "pearson")
matrice_cor

#La corrélation est significative entre les variables suivantes :
#-	apparel_return et return_policy,
#-	apparel_return et buying_impulsiveness,
#-	buying_impulsiveness et return_policy.


#===============================================================================
# Qustion 3: Régression linéaire multiple

#On estime le modèle de régression linéaire multiple suivant:

# Apparel_return = β0+ β1*fashion_innovativeness +β2*buying_impulsiveness + β3*return_policy +ϵ 

#On ajuste un modèle linéaire

model1 = lm(apparel_return ~ fashion_innovativeness + buying_impulsiveness + return_policy, data = ex)
summary(model1)

#On fait un anova sur le modèle
anova_model1=anova(model1)
anova_model1

# Significativité globale du modèle :
# H0 : β1 = β2 = β3 = 0 vs H1 : au moins un βi ≠ 0
# Le modèle est globalement significatif (p-value = 0.00) au seuil de 1%.

# Contributions individuelles des variables explicatives :
# H0 : βi = 0 vs H1 : βi ≠ 0
# Les variables buying_impulsiveness et return_policy sont individuellement significatives 
#au seuil de 5% (p-value = 0.00 pour les 2 variables), 
# alors que la variable fashion_innovativeness n’est pas individuellement significative (p-value = 0.497).

# Équation estimée du modèle et interprétation des coefficients : 

# Apparel_return = 4.879 - 0.02 * fashion_innovativeness 
#                  + 0.22 * buying_impulsiveness 
#                  + 0.23 * return_policy 

# Si les scores fashion_innovativeness, buying_impulsiveness et return_policy 
#sont nuls, la fréquence moyenne de retour est de 4.879. 
# Si le score fashion_innovativeness augmente de 1 point, la fréquence moyenne de retour diminue 
# de 0.02. (Attention, cette variable n’est pas significative !).
# Si le score buying_impulsiveness augmente de 1 point, la fréquence moyenne de retour augmente de 0.222.   
# Si le score return_policy augmente de 1 point, la fréquence moyenne de retour augmente de 0.228.    

#===============================================================================
#Question 4: Modèle parcimonieux 

#Le modèle précedent comporte une variable non significative soit «fashion_innovativeness », 
#on l’enlève et on ré-estime le modèle suivant : 
#Apparel_return = β_0 + β_1*buying_impulsiveness + β_1*return_policy +ϵ 


#On fait un modèle linéaire sans  fashion_innovativeness
model2 = lm(apparel_return ~ buying_impulsiveness + return_policy, data = ex)
summary(model2)


#On fait un anova sur le modèle parcimonieux 
anova_model2=aov(model2)
summary(anova_model2)

#Ce modèle est globalement significatif (p-value=0.00) et toutes les variables 
#explicatives sont individuellement significatives. 
#Ce modèle est meilleur que le précédent, on peut vérifier que le R_ajusté^2 s’est légèrement amélioré. 
