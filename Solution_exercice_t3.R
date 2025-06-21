#===============================================================================
# SOLUTION EXERCICE THÈME 3
#===============================================================================

# Charger les librairies
library(car)
library(rstatix) 

# Définir le répertoire de travail
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")

# Charger le jeu de données
cc = read.csv("centre_commercial.csv", sep=";", header=TRUE)

# Conversion des variables
cc$sexe      = as.factor(cc$sexe)
cc$revenu    = as.factor(cc$revenu)
cc$situation = as.factor(cc$situation)
cc$duree     = as.factor(cc$duree)

#===============================================================================
# Question 1 : Impact du revenu familial sur le montant dépensé

# Paramètres testés :
# 𝜇i : Montant moyen dépensé par les individus appartenant à la catégorie de revenu i.

# Hypothèses statistiques :
# H0 : Les moyennes des dépenses sont égales pour toutes les catégories de revenu (𝜇1 = 𝜇2 = 𝜇3 = 𝜇4).
# H1 : Au moins une des moyennes est significativement différente des autres.

# Test d'égalité des variances
levene_test_1_1 = leveneTest(depense ~ revenu, data = cc)
print(levene_test_1_1)

# P-value < 0.05 donc on rejette l’égalité des variances

# Test de Welch pour comparer les moyennes entre les groupes
# Utilisé car les variances sont inégales
test_welch_1_1 = oneway.test(depense ~ revenu, data = cc, var.equal = FALSE)
print(test_welch_1_1)

# La p-value < 0 nous permet de rejeter H0 au seuil
# 5% et de conclure que le revenu a un effet significatif 
# sur le montant moyen dépensé lors d’une visite au centre commercial.

# La présence de plusieurs valeurs extrêmes peut influencer ce test, 
# on vérifie donc le résultat avec le test non paramétrique Kruskal-Wallis.

# Test non paramétrique de Kruskal-Wallis
# On cherche à savoir si au moins une moyenne diffère significativement
test_kruskal_1_1 = kruskal.test(depense ~ revenu, data = cc)
print(test_kruskal_1_1)

# La p-value < 0 permet de rejeter H0 au seuil 5% et de
# confirmer le résultat précédent.

# Analyse fine
# Ici les variances ne sont pas égales, et on choisit de contrôler 
# le niveau de signification global. On choisit donc Games-Howell 
# pour effectuer les comparaisons multiples.

# Test de Games-Howell
games_howell_1_1 = games_howell_test(cc, depense ~ revenu, conf.level = 0.95, detailed = FALSE)
print(games_howell_1_1)

# Au seuil de signification global 5%, 
# on conclut à une différence significative entre les paires de moyennes suivantes :

# - 𝜇1 et 𝜇3 : On estime que le montant moyen dépensé lors d’une visite 
#   au centre commercial pour les personnes avec un revenu de moins de 30 000$ 
#   est de 25.48$ à 132$ inférieur à celui pour les personnes avec un revenu 
#   entre 70 000$ et 99 999$.

# - 𝜇1 et 𝜇4 : On estime que le montant moyen dépensé lors d’une visite 
#   au centre commercial pour les personnes avec un revenu de moins de 30 000$ 
#   est de 39.90$ à 153.31$ inférieur à celui pour les personnes avec un revenu 
#   de 100 000$ et plus.

# - 𝜇2 et 𝜇4 : On estime que le montant moyen dépensé lors d’une visite 
#   au centre commercial pour les personnes avec un revenu entre 30 000$ et 69 999$ 
#   est de 5.74$ à 128.61$ inférieur à celui pour les personnes avec un revenu 
#   de 100 000$ et plus.

#===============================================================================
# Question 1 : Impact du revenu familial sur la durée de visite

# On teste maintenant l’effet du revenu familial sur la durée 
# de visite au centre commercial.
# La variable durée est une variable ordinale, 
# on choisit donc le test non paramétrique Kruskal-Wallis.

# Test non paramétrique de Kruskal-Wallis
test_kruskal_1_2 = kruskal.test(duree ~ revenu, data = cc)
print(test_kruskal_1_2)

# La p-value > 5% ne permet pas de rejeter H0. 
# On ne peut donc pas conclure que le revenu a un 
# effet significatif sur la durée de visite au centre commercial.

#===============================================================================
# Question 2 : Impact de la situation d'emploi sur le montant dépensé

# Paramètres testés :
# 𝜇i : Montant moyen dépensé par les individus en fonction de leur catégorie de situation d'emploi.

# Hypothèses statistiques :
# H0 : Les moyennes des dépenses sont égales pour toutes les catégories de situation d'emploi (𝜇1 = 𝜇2 = 𝜇3 = 𝜇4).
# H1 : Au moins une des moyennes est significativement différente des autres.

# Test d'égalité des variances
levene_test_2_1 = leveneTest(depense ~ situation, data = cc)
print(levene_test_2_1)

# P-value < 0 donc on rejette l’égalité des variances.

# Test de Welch pour comparer les moyennes entre les groupes
test_welch_2_1 = oneway.test(depense ~ situation, data = cc, var.equal = FALSE)
print(test_welch_2_1)

# La p-value < 0 nous permet de rejeter H0 au seuil 5% et de conclure que 
# la situation d’emploi a un effet significatif sur le montant
# moyen dépensé lors d’une visite au centre commercial.

# La présence de valeurs extrêmes nous incite à valider ce résultat 
# avec le test non-paramétrique Kruskal-Wallis.

# Test non paramétrique de Kruskal-Wallis
test_kruskal_2_1 = kruskal.test(depense ~ situation, data = cc)
print(test_kruskal_2_1)

# La p-value < 0 permet de rejeter H0 au seuil 5% et de
# confirmer le résultat précédent.

# Analyse fine :
# Ici les variances ne sont pas égales, et on choisit de contrôler 
# le niveau de signification global. On choisit donc Games-Howell 
# pour effectuer les comparaisons multiples.

# Test de Games-Howell
games_howell_2_1 = games_howell_test(cc, depense ~ situation, conf.level = 0.95, detailed = FALSE)
print(games_howell_2_1)

# Au seuil de signification global 5%, 
# on conclut à une différence significative entre les paires de moyennes suivantes :

# - 𝜇1 et 𝜇3 : On estime que le montant moyen dépensé lors d’une visite 
#   au centre commercial pour les personnes au travail est de 0.79$ à 94.43$ 
#   supérieur à celui pour les personnes à la retraite.

# - 𝜇1 et 𝜇4 : On estime que le montant moyen dépensé lors d’une visite 
#   au centre commercial pour les personnes au travail est de 31.68$ à 116.12$ 
#   supérieur à celui pour les personnes au chômage ou à la maison.

#===============================================================================
# Question 2 : Impact de la situation d'emploi sur la durée de visite

# On teste maintenant l’effet de la situation d’emploi sur 
# la durée de visite au centre commercial.
# La variable durée est une variable ordinale, 
# on choisit donc le test non paramétrique Kruskal-Wallis.

# Test non paramétrique de Kruskal-Wallis
test_kruskal_2_2 = kruskal.test(duree ~ situation, data = cc)
print(test_kruskal_2_2)

# La p-value > 5% ne permet pas de rejeter H0. 
# On ne peut donc pas conclure que la situation
# d’emploi a un effet significatif sur la durée de visite au centre commercial.
