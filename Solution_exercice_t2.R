#===============================================================================
# SOLUTION EXERCICE DU THÈME 2
#===============================================================================

#Importer les librairies nécessaires
library(ggplot2)
library(dplyr)
library(car)
        
        
# Définir le répertoire de travail
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")
        


#===============================================================================
#Exercice 1
#===============================================================================

# Charger le jeu de données
intro = read.csv("intro.csv", sep=";", header=TRUE)
# Sélectionner uniquement les individus de moins de 40 ans
intro2 = subset(intro, age < 40)
# Convertir la variable "sexe" en facteur
intro2$sexe = as.factor(intro2$sexe)


#Préalablement à un test d'hypothèse, on effectue un test sur les variances 
#afin de vérifier si elles sont statistiquement différentes ou non.

# Effectuer un test de Levene pour vérifier l'égalité des variances
leveneTest(visite ~ sexe, data = intro2)

#Selon le test de Levene, on ne peut pas conclure à une différence significative
#entre les deux variances (valeur-p : 0.3145 > 5%). 
#On peut donc supposer l’égalité des variances dans les populations de référence.


# Hypothèses confrontées :

# H0 : μ_homme = μ_femme 
# H1 : μ_homme ≠ μ_femme 

# Test de Student pour variances égales
t.test(visite ~ sexe, data = intro2, var.equal = TRUE)

# Conclusion :
# Valeur-p du test : 0.033 < 5% => On peut rejeter H0.
# La différence entre les deux moyennes observées semble statistiquement significative.
# Au niveau des populations de référence, on peut donc conclure, au seuil de signification 5%, 
# que le nombre moyen de visites au cinéma effectuées par les hommes est différent de celui des femmes.
# Selon l’intervalle de confiance de niveau 95%, les hommes de la population considérée 
# sont allés en moyenne de 0.199 à 4.76 fois de plus au cinéma que les femmes.


# Test de Wilcoxon pour comparer les distributions
wilcox.test(visite ~ sexe, data = intro2)

# On arrive à la même conclusion qu’avec le test-T (On peut rejeter H0 et conclure à une différence entre les deux groupes car Valeur-p : 0.045 < 5%).

# Remarque:

# On utilise généralement un test non paramétrique lorsque les données sont ordinales seulement ou lorsque les conditions d’utilisation du test-T ne sont pas satisfaites.
# Ici le nombre de visites est une variable quantitative. 
# La taille de chaque « échantillon » est suffisamment grande (48 hommes et 31 femmes). 
# Dans chaque groupe, il y a quelques observations éloignées vers les grandes valeurs mais pas de donnée vraiment extrême. 
# À priori, iI n’y a donc pas de raison de douter de la validité du test-T. 
# En cas de doute, il peut être pertinent de confirmer (ou infirmer) la conclusion d’un test-T à l’aide d’un test non paramétrique.


#===============================================================================
#Exercice 2
#===============================================================================

#===============================================================================
#Question 1

# Charger le fichier centre_commercial
cc = read.csv("centre_commercial.csv", sep=";", header=TRUE)

# Convertir la variable "sexe" en facteur
cc$sexe = as.factor(cc$sexe)


# Visualisation des fréquentations avec ggplot2
ggplot(cc, aes(x = factor(1), y = m_Bruno)) +
  geom_boxplot(fill = "lightblue") +
  geom_boxplot(aes(y = P_Champlain), fill = "lightblue") +
  labs(title = "Fréquentation des centres",
       x = "Centre Commercial", 
       y = "Fréquentation") +
  theme_minimal()


# Hypothèses confrontées :
# 𝜇d : Différence des nombres de visites des deux centres commerciaux durant les 3 derniers mois.
# 𝐻0: 𝜇d = 0
# 𝐻1: 𝜇d ≠ 0


# Test-T pour données appariées
t.test(cc$m_Bruno, cc$P_Champlain, paired = TRUE)

# Conclusion:
# On ne rejette pas H0 car valeur-p: 0.108 > 5%.
# On ne peut donc pas conclure à une différence significative entre les 2 moyennes, 
# c'est-à-dire qu’on ne peut pas conclure que les gens ont tendance à aller plus souvent 
# à un des deux centres d’achats plutôt que l’autre, en moyenne.


# La présence de certaines valeurs extrêmes nous incite à valider le résultat du test T 
# par un test non paramétrique.

# Test de Wilcoxon pour vérifier la robustesse
wilcox.test(cc$m_Bruno, cc$P_Champlain, paired = TRUE)

#valeur-p: 0.4485>5%, on tombe sur la meme conclusion, il 
#n"y a pas une différence significative dans la fréquentation des deux centres.


#===============================================================================
#Question 2

# Visualisation avec ggplot2
ggplot(cc, aes(x = sexe, y = nb_magasins, fill = sexe)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightblue")) +
  labs(title = "Nombre de visites selon le sexe",
       x = "Sexe",
       y = "Nombre de magasins visités") +
  theme_minimal()


#Préalablement à un test d'hypothèse, on effectue un test sur les variances 
#afin de vérifier si elles sont statistiquement différentes ou non.

# Test de Levene pour comparer les variances
leveneTest(nb_magasins ~ sexe, data = cc)

#Selon le test de Levene, on ne peut pas conclure à une différence significative 
#entre les deux variances (valeur-p : 0.070 > 5%).
#On peut donc supposer l’égalité des variances dans les populations de référence.


# Hypothèses confrontées :

# 𝜇hommes : nombre de magasins visités lors d’une visite au centre commercial pour les hommes.
# 𝜇femmes : nombre de magasins visités lors d’une visite au centre commercial pour les femmes.

# H0 : 𝜇hommes > 𝜇femmes
# H1 : 𝜇hommes < 𝜇femmes


# Test de Student (T-test) pour comparer les moyennes
t.test(nb_magasins ~ sexe, data = cc, var.equal = FALSE, alternative = "less")


# Conclusion:
#p-value= 0.032<0.05
# On rejette H0 et on peut conclure que pour les populations considérées, 
# les femmes visitent en moyenne plus de magasins que les hommes.


# La présence de certaines valeurs extrêmes nous incite à valider le résultat du test T 
# par un test non paramétrique.

# Test de Wilcoxon pour robustesse
wilcox.test(nb_magasins ~ sexe, data = cc, alternative = "less")

#On arrive à la même conclusion qu’avec le test-T ( car valeur-p : 0.008< 5%).

#===============================================================================
#Question 3

# Visualisation avec ggplot2
ggplot(cc, aes(x = sexe, y = depense, fill = sexe)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightblue")) +
  labs(title = "Dépenses selon le sexe",
       x = "Sexe",
       y = "Montant dépensé") +
  theme_minimal()

#Préalablement à un test d'hypothèse, on effectue un test sur les variances 
#afin de vérifier si elles sont statistiquement différentes ou non.

# Test de Levene pour comparer les variances
leveneTest(depense ~ sexe, data = cc)

#Selon le test de Levene, on ne peut pas conclure à une différence significative entre les deux variances (valeur-p : 0.851 > 5%). 
#On peut donc supposer l’égalité des variances dans les populations de référence.


# Hypothèses confrontées :

# 𝜇hommes : montant dépensé lors d’une visite au centre commercial pour les hommes.
# 𝜇femmes : montant dépensé lors d’une visite au centre commercial pour les femmes.

# H0 : 𝜇hommes > 𝜇femmes
# H1 : 𝜇hommes < 𝜇femmes


# Test de Student pour comparer les moyennes
t.test(depense ~ sexe, data = cc, var.equal = TRUE)


# Conclusion:
# valeur-p du test : 0.929 > 5%
# On ne rejette pas H0 et on ne peut pas conclure que pour les populations considérées, 
# les femmes dépensent en moyenne plus que les hommes.


#===============================================================================
#Hypothèses 4

# Visualisation avec ggplot2
ggplot(cc, aes(x = sexe, y = duree, fill = sexe)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightblue")) +
  labs(title = "Durée des visites selon le sexe",
       x = "Sexe",
       y = "Durée des visites") +
  theme_minimal()



# Hypothèses confrontées :

# H0 : les hommes et les femmes ont tendance à passer le même temps dans le centre commercial.
# H1 : les femmes ont tendance à passer plus de temps dans le centre commercial que les hommes.


# Test de Wilcoxon pour comparer la durée des visites
wilcox.test(duree ~ sexe, data = cc)

#Conclusion:
# valeur-p du test = 0.038
# Ce qui permet de rejeter H0 au seuil de 5% et de conclure que 
# les femmes ont tendance à passer plus de temps que les hommes dans le centre commercial.


#===============================================================================
#Question 5

# Créer une variable indiquant la préférence pour un centre commercial
cc$preference = ifelse(cc$P_Champlain > cc$m_Bruno, "Champlain", "Bruno")

# Construire un tableau de contingence
tableau = table(cc$preference, cc$sexe)

# Hypothèses confrontées :

# p_homme : Proportion des hommes qui fréquentent plus souvent les promenades Champlain que le mail Bruno.
# p_femme : Proportion des femmes qui fréquentent plus souvent les promenades Champlain que le mail Bruno.

# H0 : p_homme = p_femme
# H1 : p_homme ≠ p_femme

# Test du Chi-deux
chisq.test(tableau)

# Conclusion:
# Valeur-p = 0.01, ce qui permet de rejeter H0 au seuil de 5%.
# On peut donc conclure que la proportion des gens qui fréquentent plus souvent 
# les promenades Champlain que le mail Bruno est différente parmi les hommes et les femmes, 
# au seuil de signification 5%.

