#===============================================================================
#Mise en situation 2 du thème 3
#===============================================================================

# Packages requis
library(car)       
library(DescTools) 
library(ggplot2)


# Importer les données
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")
auto = read.csv("Autowebdo.csv", sep=";", header=TRUE)

# S'assurer que Revenu et Age sont traités comme des variables catégorielles
auto$Revenu =as.factor(auto$Revenu)
auto$Age =as.factor(auto$Age)
auto$Prix =gsub(",", ".", auto$Prix)    
auto$Prix =as.numeric(auto$Prix)

#===============================================================================
#Question 1-Peut-on conclure que le prix moyen réel payé pour le véhicule diffère selon 
#le revenu familial annuel brut ? Si oui, poursuivre l’analyse en comparant 
#les catégories de revenus 2 à 2 afin de détecter où se trouvent les différences et, 
#s’il y a lieu, quantifier ces différences à l’aide d’un intervalle de confiance. 
#Utiliser une méthode qui contrôle le niveau globale.


#μᵢ  : prix moyen reél payé pour le véhicule pour les acheteurs dont le revenu est dans la catégorie i.
#H₀:μ₁ = μ₂ = μ₃ = μ₄  
#H₁ : au moins deux moyennes sont différentes. 


# Test d'homogénéité des variances
leveneTest(Prix ~ Revenu, data = auto, center = "mean")


#On effectue le test d’égalité des variances :
#p-value=0 donc on ne peut pas supposer l’égalité des variances. 
#Le F test n’est donc pas valide.

#On effectue un test de welch
oneway.test(Prix ~ Revenu, data = auto, var.equal = FALSE)

#Selon le test de Welch, p-value=0 <5% donc on rejette H_0et on conclue 
#qu’au moins 2 moyennes sont différentes c’est-à-dire que le prix moyen 
#réel payé diffère selon la catégorie du revenu familial brut.


#Analyse fine :
#Les variances ne sont pas égales et on veut contrôler le niveau de 
#significativité globale donc on effectue le test Games-howell.

games_howell_test(auto, Prix ~ Revenu, conf.level = 0.95, detailed = FALSE)


#Au seuil de signification de 5 %, on observe une différence significative
#pour toutes les paires de moyennes (p-value < 0.05), ce qui permet de conclure,
#au niveau de confiance de 95 %, que :

#Le prix réel moyen payé par les acheteurs dont le revenu est entre 
#60 000 $ et 90 000 $ est de 1 117,00 $ à 3 554,40 $ plus élevé que 
#celui payé par les acheteurs dont le revenu est inférieur à 60 000 $.

#Le prix réel moyen payé par les acheteurs dont le revenu est entre
#90 000 $ et 120 000 $ est de 3 948,38 $ à 6 527,12 $ plus élevé que 
#celui payé par les acheteurs dont le revenu est inférieur à 60 000 $.

#Le prix réel moyen payé par les acheteurs dont le revenu est supérieur
#à 120 000 $ est de 7 110,74 $ à 10 150,26 $ plus élevé que
#celui payé par les acheteurs dont le revenu est inférieur à 60 000 $.

#Le prix réel moyen payé par les acheteurs dont le revenu est entre 
#90 000 $ et 120 000 $ est de 1 454,08 $ à 4 350,12 $ plus élevé que
#celui payé par les acheteurs dont le revenu est entre 60 000 $ et 90 000 $.

#Le prix réel moyen payé par les acheteurs dont le revenu est supérieur
#à 120 000 $ est de 4 640,09 $ à 7 949,61 $ plus élevé que 
#celui payé par les acheteurs dont le revenu est entre 60 000 $ et 90 000 $.

#Le prix réel moyen payé par les acheteurs dont le revenu est supérieur
#à 120 000 $ est de 1 684,97 $ à 5 100,52 $ plus élevé que celui payé par 
#les acheteurs dont le revenu est entre 90 000 $ et 120 000 $.

#===============================================================================

#Question 2- Peut-on conclure que le prix moyen réel payé pour le véhicule diffère 
#selon l’âge ? Si oui, poursuivre l’analyse en comparant les catégories d’âge 
#2 à 2 afin de détecter où se trouvent les différences et, s’il y a lieu,
#quantifier ces différences à l’aide d’un intervalle de confiance.
#Utiliser une méthode qui contrôle le niveau global.


#μᵢ: prix moyen reél payé pour le véhicule pour les acheteurs dont l’âge est dans la catégorie i.
#H₀:μ₁ = μ₂ = μ₃ = μ₄  
#H₁ : au moins deux moyennes sont différentes. 


# Test d'homogénéité des variances
leveneTest(Prix ~ Age, data = auto,center = "mean")

#On effectue le test d’égalité des variances : p-value=0.86 donc on peut supposer 
#l’égalité des variances.

Anova_age=aov(Prix ~ Age, data = auto)
summary(Anova_age)



#Boxplot
ggplot(auto, aes(x = Age, y = Prix)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "age", y = "prix réel") +
  theme_minimal()


#Le F test nous permet de rejeter H₀ (p-value=0<5%) et de conclure que au moins
#deux moyennes sont différentes c’est-à-dire que l’âge a un effet significatif 
#sur le prix réel moyen payé.

#En examinant le boxplot, on remarque la présence de quelques valeurs extrêmes, 
#on confirme donc les résultats avec un test KW (p-value=0).

kruskal.test(Prix ~ Age, data = auto)


#Analyse fine : 
#Les variances sont supposées égales et on veut contrôler le niveau de 
#significativité global donc on effectue le test Tukey.

TukeyHSD(Anova_age)

#Au seuil de signification de 5%, on conclut que les prix réels moyens 
#sont significativement différents seulement pour les catégories d’âge suivantes :

# -	Moins de 25 ans et de 25 ans à moins 40 ans.
#-	Moins de 25 ans et de 40 ans à moins 55 ans.
#-	Moins de 25 ans et plus que 55 ans.
#-	De 40 ans à moins 55 ans et plus de 55 ans.

#Au niveau de confiance 95%, on estime que :
  
#Le prix moyen payé par les clients âgés de 25 à moins de 40 ans est de 
#1 765,26 $ à 5 303,14 $ plus élevé que celui payé par les clients de moins de 25 ans.

#Le prix moyen payé par les clients âgés de 40 à moins de 55 ans est de 
#3 129,88 $ à 6 613,01 $ plus élevé que celui payé par les clients de moins de 25 ans.

#Le prix moyen payé par les clients âgés de 55 ans ou plus est de 
#123,76 $ à 3 678,44 $ plus élevé que celui payé par les clients de moins de 25 ans.

#Le prix moyen payé par les clients âgés de 40 à moins de 55 ans est de 
#1 212,06 $ à 4 728,62 $ plus élevé que celui payé par les clients âgés de 55 ans ou plus.
