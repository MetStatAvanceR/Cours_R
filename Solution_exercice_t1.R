#===============================================================================
#SOLUTION EXERCICE THÈME 1
#===============================================================================

# Charger la bibliothèque nécessaire
library(ggplot2)

# Définir le répertoire de travail
setwd("C:/Users/Fatou/OneDrive - HEC Montréal/PROJET SUPERVISE/Data")

# Charger le jeu de données
intro <- read.csv("intro.csv", sep=";", header=TRUE)

# Sélectionner uniquement les individus de moins de 40 ans
intro2 <- subset(intro, age < 40)


#===============================================================================
# Question 1

# Triés les individus par nombre de visites décroissant
print(intro2[order(-intro2$visite), ])

# L'individu ayant effectué le plus grand nombre de visites est un homme de 15 ans.

#===============================================================================
# Question 2

# Créer la variable binaire vis_bin : 1 si les visites > 5, sinon 0
intro2$vis_bin = ifelse(intro2$visite > 5, 1, 0)

# # Convertir `vis_bin` en facteur avec des étiquettes claires: 0 = "5 ou moins", 1 = "plus de 5"
intro2$vis_bin = factor(intro2$vis_bin, 
                        levels = c(0, 1), 
                        labels = c("5 ou moins", "plus de 5"))

# Afficher la distribution des fréquences pour la variable vis_bin
table(intro2$vis_bin)

# Calculer manuellement le pourcentage des visites quand visites > 5
print((sum(intro2$vis_bin == "plus de 5") / 79) * 100)

#===============================================================================
# Question 3

# Afficher la moyenne, médiane, premiers et 3e quartiles pour le nombre de visites
summary(intro2$visite)

# Calculer l'écart type du nombre de visites
sd(intro2$visite)


# Créer un histogramme pour visualiser la distribution du nombre de visites
ggplot(intro2, aes(x = visite)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogramme des visites au cinéma",
       x = "Nombre de visites",
       y = "Fréquence") +
  theme_minimal()

# Créer un diagramme en boîte pour visualiser la distribution des visites
ggplot(intro2, aes(y = visite)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot des visites au cinéma",
       y = "Nombre de visites") +
  theme_minimal()

#Les 79 répondants âgés de moins de 40 ans sont allés en moyenne 9.15 fois au
#cinéma au cours de la dernière année, avec un écart type de 5.087 fois.
#La médiane nous indique que tout au plus 50% des répondants considérés y
#sont allés moins de 9 fois (et tout au plus 50%, plus de 9 fois). Les valeurs des
#premier et troisième quartiles nous permettent d’affirmer qu’au moins la moitié
#des individus y sont allés de 6 à 10 fois. L’histogramme et le diagramme boîte à
#moustache mettent en évidence l’asymétrie de la distribution et la présence de
#certaines valeurs « éloignées »...

#===============================================================================
# Question 4

# Créer un tableau croisé pour analyser la répartition entre sexe et vis_bin
table(intro2$sexe, intro2$vis_bin)

# Calculer la proportion de femmes parmi ceux qui sont allés plus de 5 fois
sum(intro2$sexe == 1 & intro2$vis_bin == "plus de 5") / sum(intro2$vis_bin == "plus de 5")

# Calculer la proportion d'individus ayant visité plus de 5 fois parmi les hommes
sum(intro2$sexe == 0 & intro2$vis_bin == "plus de 5") / sum(intro2$sexe == 0)

# Parmi ceux qui sont allés plus de 5 fois au cinéma, 35,4 % (soit 23 sur 65) sont des femmes,
# et 87,5 % (soit 42 sur 48) des hommes ont visité le cinéma plus de 5 fois.

#===============================================================================
# Question 5

# Population considérée : Les individus de moins de 40 ans qui sont allés au cinéma au moins une fois à Montréal
# au cours la dernière année.


# Calculer la proportion d'individus ayant visité plus de 5 fois parmi la population totale (79 individus)
sum(intro2$vis_bin == "plus de 5") / 79

#Estimé ponctuel de la proportion d’individus de cette population qui sont allés plus de 5 fois au
#cinéma = 65/79 = 82.3%

#CALCULER LINTERVALLE DE CONFIANCE

#===============================================================================
# Question 6

# Calculer l'estimé ponctuel du nombre moyen de visites au cinéma 
mean(intro2$visite)


# Calculer l'intervalle de confiance à 95% pour la moyenne du nombre de visites
t.test(intro2$visite, conf.level = 0.95)

# Au niveau de confiance 95% on estime que les individus de la population considérée sont allés,
# en moyenne, de 8.01 à 10.29 fois au cinéma au cours de la dernière année.

#===============================================================================
# Question 7

# Créer des diagrammes en boîte pour comparer le nombre de visites selon le sexe
ggplot(intro2, aes(x = as.factor(sexe), y = visite, fill = as.factor(sexe))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  labs(title = "Diagramme boîte des visites selon le sexe",
       x = "Sexe",
       y = "Nombre de visites") +
  theme_minimal()


# Pour les données observées, les hommes (sexe=0) semblent avoir tendance à aller légèrement plus souvent
# au cinéma que les femmes. Cependant pour savoir si l’écart entre les deux groupes est statistiquement significatif,
# ce qui permettrait de conclure qu’il y a effectivement une différence au niveau des populations d’hommes et de femmes,
# il faudrait faire un test d’hypothèse.
