# =====================================================================
# Chargement des packages
library(zoo)
library(ggplot2)

# =====================================================================
# Importer les données
setwd(":\\Projets_MSC\\Fatoumata\\Livrables\\Data")
meteo = read.csv("1987.csv", sep = ";", header = TRUE)

# QUESTION 1 : Représentation graphique de la température

# Renommer les colonnes
colnames(meteo) = c("Date", "Temp")
meteo$Date = as.Date(meteo$Date)

# Création d'une variable temporelle t (1 à 365)
meteo$t = 1:nrow(meteo)

# =====================================================================
# QUESTION 2 : Représentation graphique de la température

ggplot(meteo, aes(x = Date, y = Temp)) +
  geom_line(color = "lightblue") +
  labs(title = "Température quotidienne en 1987",
       x = "Date", y = "Température (°C)") +
  theme_minimal()

# On observe que les températures sont assez élevées en début d'année.
# Elles diminuent progressivement entre juin et août,
# avant de remonter de nouveau en fin d'année.

# =====================================================================
# Question 3 : Calcul des moyennes mobiles

# Calcul de la moyenne mobile sur 7 jours et 15 jours
meteo$mm7 = rollmean(meteo$Temp, k = 7, fill = NA, align = "center")
meteo$mm15 = rollmean(meteo$Temp, k = 15, fill = NA, align = "center")

# Afficher les 10 prmières observations
head(meteo,10)

# Représentation graphique de la série avec moyennes mobiles
ggplot(meteo, aes(x = Date)) +
  geom_line(aes(y = Temp), color = "lightgrey") +
  geom_line(aes(y = mm7), color = "red", size = 1) +
  geom_line(aes(y = mm15), color = "blue", size = 1) +
  labs(title = "Comparaison des moyennes mobiles (k=7 et k=15)",
       x = "Date", y = "Température (°C)") +
  theme_minimal()

# La moyenne mobile avec k = 7 est moins lisse, car elle reste sensible aux variations quotidiennes,
# tandis que celle avec k = 15 donne une tendance plus lissée, 
#moins influencée par les fluctuations quotidiennes.

# =====================================================================
#Question 3_suite: Calcul de la tendance a l'aide des moyennes mobiles

# Vérification des tendances à t = 77

# Extraire les valeurs de moyenne mobile à t=77
meteo[meteo$t == 77, c("mm7", "mm15")]


# À t = 77, la tendance est de 14.32°C avec la moyenne mobile à 7 jours,
# et de 12.75°C avec la moyenne mobile à 15 jours.

# =====================================================================
# QUESTION 4 : Modélisation de la tendance

# Ajustement du modèle linéaire
mod_lin = lm(Temp ~ t, data = meteo)
summary(mod_lin)

# Ajustement des modèles polynomiaux
mod_poly2 = lm(Temp ~ poly(t, 2), data = meteo)
summary(mod_poly2)

mod_poly3 = lm(Temp ~ poly(t, 3), data = meteo)
summary(mod_poly3)

mod_poly4 = lm(Temp ~ poly(t, 4), data = meteo)
summary(mod_poly4)

# Prédiction des tendances
meteo$pred_lin = predict(mod_lin)
meteo$pred_poly2 = predict(mod_poly2)
meteo$pred_poly3 = predict(mod_poly3)
meteo$pred_poly4 = predict(mod_poly4)

# =====================================================================
# Représentations graphiques des différents modèles

# Modèle linéaire
ggplot(meteo, aes(x = t)) +
  geom_line(aes(y = Temp), color = "grey70") +
  geom_line(aes(y = pred_lin), color = "blue", linetype = "dashed") +
  labs(title = "Tendance estimée - Modèle linéaire",
       x = "Jour", y = "Température (°C)") +
  theme_minimal()

# Modèle polynomial degré 2
ggplot(meteo, aes(x = t)) +
  geom_line(aes(y = Temp), color = "grey70") +
  geom_line(aes(y = pred_poly2), color = "green") +
  labs(title = "Tendance estimée - Modèle polynomial (degré 2)",
       x = "Jour", y = "Température (°C)") +
  theme_minimal()

# Modèle polynomial degré 3
ggplot(meteo, aes(x = t)) +
  geom_line(aes(y = Temp), color = "grey70") +
  geom_line(aes(y = pred_poly3), color = "red") +
  labs(title = "Tendance estimée - Modèle polynomial (degré 3)",
       x = "Jour", y = "Température (°C)") +
  theme_minimal()

# Modèle polynomial degré 4
ggplot(meteo, aes(x = t)) +
  geom_line(aes(y = Temp), color = "grey70") +
  geom_line(aes(y = pred_poly4), color = "purple") +
  labs(title = "Tendance estimée - Modèle polynomial (degré 4)",
       x = "Jour", y = "Température (°C)") +
  theme_minimal()


# Les modèles polynomiaux de degré 2 et 3 permettent de capturer la tendance principale de la série,
# tandis que le modèle de degré 4 s'ajuste beaucoup plus aux variations de court terme, au risque de surajuster.
# Le R² ajusté du modèle de degré 2 est de 0.34, et celui du modèle de degré 3 est de 0.36.
# La différence étant négligeable, nous choisissons le modèle de degré 2,
# car il offre le meilleur compromis entre simplicité et qualité d'ajustement.
# Il permet de modéliser la tendance globale sans suivre excessivement le bruit quotidien.
# le modèle 2 est un modèle parcimonieux.


