install.packages("tidyverse")
install.packages("stargazer")
install.packages("dyplr")

library(tidyverse)
library(stargazer)
library(dplyr)
library(ggplot2)

# --- Étape 1 : Chargement des données ---
# Important : Le séparateur est un point-virgule (;) et non une virgule (,)
# L'argument 'file' doit être le chemin où votre fichier CSES_1.csv est stocké localement.
df_cses <- read.csv("/Users/juliaduludebellavance/Documents/CSES_1.csv", sep = ";", header = TRUE)

# --- Étape 2 : Nettoyage et Préparation ---
# Nettoyage des données : On retire les lignes où les variables d'intérêt sont manquantes (NA)
cses_clean <- df_cses %>%
  # On utilise les noms de colonnes identifiés dans votre base de données
  drop_na(int_pol, perc_corruption)

# --- Étape 3 : Analyse Statistique (Régression Linéaire Simple) ---
# Modélisation : Intérêt politique (int_pol) en fonction de la perception de la corruption (perc_corruption)
modele_corruption <- lm(int_pol ~ perc_corruption, data = cses_clean)

# Affichage des résultats pour tester l'hypothèse (corrélation négative)
cat("--- Résultats de la Régression Linéaire : Intérêt Politique vs Corruption ---\n")
summary(modele_corruption)

# --- Étape 4 : Visualisation (Nuage de points + Ligne de régression) ---
# Création du diagramme de dispersion
ggplot(cses_clean, aes(x = perc_corruption, y = int_pol)) +
  
  # Nuage de points
  geom_point(alpha = 0.5, color = "#5a4a42") +
  
  # Ajout de la ligne de régression (ligne de tendance)
  # se = TRUE affiche l'intervalle de confiance
  geom_smooth(method = "lm", se = TRUE, color = "brown") +
  
  # Personnalisation des titres et étiquettes
  labs(
    title = "Relation entre la Perception de la corruption et l'Intérêt politique",
    x = "Perception de la corruption (Variable Indépendante)",
    y = "Niveau d'intérêt politique (Variable Dépendante)"
  ) +
  theme_minimal() + # Style minimaliste
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Centre le titre