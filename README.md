# Simulation des cours boursiers ( de Pfizer Inc)  à l'aide du mouvement brownien géométrique

# Description du projet
Dans ce projet, nous discuterons du mouvement brownien géométrique. Nous allons apprendre à simuler un tel processus et estimer les paramètres nécessaires  pour une simulation à partir de données utilisant R . Notre objectif concret sera de simuler de nombreux mouvements browniens géométriques, éventuellement corrélés. Lors de la construction du script, il est également exploré l'intuition derrière le modèle GBM. Le modèle de simulation que nous développons ici est un modèle à temps discret. Par conséquent, toutes les mathématiques discutées ici sont l'analogie en temps discret de GBM pour les processus stochastiques continus.

# Contenu

# Définition du mouvement brownien géométrique
Un GBM est un processus stochastique en temps continu dans lequel une quantité suit un mouvement brownien ( appelé aussi  processus de Wiener) avec dérive. C'est un exemple important de processus stochastiques satisfaisant une équation différentielle stochastique (SDE); en particulier, il est utilisé en finance mathématique pour modéliser les cours des actions dans le modèle Black-Scholes.

# À propos de Pfizer 
Pfizer est une société pharmaceutique américaine fondée en 1849.
Présent dans plus de 150 pays, le groupe est, en 2013, le leader mondial dans son secteur avec un chiffre d'affaires s'élevant à 51,58 milliards de dollars US, une capitalisation boursière de 181 milliards de dollars US en septembre 2012 (111 en 2009) et des effectifs de 81 800 employés dans le monde.

# Installation des packages 
```R
install.packages("xts")
install.packages("readxl")
```
# Importer des données
```R
library(readxl)
pfizer= read_excel("pfizer.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
```
Nous allons maintenant travailler avec l'objet pfizer , qui contient le cours quotidien de l'action.

# Analyse descriptive des données Pfizer 



