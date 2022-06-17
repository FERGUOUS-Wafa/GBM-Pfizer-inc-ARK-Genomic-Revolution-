# Prévision stochastique de l'action pfizer - mouvement brownian géometrique 

Un GBM est un processus stochastique en temps continu dans lequel une quantité suit un mouvement brownien (processus de Wiener) avec dérive. C'est un exemple important de processus stochastiques satisfaisant une équation différentielle stochastique (SDE), en particulier, il est utilisé en finance mathématique pour modéliser les cours des actions dans le modèle Black-Scholes.



Nous commencerons par importer quelques packages utiles:
# Installation des packages 
```R
install.packages("readxl")
install.packages("moments")
install.packages("xts")   
install.packages("ggplot2")
install.packages("dygraphs", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("tidyverse",dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("lubridate",dependencies=TRUE, repos='http://cran.rstudio.com/')
```
# Charges les packages 
```R
library(xts)                         
library(readxl)
library(moments)
library(stats)
library(ggplot2)
library(dygraphs)
library(tidyverse)
library(lubridate)
```
# Impotrer les données 
```R
pfizer= read_excel("pfizer3.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
```
Voici les cours historiques des actions de Pfizer récupérés entre 2021-10-01 et 2022-01-31
```R
head(pfizer)
tail(pfizer)
```
ici, nous créons un data frame contenant la date et le prix de clôture de Pfizer
```R
pfe = data.frame(pfizer$Date,pfizer$Close)
head(pfe)
```
# Analyse descriptive des données Pfizer
```R
summary(pfe$pfizer.Close)
sd(pfe$pfizer.Close)


## Traçons le prix de clôture quotidien ##
PFE = xts(x = pfe$pfizer.Close, order.by = pfe$pfizer.Date)
p=dygraph(PFE , main="Pfizer close price ", ,xlab="days",ylab=" price") %>%
 dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
 dyRangeSelector() %>%
 dyCrosshair(direction = "vertical") %>%
 dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  
p
```
il y a une assez belle tendance . Il y a quelques baisses, mais la dérive semble plutôt positive.

# Les rendements 
il est très utile de travailler avec les rendements et non avec le prix lui-même.
Le rendement d'un jour à l'autre est le pourcentage de variation du cours de clôture entre les deux jours.
```R
j=1
r=seq((length(pfe[,1]))-1)
n=length(r)
n
for (i in 1:n){
r[i]= log(pfe$pfizer.Close[j+1])-log(pfe$pfizer.Close[j])
j=j+1
} 
r
```
Le résultat est celui-ci :
### Une analyse rapide des rendement 
```R
summary(r)
sd(r)
```
la moyenne est assez proche de zéro, mais le fait qu'il soit positif explique la dérive positive de la série des prix.
l'ecart type est plus d'un ordre de grandeur supérieur à la valeur moyenne. C'est clairement l'effet des valeurs aberrantes.


# La distribution des rendements
## Histograme 
```R
hist(r,col="#69b3a2")
```
Comme on peut le voir, il est assez centré autour de zéro et il semble symétrique. 
## Diagramme quantile-quantile ( Q-Qplot)
```R
qqnorm(r)
qqline(r)
```
Comme tous les points tombent approximativement le long de cette ligne de référence, nous pouvons supposer la normalité.

