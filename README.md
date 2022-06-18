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
![Capture d’écran 2022-06-18 à 04 00 21](https://user-images.githubusercontent.com/79210016/174420377-7ec9a511-aa50-424e-bcd1-15fca4d8cb7b.png)

ici, nous créons un data frame contenant la date et le prix de clôture de Pfizer
```R
pfe = data.frame(pfizer$Date,pfizer$Close)
head(pfe)
```
![Capture d’écran 2022-06-18 à 04 00 54](https://user-images.githubusercontent.com/79210016/174420373-33b53607-184b-4e0b-91d2-9f15273c26f3.png)

# Analyse descriptive des données Pfizer
```R
summary(pfe$pfizer.Close)
sd(pfe$pfizer.Close)
```
![Capture d’écran 2022-06-18 à 04 01 14](https://user-images.githubusercontent.com/79210016/174420372-25b6dbeb-7ffc-40ca-aadd-ebe39d0c7c7c.png)

```R
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
![Capture d’écran 2022-06-18 à 04 01 40](https://user-images.githubusercontent.com/79210016/174420371-7fbc137b-ea55-400a-b8b3-623247df6b18.png)

### Une analyse rapide des rendement 
```R
summary(r)
sd(r)
```
![Capture d’écran 2022-06-18 à 04 03 16](https://user-images.githubusercontent.com/79210016/174420368-2ea119f7-2da5-4cbe-ae5c-19c6b48c70cd.png)

la moyenne est assez proche de zéro, mais le fait qu'il soit positif explique la dérive positive de la série des prix.
l'ecart type est plus d'un ordre de grandeur supérieur à la valeur moyenne. C'est clairement l'effet des valeurs aberrantes.


# La distribution des rendements
## Histograme 
```R
hist(r,col="#69b3a2")
```
![Capture d’écran 2022-06-18 à 02 49 29](https://user-images.githubusercontent.com/79210016/174419723-b7f31cc2-f3c6-41bd-a460-1c96e3f043d1.png)

Comme on peut le voir, il est assez centré autour de zéro et il semble symétrique. 
## Diagramme quantile-quantile ( Q-Qplot)
```R
qqnorm(r)
qqline(r)
```
![Capture d’écran 2022-06-18 à 02 49 52](https://user-images.githubusercontent.com/79210016/174419721-a7a6ecfc-5bf0-4f87-93d4-f823678e12fb.png)

Comme tous les points tombent approximativement le long de cette ligne de référence, nous pouvons supposer la normalité.

###le carré de la serie des rendement 
```R
r2=r^2
r2
```
![Capture d’écran 2022-06-18 à 04 03 35](https://user-images.githubusercontent.com/79210016/174420367-deb8bfd6-9268-4ad2-becc-b77f74d79d21.png)

## les corrélogrammes
```R
acf(r,type="correlation")
acf(r2,type="correlation")
```
![Capture d’écran 2022-06-18 à 02 50 36](https://user-images.githubusercontent.com/79210016/174419719-fe4ddfdc-f6aa-4207-b982-85f5f808a80d.png)
![Capture d’écran 2022-06-18 à 02 50 57](https://user-images.githubusercontent.com/79210016/174419717-03b550f3-0299-4da7-8dd4-4c5b40d1a5d4.png)

D'après le graphique, nous pouvons voir qu'il n'y a pas de corrélation élevée entre tous les lag.

## teste d'autocorrélation
```R
Box.test(r,lag=1)
Box.test(r2,lag=1)
```
![Capture d’écran 2022-06-18 à 04 03 51](https://user-images.githubusercontent.com/79210016/174420365-197219be-2392-4d80-a5b8-22ac4dd7edb9.png)

L'EDS (équation différentielle stochastique) du processus est :

# Estimation de dérive et de volatilité 

```R
#La moyenne de rendements #
rmoy=mean(r)
rmoy
#La variance de rendement #
sr=sd(r)
sr
sr2=sr^2
# Estimateur de mu
h=1/252
h
mu=(rmoy/h)+(sr2/2*h)
mu
# Estimatur de sigma 
sigma=sr/sqrt(h)
sigma
```
![Capture d’écran 2022-06-18 à 04 04 21](https://user-images.githubusercontent.com/79210016/174420364-7320fc91-04eb-48d2-9804-7962038fb1c3.png)


![Capture d’écran 2022-06-18 à 04 05 11](https://user-images.githubusercontent.com/79210016/174420362-ad56d834-31e0-4890-85af-bd3be7ef5022.png)

![Capture d’écran 2022-06-18 à 04 05 18](https://user-images.githubusercontent.com/79210016/174420361-c3785dfb-d832-408a-9578-687c110f377b.png)
# Simulation d’un Mouvement Brownien Géométrique
```R
nsim =1000
S0 = pfe$pfizer.Close[1]
s = length(pfe$pfizer.Date)
t=1/3
dt=t/s
GBM=matrix(ncol = nsim, nrow = s)
for (j in 1:nsim){
    for (i in 2:s) {
        y=rnorm(1,0,1)
       GBM[1,j]=S0
       GBM[i,j]=GBM[i-1,j]*exp(((mu-(sigma^2)/2)*dt)+(sigma*y*sqrt(dt)))

    }
}
 head(GBM)
```
![Capture d’écran 2022-06-18 à 04 06 51](https://user-images.githubusercontent.com/79210016/174420357-e81fb67e-b178-4faf-b813-50cd033c49d0.png)
```R
 ts.plot(GBM, gpars = list(col=rainbow(10))

```
![Capture d’écran 2022-06-18 à 02 55 31](https://user-images.githubusercontent.com/79210016/174419713-16efdf45-e3ab-4df3-b132-1382d863b4a3.png)



```R
g=data.frame(pfe$pfizer.Date,GBM[,12],pfe$pfizer.Close)
require(ggplot2)

ggplot(g, aes(pfe$pfizer.Date)) +                    
  geom_line(aes(y=GBM[,12]), colour="red") +  
  geom_line(aes(y=pfe$pfizer.Close), colour="green")  
```

![Capture d’écran 2022-06-18 à 03 56 21](https://user-images.githubusercontent.com/79210016/174420010-932247ef-9e39-4c52-bdd3-078b436709f1.png)
