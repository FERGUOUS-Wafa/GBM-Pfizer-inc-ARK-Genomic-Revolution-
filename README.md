# Prévision stochastique de l'action pfizer - mouvement brownian géometrique 

Un GBM est un processus stochastique en temps continu dans lequel une quantité suit un mouvement brownien (processus de Wiener) avec dérive. C'est un exemple important de processus stochastiques satisfaisant une équation différentielle stochastique (SDE), en particulier, il est utilisé en finance mathématique pour modéliser les cours des actions dans le modèle Black-Scholes.

l'équation différentielle stochastique est la suivante:

![Capture d’écran 2022-06-18 à 14 17 55](https://user-images.githubusercontent.com/79210016/174439490-f4655888-67f9-44ba-876a-41c51be2cbe2.png)

les deux sections sont divisées par 𝑆𝑡 de sorte que :
![Capture d’écran 2022-06-18 à 14 20 06](https://user-images.githubusercontent.com/79210016/174439535-46cae8e5-e8c1-456d-95c1-74de44488c74.png)

basée sur le lemme d'Ito, l'équation peut être écrite comme suit : 

![Capture d’écran 2022-06-18 à 14 18 19](https://user-images.githubusercontent.com/79210016/174439488-552b8582-1945-4924-909b-3e2b2ae1edd6.png)

donc 
![Capture d’écran 2022-06-18 à 14 17 48](https://user-images.githubusercontent.com/79210016/174439491-44a56684-7d87-493c-ba3b-24d7d697d719.png)

pour S(t)>0

![Capture d’écran 2022-06-18 à 14 24 53](https://user-images.githubusercontent.com/79210016/174439734-de217da1-d783-48c7-b7f6-89f62e2f6457.png)

et pour K=1,2,3,...

![Capture d’écran 2022-06-18 à 14 25 00](https://user-images.githubusercontent.com/79210016/174439733-892ec3bb-aca5-4956-8164-040ac824c1cc.png)

et comme $W_t_k - W_t_k-1 
et suit une distebution normale 
alors 

![Capture d’écran 2022-06-18 à 14 25 07](https://user-images.githubusercontent.com/79210016/174439730-72968b39-1cda-45aa-bed5-0e255139ff31.png)


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

### le carré de la serie des rendement 
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
$dS_t=muS_t dt + sigma S_t dW_t$

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

# La prévision 
```R
  #forecasting 
  MAPE(GBM[,2],pfe$pfizer.Close)
```
![Capture d’écran 2022-06-18 à 14 39 34](https://user-images.githubusercontent.com/79210016/174440215-e2b3c27a-f7fd-479b-afc6-6093dfb44642.png)

Le MAPE pour ce modèle s'avère être de 6,876 %. Autrement dit, la différence absolue moyenne entre la valeur prévue et la valeur réelle est de 6,467 %.
ça veut dire qu'on a une prévisions précises

# Simulation de l’ETF ARK Genomic Revolution
```R
arkg= read_excel("ARKG .xlsx",col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric"))
head(arkg)
```
![Capture d’écran 2022-06-18 à 12 05 27](https://user-images.githubusercontent.com/79210016/174435253-a80f36e5-78cc-485c-8abd-4096d8b31220.png)

# la somme pondérée des actifs 
```R
Markg=arkg[,2:11]
Mark=as.matrix(Markg)
Poid=c(0.339,0.925,0.51,0.427,0.427,0.396,0.377,0.364,0.35,0.346)
tra=Mark%*%Poid
head(tra)
```
![Capture d’écran 2022-06-18 à 12 06 55](https://user-images.githubusercontent.com/79210016/174435251-0eb9461f-5429-4615-9a2a-2d501a9d11a6.png)

#  les rendement 
```R
j=1
R=seq((length(tra)-1))
n=length(R)
n
for (i in 1:n){
R[i]= log(tra[j+1])-log(tra[j])
j=j+1
} 
head(R)
```
![Capture d’écran 2022-06-18 à 12 07 31](https://user-images.githubusercontent.com/79210016/174435250-a66e27d9-9e7f-4add-909b-0ea2f1f10aee.png)

```R
summary(R)
```
![Capture d’écran 2022-06-18 à 12 07 57](https://user-images.githubusercontent.com/79210016/174435249-021803c2-5729-4399-9d1c-7290f153b809.png)

# Les estimateurs du maximum de vraisemblence 
```R
# la moyenne des rendement 
Rmoy=mean(R)
Rmoy
#l'écart type
SR=sd(R)
SR2=SR^2
#mu estimé 
m=(abs(Rmoy)/h)+(SR2/2*h)
m
#sigma estimé
sig=SR/sqrt(h)
sig
```
![Capture d’écran 2022-06-18 à 12 08 29](https://user-images.githubusercontent.com/79210016/174435248-79fd3054-7d9f-4911-8874-4a8e42a6e965.png)

# Simulation de la trajectoire moyenne de ETF ARK genomic 
```R
S0 = tra[1]
s = length(tra)
t=1
dt=t/s
RGBM=matrix(ncol = 1, nrow = s)

  RGBM[1,1]= S0
    for (i in 2:s) {
        x=rnorm(1,0,1)
       RGBM[i,1]=RGBM[i-1,1]*exp(((abs(m)-(sig^2)/2)*dt)+(sig*x*sqrt(dt)))

    }
```
![Capture d’écran 2022-06-18 à 13 43 56](https://user-images.githubusercontent.com/79210016/174438467-558f4e24-a686-4198-8a92-ce336066311b.png)

# Le graphe des donnée réelles de l'EFT avec la trajectoire moyenne 
```R
plot(arkg$Date,RGBM,"l",col="#69b3a2",,main="mean path simulated for ETF ARK genomic  ",xlab="days",ylab=" price")
g=data.frame(arkg$Date,RGBM,tra)
require(ggplot2)
z=ggplot(g, aes(arkg$Date) ) +   
    # basic graphical object
  geom_line(aes(y=RGBM), colour="red") +  
  geom_line(aes(y=tra), colour="green")  +
  xlab("days") + 
  ylab("price")
z
```
![Capture d’écran 2022-06-18 à 13 44 08](https://user-images.githubusercontent.com/79210016/174438463-39af4464-debb-4639-a9c5-ec3d74334eb4.png)

# La prévision 
```R
  MAPE(RGBM,tra)
```

![Capture d’écran 2022-06-18 à 14 55 23](https://user-images.githubusercontent.com/79210016/174441685-cb2c4901-8a7c-4801-bf3a-5e2ae3c00ad4.png)


Le MAPE pour ce modèle s'avère être de 22.05%, donc on a une prévision raisonabe 

# Conclusion 
D'une manière générale, la modélisation des prix des actions consiste à modéliser de nouvelles informations sur les actions. Dans cette étude, la modélisation a été réalisée à travers la quantification d'une part stochastique dans l'expression générale du modèle. Les résultats de la simulation réalisée dans cette étude ne correspondent pas toujours à ceux du modèle théorique même si les hypothèses sur lesquelles repose le modèle respectent les règles des marchés financiers.

