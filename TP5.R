cardiaque <- read.table(file = "cardiaque.csv", sep=",", header =T)
#un échantillon de 100 valeurs distribuées selon une loi de Bernoulli 
p <- 0.45
x<-  rbinom(100,1,p)
suitemoy <- cumsum(x)/(1:100)
plot(suitemoy)
abline(h=p,col="red")
s<-cardiaque$systolique
BMI<- cardiaque$BMI

sb23 <- s[BMI >= 23]
hist(sb23, probability = TRUE)
moyenne<- mean(sb23)
ecart<- sd(sb23)
curve(dnorm(x,moyenne, ecart), add=TRUE)

S_fumeurs <- s[cardiaque$tabacBinaire == 1]
s_non_fum <- s[cardiaque$tabacBinaire == 0]
boxplot(S_fumeurs,s_non_fum)

#La fonction dnorm() est une fonction de probabilité dans R 
#qui calcule les valeurs de la fonction de densité de probabilité
#d'une distribution normale pour un ensemble donné de points


# Exemple d'utilisation de dnorm()
x <- seq(-3, 3, by = 0.1)  # Générer une séquence de points
pdf <- dnorm(x, mean = 0, sd = 1)  # Calculer les valeurs de la PDF pour ces points



