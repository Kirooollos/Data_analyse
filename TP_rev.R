#TP8

#EXERCICE 2 (test de moyenne)

#1- charger les donnees
data <- read.table(file = "apnee.csv", sep=",", header =T)

#2-Extraire l'echantillon des mesures de la variable taille chez les hommes
tailleH <- data[data$sexe == 0, "taille"]

#3- Cette Varibale suit la loi normale, 
# Calculer l'estimation sans biais de la moyenne et de la variance
mu<- mean(tailleH)
sigma<- sd(tailleH)

mu0<-178
n <- length(tailleH)

tcalc <- (mu-mu0)/(sigma/sqrt(taille))

alpha<- seq(0.01, 0.1, 0.01)

#calcluer les quantiles de distrubition de la loi Student a chaque alpha
t<-qt(1-alpha, n-1)
#test Unilateral superieur : on rejette H0 si tcalc > t
alpha[tcalc > t]   

#Pour ces valeurs de alpha On a donc un changement de decision et on rejette H0
#0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10

#Calcule de p-valeur
t.test(tailleH, mo = mu0, alternative = "greater")

#p-value < 2.2e-16 donc comme p-valeur < alpha ainsi on rejette H0
#Et donc on rejete H0 comme p-valeur < alpha pour ces valeurs de alpha
# 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10





#TP9 

#Exerice 2
#On considere les meme donnees , mais on fait ici un test sur les variances
#H0:var=200   H1:var !=  200      test bilateral

variance <-var(tailleH)

valeur_var<- 200

tobs<-(n-1)*variance/valeur_var

alpha<- seq(0.01, 0.9, 0.01)

#calculer les quantiles pour la loi de khi-deux

q1a<- qchisq(alpha/2, n-1)
q1a2<- qchisq(1-(alpha/2), n-1)


#test biilateral: on rejette H0 si tcalc < q1a ou tcalc > q1a2
alpha[tobs < q1a | tobs > q1a2]

#0.73 0.74 0.75 0.76 0.77 0.78 0.79 0.80 0.81 0.82 0.83 0.84 0.85 0.86 0.87 0.88 0.89 0.90
#Pour ces valeurs de alpha on rejette H0

#pour calculer p-valeur

# Statistique de test observée :tobs
# Degré de liberté
df <- n-1

# Calcul de la p-valeur pour un test unilatéral supérieur
p_valeur_unilaterale_superieure <- pchisq(tobs, df)

# Calcul de la p-valeur pour un test unilatéral inférieur
p_valeur_unilaterale_inferieure <- 1 - pchisq(tobs, df)

# Calcul de la p-valeur pour un test bilatéral
p_valeur_bilaterale <- 2 * min(p_valeur_unilaterale_superieure, p_valeur_unilaterale_inferieure)



#TP10 

data2<-read.table("her.txt",header = TRUE, sep = "\t")

sys<- data2$sys
dia<-data2$dia

diff<- sys - dia

#Quand ca provient des memes individus donc c'est apparies (ici oui ca provient des memes individus)

#Pour verifier ci ca suit ou non  la loi normale:
qqnorm(diff)
qqline(diff)
shapiro.test(diff)















