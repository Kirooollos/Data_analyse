data <- read.table(file = "apnee.csv", sep=",", header =T)
# Définir les paramètres
p <- 0.3
n <- 500

# Générer un échantillon de taille n pour une variable de Bernoulli B(p)
echantillon <- rbinom(n, size = 1, prob = p)


x<- cumsum(echantillon)/1:n

#prop.test(x,n,conf.level= 0.98)$conf.int
#ca donne un intervalle de confiance d'une proportion 

#on utilise pnorm pour calculer les proba
#qnorm pour calculer les quantiles +sora


#Exercice 2

poidsH <-data[data$sexe==0,"poids"]

moyenne <- mean(poidsH)
variance<- var(poidsH)
ecart_type <- sqrt(variance)


var_Connu <- 19
niveau_confiance <- 0.95
# Quantile de la distribution normale standard
Z <- qnorm((1 + niveau_confiance) / 2)

# Calcul des bornes de l'intervalle de confiance
borne_inf <- moyenne - Z * (ecart_type / sqrt(n))
borne_sup <- moyenne + Z * (ecart_type / sqrt(n))



t.test(poidsH, conf.level = 0.99)


qqnorm(poidsH)
qqline(poidsH)


#les quantiles empiriques en fonctions des quantiles theoriques

hist(poidsH)

#pour avoir les quantiles d'une loi khi-deux

t.test(poidsH,conf.level = 0.98)$conf.int
#qchisq(alpha/2; df = n-1)

#qt (pour la loi de student)




#TP8  #Exerice 2
tailleH <- data[data$sexe == 0, "taille"]

n <- length(tailleH)

moy <- mean(tailleH)
ecarttype <- sd(tailleH)

mu0 <- 178
tcalc <- (moy - mu0) * sqrt(n) / ecarttype

alpha <- seq(0.01, 0.1, 0.01)
t <- qt(1 - alpha, n - 1)
alpha[tcalc > t]

t.test(tailleH, mo = mu0, alternative = "greater")


#et donc on rejete H0 comme p-valeur < alpha pour ces valeurs 
# 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10



#pour calculer les p-valuers 
#moyenne pn a t.test
#proba prop.test


#EX2 TP9


tailleH <- data[data$sexe == 0, "taille"]

variance_nb <- var(tailleH)

n <- length(tailleH)

valeur_var <- 200

tcalc <- ((n-1)*(variance_nb))  / valeur_var

alpha<- seq(0.01, 0.9, 0.01)

t<- qchisq(alpha/2, n-1)

t1a <- qchisq(1-(alpha/2),n-1)

alpha[tcalc<t | tcalc > t1a]


#0.73 0.74 0.75 0.76 0.77 0.78 0.79 0.80 0.81 0.82 0.83 0.84 0.85 0.86 0.87 0.88 0.89 0.90
#pour ces valeurs on rejette H0

#sowar ketir

prop.test(sum(taille>80), length (taille), 0.15,1-alpha)






#TP10 
data<-read.table("her.txt",header = TRUE, sep = "\t")


#on peut faire avec 2 methodes:
#1- calculer tobs qui suit une loi particuliere
#2-  calculer avec les p-valeurs


sys<-data$sys
dia<- data$dia

#quand ca provient des memes individus c'est apparies 
diff <- sys - dia

#pour verifier si ca suit la loi normale : qqnorm(D) qqline (D)   chapiro.test(D)

variance <- var(diff)
moyenne <- mean(diff)

n<- length(diff)

tcalc<- sqrt(n)* (moyenne/variance)

alpha <- 0.05

t<- qt(alpha/2,n-1)

test<- tcalc < t | tcalc >-t

t.test(diff, mu=0 ,alt="two.sided")

#ex1-6 
t.test(diff, mu-40, alt="greater")
#ou 
t.test(sys, dia, mu=40, alt="greater")



#test sur la variance
varsys <- var(sys)
vardia <-  var(dia)

n1<- length(sys)
n2<- length(dia)

tcalc2<- (varsys^2)/(vardia^2)

alpha2 <- 0.05

t<- qf(alpha/2, df1= n1-1 , df2= n2-1)


  
  
  
#exercice 2 on ait ce type de test quand on a 2 populations independants
  
#il faut  ait varx=vary

  




















































