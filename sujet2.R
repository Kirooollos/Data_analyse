#Exercice 2

diamants <- read.table("diamantsPurs.csv", header=T)
x<- diamants$price[diamants$cut == "Ideal"]

nx<- length(x)
mx<-mean(x)
vex<-var(x)     #variance empirque corrigee
vx<-(nx-1)/nx*vex  #varaince empique
sdex<-sd(x)
min<-min(x)
min<-quantile(x,prob=0,name=F)   #byedo nafs le resultas
max<-quantile(x,prob=1, name=F)

#pour calculer les quantiles
q1<-quantile(x,prob=0.25,name=F)
q2<-quantile(x,prob=0.5,name=F)       #la mediane
q3<-quantile(x,prob=0.75,name=F)


#EXERCICE 2

curve(tan, from =0.0, to=1.55, xlim = c(0,2*pi), ylim=c(-6,6),
      main="fonction tangeante", xlab="x", ylab="tg(x)")

#xlim et ylim :pour specifier les limites des axes des abscices et des ordonnées 


# Tracer la fonction tangente sur l'intervalle ]0, 2pi[
curve(tan, from = 0, to = 2*pi,n=1000, col = "blue", xlab = "x", ylab = "tan(x)", main = "Tangente sur l'intervalle ]0, 2pi[")


""" tan :est la fonction tangente de base.
from :spécifie le début de l'intervalle, ici 0.
to: spécifie la fin de l'intervalle, ici 2π.
n :spécifie le nombre de points à échantillonner pour tracer la courbe.
col :définit la couleur de la courbe.
xlab et ylab définissent les étiquettes des axes x et y respectivement.
main :définit le titre du graphique.
""" 

"""Tracer de la fonction tangeante sur ]0, 2π[/{π/2, 3π/2} en noir, 
avec titres et legendes et ajout des
axes en noir et des asymptotes en rouge. 
Imposer les limites [0, 2 ∗ pi] en abscisses et [−6, 6] en ordonnees.
"""

# Tracer la fonction tangente sur l'intervalle ]0, 2*pi[ avec exclusions
curve(tan(x), from = 0, to = 2*pi, xlim = c(0, 2*pi), ylim = c(-6, 6), col = "black", xlab = "x", ylab = "tan(x)", main = "Fonction tangente sur ]0, 2*pi[", axes = FALSE)
# Ajouter les axes
abline(h = 0, v = seq(0, 2*pi, by = pi/2), col = "black")
# Ajouter les asymptotes en rouge
abline(v = seq(0, 2*pi, by = pi), col = "red", lty = 2)
# Ajouter une légende
legend("topleft", legend = c("tangente", "asymptotes"), col = c("black", "red"), lty = c(1, 2))


#correction

curve(tan, from =0.0, to=1.55, xlim = c(0,2*pi), ylim=c(-6,6),
      main="fonction tangeante", xlab="x", ylab="tg(x)")
curve(tan, from =1.64, to=4.65, add=T)              #on ajoute l'option add=T parceque on superpose plusieurs curves
curve(tan, from =4.75, to=6.2, add=T)
abline(v=0,h=0)                                       #ajout des axes
abline(v=pi/2,col=2);abline(v=3*pi/2,col=2)           #ajout des asymptotes
abline(h=5,lty=2,col=3)                         #aide à la lecture des antécédants de 5
abline(v=atan(5),lty=2,col=3)                   #aide à la lecture des antécédants de 5
abline (v=atan(5)+pi,lty=2,col=3)               #aide à la lecture des antécédants de 5

#abline : a, b : Ces arguments permettent de tracer une ligne diagonale d'équation y=ax+b.


# Tracer une ligne diagonale avec une pente de 1 et une ordonnée à l'origine de 0
abline(a = 0.6, b = 1.9, col = "red")









