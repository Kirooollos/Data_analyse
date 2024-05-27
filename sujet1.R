#EXERCICE 1

mtcars<- mtcars
x<-mtcars$disp[mtcars$am == 0 ]    #les vehicules automatiques
y<-mtcars$disp[mtcars$am ==1 ]   #les vehicules manuelles

#pour calculer la taille on utilise length
nx<- length(x)
ny<-length(y)

#pour calculer les moyennes on utilise mean
mx<- mean(x)
my<-mean(y)

#pour calculer la variance empirique corrigee on utilise var
vex<- var(x)
vey<-var(y)
#pour calculer la variance empirque
vx<-(nx-1)/nx*vex
vy<-(ny-1)/ny*vey

#pour calculer l'ecart-type on utilise sd
sdex<-sd(x)
sdey<-sd(y)

#pour construire un tableau
data.frame("boite automatique" =c("taille","moyenne","variance empirique",
                                  "variance empirique corrigée", "ecart-type emp. corrigé"),
           valeurs_x=c(nx,mx,vx,vex,sdex),
           "boite manuelle" =c("taille","moyenne","variance empirique",
                               "variance empirique corrigée", "ecart-type emp. corrigé"),
           valeurs_y=c(ny,my,vy,vey,sdey))->d
print(d,digits=2)


#Commentaire : la variable disp est en moyenne presque deux fois plus élevée 
#pour les véhicules automatiques que pour les véhicules manuels.
#Ces derniers sont donc beaucoup moins puissants en moyenne.
#Par ailleurs les indices de dispersion étant autour de la centaine et comparables,
#l’écart observé sur les moyennes est assez significatif.

#EXERCICE 2
table<-table(x)
table
#1- La variable disp est continue.
#En effet son tableau en effectifs obtenu avec la fonction table montre très peu 
#de répétitions de mêmes valeurs dans l’échantillon.
#2-On représente donc sa répartition observée avec un histogramme en densité.
hist(x,probability = T, main="displacement pour les véhicules automatiques",
     xlab="disp",ylab="densité")
curve(dnorm(x,mean(x),sd(x)),add=T,col=2)  #ajouter la densitee d'une variable normale
points(mean(x),0,col="green",pch=20)        #ajuter des points
















