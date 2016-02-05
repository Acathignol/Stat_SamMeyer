rm(list=ls()) 
#===============================================================
#                              TD2-EXO2
#===============================================================


#======================== 1) ===================================

Var3<-c()
Var5<-c()
Var10<-c()
Var20<-c()


for (i in 1:5){
  ech1=rnorm(n=3,m=0,sd=2)
  ech2=rnorm(n=5,m=0,sd=2)
  ech3=rnorm(n=10,m=0,sd=2)
  ech4=rnorm(n=20,m=0,sd=2)
  
  Var3[i]=var(ech1)
  Var5[i]=var(ech2)
  Var10[i]=var(ech3)
  Var20[i]=var(ech4)
}


par(mfrow = c(2,2))
hist(Var3,breaks=15,col="grey")
hist(Var5,breaks=15,col="grey")
hist(Var10,breaks=15,col="grey")
hist(Var20,breaks=15,col="grey")

#plus la taille de notre échantilon est grande, plus la 
#répartition se décale vers la droite --la loi du chi 2 n-1

#======================== 2) ===================================
rm(list=ls()) 

Var3<-c()
Var5<-c()
Var10<-c()
Var20<-c()

vartheo=4

for (i in 1:1000){
  ech1=rnorm(n=3,m=0,sd=2)
  ech2=rnorm(n=5,m=0,sd=2)
  ech3=rnorm(n=10,m=0,sd=2)
  ech4=rnorm(n=20,m=0,sd=2)
  
  Var3[i]=var(ech1)/vartheo
  Var5[i]=var(ech2)/vartheo #(n-1)*var(ech)/var(théoriq)
  Var10[i]=var(ech3)/vartheo
  Var20[i]=var(ech4)/vartheo
}

x<-seq(0,100,0.1)

par(mfrow = c(2,2))
hist(Var3,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*2,2)*2,col="red",lwd=2)

hist(Var5,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*4,4)*4,col="red",lwd=2)

hist(Var10,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*9,9)*9,col="red",lwd=2)

hist(Var20,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*19,19)*19,col="red",lwd=2)

#suit la loi du chi 2 n-1
#la courbe devient bilatérale
#=> var converge plus vite vers valeur théo?


#======================== 3) ===================================
#si 1 courbe centrée en 1 mais commet une erreur 
#=> prob d'être dans une borne.?????
#=> pour dire si précis ou pas

#courbe symétrique pas bilatérale mais par quotient!
#=>si 1centre choose 1.05 droite et 1/1.05 gauche

hist(Var3,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*2,2)*2,col="black",lwd=1)
abline(v=1.05,col="orange",lwd=2)
abline(v=1/1.05,col="orange",lwd=2)

hist(Var5,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*4,4)*4,col="black",lwd=1)
abline(v=1.05,col="orange",lwd=2)
abline(v=1/1.05,col="orange",lwd=2)

hist(Var10,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*9,9)*9,col="black",lwd=1)
abline(v=1.05,col="orange",lwd=2)
abline(v=1/1.05,col="orange",lwd=2)

hist(Var20,breaks=30,col="grey",freq=F)
lines(x,dchisq(x*19,19)*19,col="black",lwd=1)
abline(v=1.05,col="orange",lwd=2)
abline(v=1/1.05,col="orange",lwd=2)

x11()
par(mfrow = c(1,2))

p1000<-c()
for (n in 1:1000){
  q=(n-1)
  p1000[n]=pchisq(q/(1.05), df=(n-1))+1-pchisq(q*(1.05), df=(n-1))
}    

mu=0
pm1000<-c()
for (n in 1:1000){
  pm1000[n]=2*pnorm(-0.1,0,1/sqrt(n))
}

n=c(1:1000)
plot(n,p1000,type="l",main="Convergence")#de var à5% env
lines(n,pm1000,col="green",lwd=2)#de mean à sd*0.1
plot(n,p1000,type="l",main="Echelle Log de la convergence",log="y")#de var à5% env
lines(n,pm1000,col="green",lwd=2,log="y")#de mean à sd*0.1
legend("topright", legend=c("variance","mean"), col = c("black","green"),
       border = "black", lty=c(1,1), lwd=c(1,2))
#proportions des estimations à l'intérieur des seuils!!!!



