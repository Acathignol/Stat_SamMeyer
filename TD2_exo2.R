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
  Var5[i]=var(ech2)/vartheo
  Var10[i]=var(ech3)/vartheo
  Var20[i]=var(ech4)/vartheo
}

x0<-seq(from = 0.1, to = 6, length = 200)

par(mfrow = c(2,2))
hist(Var3,breaks=15,col="grey")
lines(x0,dchisq(x0,2),col="red",lwd=2)
hist(Var5,breaks=15,col="grey")
lines(x0,dchisq(x0,4),col="red",lwd=2)
hist(Var10,breaks=15,col="grey")
lines(x0,dchisq(x0,9),col="red",lwd=2)
hist(Var20,breaks=15,col="grey")
lines(x0,dchisq(x0,19),col="red",lwd=2)

#suit la loi du chi 2 n-1

