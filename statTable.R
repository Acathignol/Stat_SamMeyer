rm(list=ls()) #echantillons (2) de var connues !!!!

Table<-read.table("/home/acathignol/R/Stat_SamMeyer/moy.dat")
plot(Table$V2~Table$V1)

echTrait<-Table$V2[Table$V1=="trait"]
echTem<-Table$V2[Table$V1=="tem"]

moyTrait<-mean(echTrait)
moyTem<-mean(echTem)

etTrait=sqrt(var(echTrait))
etTrait=sqrt(var(echTem))

alpha=0.05

QTrait1 =moyTrait+(2/sqrt(10))*qnorm(alpha/2) #intervalle de confiance à 95%
QTrait2 =moyTrait+(2/sqrt(10))*qnorm(1-alpha/2) #intervalle de confiance à 95%
  
QTem1 =  moyTem+(2/sqrt(10))*qnorm(alpha/2) #intervalle de confiance à 95%
QTem2 =  moyTem+(2/sqrt(10))*qnorm(1-alpha/2) #intervalle de confiance à 95%

dm=moyTrait-moyTem
sd=sqrt(2*4/10)
x=seq(-3,3,.1)
plot(x,dnorm(x,m=0,sd=sd),type='l',ylab='prob',xlab='m')
abline(v=dm,col=4,lw=2)
2*(1-pnorm(dm,sd=sd))