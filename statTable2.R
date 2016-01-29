rm(list=ls()) #echantillons (2) de var inconnues !!!!

Table<-read.table("/home/acathignol/R/Stat_SamMeyer/moy.dat")
plot(Table$V2~Table$V1)

echTrait<-Table$V2[Table$V1=="trait"]
echTem<-Table$V2[Table$V1=="tem"]

moyTrait<-mean(echTrait)
moyTem<-mean(echTem)

s1=sqrt(var(echTrait))
s2=sqrt(var(echTem))

n1=10
n2=10

alpha=0.05

t=(moyTrait-MU1)/(s1/sqrt(10))

QTrait1 =moyTrait+(X/sqrt(10))*qnorm(alpha/2) #intervalle de confiance à 95%
QTrait2 =moyTrait+(X/sqrt(10))*qnorm(1-alpha/2) #intervalle de confiance à 95%

QTem1 =  moyTem+(X/sqrt(10))*qnorm(alpha/2) #intervalle de confiance à 95%
QTem2 =  moyTem+(X/sqrt(10))*qnorm(1-alpha/2) #intervalle de confiance à 95%


dm=moyTrait-moyTem
S2=((10-1)*s1)+(10-1)*s2)/(10+10-2)
T=(dm-0)/(sqrt(S2*(1/10+1/10)))

x=seq(-3,3,.1)
plot(x,dnorm(x,m=0,sd=sd),type='l',ylab='prob',xlab='m')
abline(v=dm,col=4,lw=2)
2*(1-pnorm(dm,sd=sd))




#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------





rm(list=ls()) #echantillons (2) de var inconnues !!!!

Table<-read.table("/home/acathignol/R/Stat_SamMeyer/moy.dat")
plot(Table$V2~Table$V1)

echTrait<-Table$V2[Table$V1=="trait"]
echTem<-Table$V2[Table$V1=="tem"]

alpha=0.05


t.test(x=echTrait, y=echTem, var.equal = FALSE,
       conf.level = 0.95)

