rm(list=ls()) 

ech1=rnorm(3,m=10,sd=1)
ech2=rnorm(5,m=10,sd=1)
ech3=rnorm(10,m=10,sd=1)

moy1=mean(ech1)
moy2=mean(ech2)
moy3=mean(ech3)

#-------------------------------------------------------------------------------

mu1=9
mu2=12
listT<-c()
for(n in 1:10000){
  a<-rnorm(10,m=mu1,sd=1)
  b<-rnorm(10,m=mu2,sd=1)
  T=(mean(b)-mean(a)-(mu2-mu1))/sqrt((var(a)/10)+(var(b)/10))
  listT[n]<-T
}

hist(listT,breaks=80,col='grey',main='T\' selon la loi de student',freq=F)
curve(dt(x,18),add=TRUE,col='blue',lwd=2)


#-------------------------------------------------------------------------------

Table<-read.table("/home/acathignol/R/Stat_SamMeyer/ecart.txt")
plot(Table$V2~Table$V1,ylab='Ecart',xlab='T1 et T2')

echT1<-Table$V2[Table$V1=="T1"]
echT2<-Table$V2[Table$V1=="T2"]

m=8

#si on suppose que les variances sont égales
T=(mean(echT2)-mean(echT1)-8)/sqrt((var(echT1)*2/10))
t.test(echT1,echT2,mu=-8,var.equal = TRUE)

#si on suppose que les variances ne sont pas égales
Tp=(mean(echT2)-mean(echT1)-8)/sqrt((var(echT1)/10)+(var(echT2)/10))
t.test(echT1,echT2,mu=-8,var.equal = FALSE)

print(T)
print(Tp)
