
ech=rnorm(10,m=10.5,sd=1)
moy=mean(ech)
pval=2*(1-pnorm(moy,10,1/sqrt(10)))
print(moy)
print(pval)
'qbinom(0.025,100,7/20) qbinom(0.975,100,7/20)'

ECH2<-c(10.8,11.6,9.4,10.8,11.3,9.7,11.7,9.2,10.9,10.8)
m=mean(ECH2)
n=10
sd=var(ECH2)

'a<-c()
for(n in 1:10){
  x<-rnorm(10,m=10,sd=1)
  (10-mean(x)/(sd/sqrt(10))
   
}'
