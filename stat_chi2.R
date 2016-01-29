rm(list=ls()) 

ech<-c(13.9,11.6,12.4,13.4,13.5,13.7,10.8,12.6)

s2=var(ech)

n=8
s0=2

N1chi2=((n-1)*s2)/(s0^2)
df=n-1

pvalue=2*pchisq(N1chi2,df=7)

#test bilatéral: en effet variance peut devenir + grande ou plus ptite
x=seq(1,9,.1)
plot(x,dchisq(x,df=7),type="l",ylab="proba")
abline(v=N1chi2,col=2)

#pvalue => 0.09 => alpha 0.05 => on ne peut pas rejeter H0 : sigma=sigma0