rm(list=ls()) 

a<-c(0.9,10.1,1.9,-0.9,12.6,6.1,6.8,4.0,6.8,-7.3)
b<-c(18.0,25.1,1.2,14.6,-3.4,-1.2,24.1,15.5,3.7,5.1)

va=var(a)
vb=var(b)

na=10
nb=10

# interval confiance pour ça !!!! : 

q=na-1
X1=qchisq(0.025, df=q)
X2=qchisq(0.975, df=q)
SigmaA1=(1/X1)*(va)*q
SigmaA2=(1/X2)*(va)*q

q=nb-1
X1=qchisq(0.025, df=q)
X2=qchisq(0.975, df=q)
SigmaB1=(1/X1)*(vb)*q
SigmaB2=(1/X2)*(vb)*q

Ftest=va/vb
#Proviennent ils de même variance? H0 : YES
pval=2*pf(Ftest,na-1,nb-1) #0.088 => pour alpha (risque) =0.05 => on ne peut pas rejeter H0


var.test(a,b) #!!!!!!!!!!!!!!!!!!

