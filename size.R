##Size of Test for Non Parametric Exact Distribution :  


alpha=0.4
#NORMAL
f_norm=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(wilcox.test(rnorm(n1),rnorm(n2),alternative ="t",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(wilcox.test(runif(n1),runif(n2),alternative ="t",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(wilcox.test(rcauchy(n1),rcauchy(n2),alternative ="t",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(wilcox.test(rexp(n1),rexp(n2),alternative ="t",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}
#par(mfrow=c(2,1))
matplot(total,M2,main="Mann-Whitney U Test",xlab="Total Sample Size",ylab="Estimate of level",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.4")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


_______________________________________________________________________________________________________________________

#Size Of Test for Non Parametric Asymptotic Distribution :

#alternative="less"
#theta >0

alpha=0.01
M=5000
sample_size=matrix(c(2,3,3,5,5,7,6,10,15,20,25,30,40,50,60,70,70,75,90,95),nrow=10,ncol=2,byrow=T)

#NORMAL
f_norm=array(10)

f=function(n1,n2)
{
  t=replicate(M,(wilcox.test(rnorm(n1),rnorm(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[-(t-mean)/sd>qnorm(1-alpha)])/M)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(runif(n1),runif(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[-(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(rcauchy(n1),rcauchy(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[-(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(rexp(n1),rexp(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[-(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}

matplot(total,M2,main="Mann-Whitney U Test",xlab="Total Sample Size",ylab="Estimate of level",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


________________________________________________________

#alternate="greater"
#theta<0


alpha=0.05
M=5000
sample_size=matrix(c(2,3,3,5,5,7,6,10,15,20,25,30,40,50,60,70,70,75,90,95),nrow=10,ncol=2,byrow=T)

#NORMAL
f_norm=array(10)

f=function(n1,n2)
{
  t=replicate(M,(wilcox.test(rnorm(n1),rnorm(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(runif(n1),runif(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(rcauchy(n1),rcauchy(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(rexp(n1),rexp(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}

matplot(total,M2,main="Mann-Whitney U Test",xlab="Total Sample Size",ylab="Estimate of level",ylim=c(0,2*alpha),lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.05")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


______________________________________________________________________



#alternative ="two sided"

alpha=0.001
M=5000
sample_size=matrix(c(2,3,3,5,5,7,6,10,15,20,25,30,40,50,60,70,70,75,90,95),nrow=10,ncol=2,byrow=T)

#NORMAL
f_norm=array(10)

f=function(n1,n2)
{
  t=replicate(M,(wilcox.test(rnorm(n1),rnorm(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t-mean)/sd>qnorm(1-alpha/2)])/M)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(runif(n1),runif(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t-mean)/sd>qnorm(1-alpha/2)])/M)
}

for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(rcauchy(n1),rcauchy(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t-mean)/sd>qnorm(1-alpha/2)])/M)
}

for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(wilcox.test(rexp(n1),rexp(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t-mean)/sd>qnorm(1-alpha/2)])/M)
}

for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}

matplot(total,M2,main="Mann-Whitney U Test",xlab="Total Sample Size",ylab="Estimate of level",,lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.001")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


_____________________________________________________________________________________________________________________________________________

##Size of Test for Parametric Exact Distribution :  

alpha=0.2
#NORMAL
f_norm=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(t.test(rnorm(n1),rnorm(n2),alternative ="g",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(t.test(runif(n1),runif(n2),alternative ="g",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(t.test(rcauchy(n1),rcauchy(n2),alternative ="g",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)
sample_size=matrix(c(5,10,15,20,25,30,40,50,50,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)

f=function(n1,n2)
{
  
  p=replicate(10000,(t.test(rexp(n1),rexp(n2),alternative ="g",conf.level=1-alpha))$p.value)
  return(length(p[p<alpha])/10000)
}
for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}
#par(mfrow=c(2,1))
matplot(total,M2,main="Parametric t Test",xlab="Total Sample Size",ylab="Estimate of level",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.2")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


_________________________________________________________________________________________________________________________________________________



#Size Of Test for Parametric Asymptotic Distribution :

#first alternative

alpha=0.3
M=5000
sample_size=matrix(c(2,3,3,5,5,7,6,10,15,20,25,30,40,50,60,70,70,75,90,95),nrow=10,ncol=2,byrow=T)

#NORMAL
f_norm=array(10)

f=function(n1,n2)
{
  t=replicate(M,(t.test(rnorm(n1),rnorm(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=0
  sd=((n1+n2-2)/((n1+n2-2)-2))^0.5
  return(length(t[((t-mean)/sd)>qnorm(1-alpha)])/M)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(runif(n1),runif(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=0
  sd=((n1+n2-2)/((n1+n2-2)-2))^0.5
  return(length(t[((t-mean)/sd)>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(rcauchy(n1),rcauchy(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=0
  sd=((n1+n2-2)/((n1+n2-2)-2))^0.5
  return(length(t[((t-mean)/sd)>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(rexp(n1),rexp(n2),alternative ="g",conf.level=1-alpha))$statistic)
  mean=0
  sd=((n1+n2-2)/((n1+n2-2)-2))^0.5
  return(length(t[((t-mean)/sd)>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}


matplot(total,M2,main="Standardised t-Statistic",xlab="Total Sample Size",ylab="Estimate of level",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.3")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)

________________________________________________________

#second alternative


alpha=0.05
M=5000
sample_size=matrix(c(2,3,3,5,5,7,6,10,15,20,25,30,40,50,60,70,70,75,90,95),nrow=10,ncol=2,byrow=T)

#NORMAL
f_norm=array(10)

f=function(n1,n2)
{
  t=replicate(M,(t.test(rnorm(n1),rnorm(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(runif(n1),runif(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
f=function(n1,n2)
{
  
  t=replicate(M,(t.test(rcauchy(n1),rcauchy(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(rexp(n1),rexp(n2),alternative ="l",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[(t-mean)/sd>qnorm(1-alpha)])/M)
}

for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}

matplot(total,M2,main="Mann-Whitney U Test",xlab="Total Sample Size",ylab="Estimate of level",ylim=c(0,2*alpha),lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.05")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


______________________________________________________________________



#alternative ="two.sided"

alpha=0.001
M=5000
sample_size=matrix(c(2,3,3,5,5,7,6,10,15,20,25,30,40,50,60,70,70,75,90,95),nrow=10,ncol=2,byrow=T)

#NORMAL
f_norm=array(10)

f=function(n1,n2)
{
  t=replicate(M,(t.test(rnorm(n1),rnorm(n2),alternative ="t",conf.level=1-alpha))$statistic)
  return(length(t[abs(t)>qnorm(1-alpha/2)])/M)
}
for(i in 1:10)
{
  f_norm[i]=f(sample_size[i,1],sample_size[i,2])
}
#UNIFORM
f_unif=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(runif(n1),runif(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t)>qnorm(1-alpha/2)])/M)
}

for(i in 1:10)
{
  f_unif[i]=f(sample_size[i,1],sample_size[i,2])
}
#CAUCHY
f_cauchy=array(10)
f=function(n1,n2)
{
  
  t=replicate(M,(t.test(rcauchy(n1),rcauchy(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t-mean)/sd>qnorm(1-alpha/2)])/M)
}

for(i in 1:10)
{
  f_cauchy[i]=f(sample_size[i,1],sample_size[i,2])
}
#EXPONENTIAL
f_exp=array(10)

f=function(n1,n2)
{
  
  t=replicate(M,(t.test(rexp(n1),rexp(n2),alternative ="t",conf.level=1-alpha))$statistic)
  mean=n1*n2/2
  sd=(n1*n2*(n1+n2+1)/12)^0.5
  return(length(t[abs(t-mean)/sd>qnorm(1-alpha/2)])/M)
}

for(i in 1:10)
{
  f_exp[i]=f(sample_size[i,1],sample_size[i,2])
}
M2=matrix(c(f_norm,f_unif,f_cauchy,f_exp),ncol=4)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}

matplot(total,M2,main="Mann-Whitney U Test",xlab="Total Sample Size",ylab="Estimate of level",,lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.001")
abline(h=alpha)
legend("topright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)

