Distribution free
------------------------
non Parametric part
-------------------------
Histogram
------------------------
set.seed(123)
library(lattice)
n=40
m=47
X=matrix(0,ncol=4,nrow=1000)
X[,1]=replicate(1000,wilcox.test(rnorm(n),rnorm(m))$statistic)
X[,2]=replicate(1000,wilcox.test(runif(n),runif(m))$statistic)
X[,3]=replicate(1000,wilcox.test(rcauchy(n),rcauchy(m))$statistic)
X[,4]=replicate(1000,wilcox.test(rexp(n),rexp(m))$statistic)
par(mfrow=c(2,2))
dist=c("normal","uniform","cauchy","exponential")
data=data.frame(X)
colnames(data)=dist
histogram(~cauchy+exponential+normal+uniform,xlab="",data=data,type="density")
----------------
Kolgomorov
----------------
K=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K[i,j]=(ks.test(X[,i],X[,j]))$p.value
}
}
K1=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K1[i,j]=(ks.test(X[,i],X[,j],alternative="greater"))$p.value
}
}
K2=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K2[i,j]=(ks.test(X[,i],X[,j],alternative="l"))$p.value
}
}
-------------------
parametric part
-------------------
n=10
m=15
X=matrix(0,ncol=4,nrow=1000)
X[,1]=replicate(1000,t.test(rnorm(n),rnorm(m))$statistic)
X[,2]=replicate(1000,t.test(runif(n),runif(m))$statistic)
X[,3]=replicate(1000,t.test(rcauchy(n),rcauchy(m))$statistic)
X[,4]=replicate(1000,t.test(rexp(n),rexp(m))$statistic)
dist=c("normal","uniform","cauchy","exponential")
data=data.frame(X)
colnames(data)=dist
histogram(~cauchy+exponential+normal+uniform,xlab="",data=data,type="density")
-----------------
Kolgomorov
----------------
K=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K[i,j]=(ks.test(X[,i],X[,j]))$p.value
}
}
K


K1=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K1[i,j]=(ks.test(X[,i],X[,j],alternative="greater"))$p.value
}
}
K1


K2=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K2[i,j]=(ks.test(X[,i],X[,j],alternative="l"))$p.value
}
}
K2
-------------------------
non parametric limiting
-------------------------
X=matrix(0,ncol=4,nrow=1000)
n1=30
n2=30
mean=n1*n2/2
sd=(n1*n2*(n1+n2+1)/12)^0.5

X[,1]=replicate(1000,wilcox.test(rnorm(n1),rnorm(n2))$statistic)
X[,2]=replicate(1000,wilcox.test(runif(n1),runif(n2))$statistic)
X[,3]=replicate(1000,wilcox.test(rcauchy(n1),rcauchy(n2))$statistic)
X[,4]=replicate(1000,wilcox.test(rexp(n1),rexp(n2))$statistic)
par(mfrow=c(2,2))
dist=c("normal","uniform","cauchy","exponential")
for(i in 1:4)
{X[,i]=(X[,i]-mean)/sd
}
data=data.frame(X)
colnames(data)=dist
histogram(~cauchy+exponential+normal+uniform,xlab="",data=data,type="density")
curve(dnorm(x), from=-4, to=4, col='blue')

y=array(0)
for(i in 1:4)
{
y[i]=shapiro.test((X[,i]-mean)/sd)$p.value
}
y
---------------
Kolgomorov
----------------
K=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K[i,j]=(ks.test((X[,i]-mean)/sd,(X[,j]-mean)/sd))$p.value
}
}
K
-----------------
Parametric limiting distribution
-----------------------------------
X=matrix(0,ncol=4,nrow=1000)
n1=10
n2=15
mean=0
sd=((n1+n2-2)/((n1+n2-2)-2))^0.5
X[,1]=replicate(1000,t.test(rnorm(10),rnorm(15))$statistic)
X[,2]=replicate(1000,t.test(runif(10),runif(15))$statistic)
X[,3]=replicate(1000,t.test(rcauchy(10),rcauchy(15))$statistic)
X[,4]=replicate(1000,t.test(rexp(10),rexp(15))$statistic)
par(mfrow=c(2,2))
dist=c("normal","uniform","cauchy","exponential")
for(i in 1:4)
{X[,i]=((X[,i]-mean)/sd)}
data=data.frame(X)
colnames(data)=dist
histogram(~cauchy+exponential+normal+uniform,xlab="",data=data,type="density")

y=array(0)
for(i in 1:4)
{
y[i]=shapiro.test((X[,i]-mean)/sd)$p.value
}
y
Kolgomorov
----------------
K=matrix(0,ncol=4,nrow=4)
for(i in 1:4)
{for(j in 1:4)
{
K[i,j]=(ks.test((X[,i]-mean)/sd,(X[,j]-mean)/sd))$p.value
}
}







