#POWER VS THETA FOR ALL DISTRIBUTIONS
#CHANGE alternative for all the cases
theta_v=seq(-5,5,0.1)
#NORMAL
#POWER ESTIMATION of mannwhitney test 
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(rnorm(n1),rnorm(n2,theta,1),alternative ="l",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_norm_1=power_w(theta_v,10,15,1000,0.01)


#UNIFORM
#POWER ESTIMATION of mannwhitney test
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(runif(n1),runif(n2,theta,theta+1),alternative ="l",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_unif_1=power_w(theta_v,10,15,1000,0.01)



#CAUCHY

#POWER ESTIMATION of mannwhitney test
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(rcauchy(n1),rcauchy(n2,theta,1),alternative ="l",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_cauchy_1=power_w(theta_v,10,15,1000,0.01)




#EXPONENTIAL


power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(rexp(n1),rexp(n2)+theta,alternative ="l",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_exp_1=power_w(theta_v,10,15,1000,0.01)



M1=matrix(c(f_norm_1,f_unif_1,f_cauchy_1,f_exp_1),ncol=4)

matplot(theta_v,M1,main="Mann-Whitney U Test",xlab="theta",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.005")
abline(h=0.005)
legend("bottomright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)




________________________________________________________________________________









##wilcoxon vs parametric power comparison

#NORMAL


theta_v=seq(-2.5,2.5,0.1)
#POWER ESTIMATION of mannwhitney test
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(rnorm(n1),rnorm(n2,theta,1),alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_unif_1=power_w(theta_v,10,15,5000,0.01)



#power for t test 
power_t=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{
		p=replicate(M,t.test(rnorm(n1),rnorm(n2,theta,1),alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)

}
f_unif_2=power_t(theta_v,10,15,5000,0.01)

M2=matrix(c(f_unif_1,f_unif_2),ncol=2)
matplot(theta_v,M2,main="Plot of power function for samples from Normal population",xlab="Value of the parameter",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
#abline(h=0.01)
legend("top",c("Mann-Whitney Statistic", "Parametric counterpart"),lty=3:4,pch=15:16,col=1:6)

______________________________________________
#UNIFORM 



theta_v=seq(-2,2,0.1)
#POWER ESTIMATION of mannwhitney test
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(runif(n1),runif(n2)+theta,alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_unif_1=power_w(theta_v,10,15,1000,0.01)



#power for t test 
power_t=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{
		p=replicate(M,t.test(runif(n1),runif(n2)+theta,alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)

}
f_unif_2=power_t(theta_v,10,15,1000,0.01)

M2=matrix(c(f_unif_1,f_unif_2),ncol=2)
matplot(theta_v,M2,main="Plot of power function for samples from Uniform population",xlab="Value of the parameter",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
#abline(h=0.01)
legend("bottomright",c("Mann-Whitney Statistic", "Parametric counterpart"),lty=3:4,pch=15:16,col=1:6)









____________________________________________

#CAUCHY


theta_v=seq(-5,5,0.1)
#POWER ESTIMATION of mannwhitney test
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(rcauchy(n1),rcauchy(n2,theta,1),alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_unif_1=power_w(theta_v,10,15,1000,0.01)



#power for t test 
power_t=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{
		p=replicate(M,t.test(rcauchy(n1),rcauchy(n2,theta,1),alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)

}
f_unif_2=power_t(theta_v,10,15,1000,0.01)

M2=matrix(c(f_unif_1,f_unif_2),ncol=2)
matplot(theta_v,M2,main="Plot of power function for samples from Cauchy population",xlab="Value of the parameter",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
#abline(h=0.01)
legend("top",c("Mann-Whitney Statistic", "Parametric counterpart"),lty=3:4,pch=15:16,col=1:6)

_______________________________________________________________

#EXPONENTIAL


theta_v=seq(-2,2,0.1)
#POWER ESTIMATION of mannwhitney test
power_w=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{		
		p=replicate(M,wilcox.test(rexp(n1),rexp(n2)+theta,alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)
}
f_unif_1=power_w(theta_v,10,15,1000,0.01)



#power for t test 
power_t=function(theta_v,n1,n2,M,alpha)
{
	power_v=array(length(theta_v))
	i=1
	for(theta in theta_v)
	{
		p=replicate(M,t.test(rexp(n1),rexp(n2)+theta,alternative ="t",conf.level=1-alpha)$p.value)
		power_v[i]=(length(p[p<alpha])/M)
		i=i+1
	}
	return(power_v)

}
f_unif_2=power_t(theta_v,10,15,1000,0.01)

M2=matrix(c(f_unif_1,f_unif_2),ncol=2)
matplot(theta_v,M2,main="Plot of power function for samples from Exponential population",xlab="Value of the parameter",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
#abline(h=0.01)
legend("top",c("Mann-Whitney Statistic", "Parametric counterpart"),lty=3:4,pch=15:16,col=1:6)



_________________________________________________________

#ASYMPTOTIC BEHAVIOUR OF POWER FUNCTION

#wilcoxon test
#vary theta



theta=1
sample_size=matrix(c(2,3,5,10,15,20,25,30,40,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}
f_norm_1=array(10)
f_unif_1=array(10)
f_cauchy_1=array(10)
f_exp_1=array(10)


#NORMAL
#POWER ESTIMATION of mannwhitney test 
power_w=function(theta,n1,n2,M,alpha)
{
			
	p=replicate(M,wilcox.test(rnorm(n1),rnorm(n2,theta,1),alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_norm_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)


#UNIFORM
#POWER ESTIMATION of mannwhitney test
power_w=function(theta,n1,n2,M,alpha)
{
	
			
	p=replicate(M,wilcox.test(runif(n1),runif(n2,theta,theta+1),alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_unif_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)



#CAUCHY

#POWER ESTIMATION of mannwhitney test
power_w=function(theta,n1,n2,M,alpha)
{	
	p=replicate(M,wilcox.test(rcauchy(n1),rcauchy(n2,theta,1),alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_cauchy_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)



#EXPONENTIAL


power_w=function(theta,n1,n2,M,alpha)
{
	p=replicate(M,wilcox.test(rexp(n1),rexp(n2)+theta,alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_exp_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)



M1=matrix(c(f_norm_1,f_unif_1,f_cauchy_1,f_exp_1),ncol=4)

matplot(total,M1,main="Mann-Whitney U Test",xlab="Total sample size",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
abline(h=0.01)
legend("bottomright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)


__________________________________________________


#Parametric counterpart
#vary theta
theta=1
sample_size=matrix(c(2,3,5,10,15,20,25,30,40,50,48,55,60,70,70,75,80,85,90,95),nrow=10,ncol=2,byrow=T)
total=array(10)
for(i in 1:10)
{  total[i]=sample_size[i,1]+sample_size[i,2]
}
f_norm_1=array(10)
f_unif_1=array(10)
f_cauchy_1=array(10)
f_exp_1=array(10)


#NORMAL
power_w=function(theta,n1,n2,M,alpha)
{
			
	p=replicate(M,t.test(rnorm(n1),rnorm(n2,theta,1),alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_norm_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)


#UNIFORM
power_w=function(theta,n1,n2,M,alpha)
{
	
			
	p=replicate(M,t.test(runif(n1),runif(n2,theta,theta+1),alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_unif_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)



#CAUCHY
power_w=function(theta,n1,n2,M,alpha)
{	
	p=replicate(M,t.test(rcauchy(n1),rcauchy(n2,theta,1),alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_cauchy_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)



#EXPONENTIAL


power_w=function(theta,n1,n2,M,alpha)
{
	p=replicate(M,t.test(rexp(n1),rexp(n2)+theta,alternative ="l",conf.level=1-alpha)$p.value)
	power_v=(length(p[p<alpha])/M)
	return(power_v)
}
for(i in 1:10)
f_exp_1[i]=power_w(theta,sample_size[i,1],sample_size[i,2],1000,0.01)



M1=matrix(c(f_norm_1,f_unif_1,f_cauchy_1,f_exp_1),ncol=4)

matplot(total,M1,main="Parametric t Test",xlab="Total sample size",ylab="Estimate of Power function",lty=3:6,pch=15:18,lwd=1.5,type="b",sub="Size=0.01")
abline(h=0.01)
legend("bottomright",c("Normal","Uniform","Cauchy","Exponential"),lty=3:6,pch=15:18,col=1:6)








