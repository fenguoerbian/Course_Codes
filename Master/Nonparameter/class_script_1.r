# Class script to compare the performance of parametirc and nonparametric density estimators
# The functions at the "pre-define functions" part of this script should be runned before the main part


n<-10000
mu<-1
sqrt_var<-2
x<-rnorm(mean=mu,sd=sqrt_var,n=n)


# Real density
Real_density<-dnorm(seq(-1,3,length.out = 100),mean=mu,sd=sqrt_var)

#MLE
mu_hat_mle<-mean(x)
var_hat_mle<-var(x)*(n-1)/n
MLE<-dnorm(-1:3,mean=mu_hat_mle,sd=sqrt(var_hat_mle))

#kernel
gauss_k<-mydensity(x,"Gauss",-1:3)
uniform_k<-mydensity(x,"Uniform",-1:3)
epan_k<-mydensity(x,"Epan",-1:3)

# summary plot
matplot(-1:3,cbind(MLE,gauss_k,uniform_k,epan_k),type="l")
lines(seq(-1,3,length.out = 100),Real_density)


###### pre-define functions ######
### This function comes from mydensity.r 
### I revised it a little bit to fit the question here
mydensity<-function(Data,indicator,x)
{
  n<-length(Data)
  sample_var<-sqrt(var(Data))
  Temp<-NULL
  if(indicator=="Gauss")
  {
    h<-sample_var*1.06*n^(-1/5)
    for(i in x)
    {
      u<-(i-Data)/h;
      Temp<-append(Temp,sum(1/sqrt(2*pi)*exp(-(u)^2/2))/n/h);
    } 
  }
  
  
  if(indicator=="Uniform")
  {
    h<-sample_var*2.78*n^(-1/5)
    for(i in x)
    {
      u<-(i-Data)/h;
      Temp<-append(Temp,sum( 0.5*( abs(u)<1 ))/n/h);
    }
  }
  
  if(indicator=="Epan")
  {
    h<-sample_var*2.34*n^(-1/5)
    for(i in x)
    {
      u<-(i-Data)/h
      Temp<-append(Temp,sum( 3/4*(1-u^2)*( abs(u)<1 )  )/n/h)
    }
  }
  
  return(Temp)
}
