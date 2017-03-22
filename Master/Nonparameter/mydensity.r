# Data: Original sample data, I used the "faithful". It contains 2 columns and I only need the first one.
# h: window width
# indicator: which kernel to use. Currently support "Gauss", "Epan" and "Uniform"
# x: points where you want to estimate f(x)


mydensity<-function(Data,indicator,x)
{
  n<-length(Data[,1])
  sample_var<-sqrt(var(Data[,1]))
  Temp<-NULL
  if(indicator=="Gauss")
  {
    h<-sample_var*1.06*n^(-1/5)
    for(i in x)
    {
      u<-(i-Data[,1])/h;
      Temp<-append(Temp,sum(1/sqrt(2*pi)*exp(-(u)^2/2))/n/h);
    } 
  }
  
  
  if(indicator=="Uniform")
  {
    h<-sample_var*2.78*n^(-1/5)
    for(i in x)
    {
      u<-(i-Data[,1])/h;
      Temp<-append(Temp,sum( 0.5*( abs(u)<1 ))/n/h);
    }
  }
  
  if(indicator=="Epan")
  {
    h<-sample_var*2.34*n^(-1/5)
    for(i in x)
    {
      u<-(i-Data[,1])/h
      Temp<-append(Temp,sum( 3/4*(1-u^2)*( abs(u)<1 )  )/n/h)
    }
  }
  
  plot(x,Temp,type="l")
  print(h)
}

