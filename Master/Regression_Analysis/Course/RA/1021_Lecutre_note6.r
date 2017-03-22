require(scatterplot3d)
require(MASS)


load("RA_Lecture.rda")
str(viscosity)
# in this data, Z=log(Y)


###### Original response: viscosity ######
with(viscosity,scatterplot3d(f,p,Y,type="h",color=4,box=F))

###### Transformed response: viscosity ######
with(viscosity,scatterplot3d(f,p,Z,color=4,type="h",box=F))

###### working with the original response ######
Y.lm<-lm(Y~f+p+I(f^2)+I(p^2)+f:p,data=viscosity)
summary(Y.lm)
anova(Y.lm)
anova(lm(Y~f+p,data=viscosity),Y.lm)

###### working with the transformed response ######
Z.lm<-lm(Z~f+p+I(f^2)+I(p^2)+f:p,data=viscosity)
summary(Z.lm)
Z.anova<-anova(Z.lm)

### conduct F-test comparing 2 models dirctly ###
SS<-sum(Z.anova[3:5,2])
SS_df<-sum(Z.anova[3:5,1])
MS_res<-Z.anova[6,3]
F_statistic<-SS/SS_df/MS_res
p_value<-pf(F_statistic,SS_df,Z.anova[6,1],lower.tail = F)
### compare F-test comparing 2 models using anova command ###
anova(lm(Z~f+p,data=viscosity),Z.lm)


### model diagnostic of the chosen model: lm(Z~f+p, data=viscosity) ###
Z.lm<-lm(Z~f+p,data=viscosity)
summary(Z.lm)
anova(Z.lm)

par(mfrow=c(2,2))
plot(Z.lm)
par(mfrow=c(1,1))

Z.lm.r<-resid(Z.lm)
Z.lm.s<-rstandard(Z.lm)
Z.lm.t<-rstudent(Z.lm)
Z.lm.c<-cooks.distance(Z.lm)

par(mfrow=c(2,2))
plot(abs(Z.lm.r),type="h")
k<-sort.list(abs(Z.lm.r),decreasing = T)[1:3]
points(k,abs(Z.lm.r)[k],pch=16,col=2)
text(k+0.5,abs(Z.lm.r)[k],k)

plot(abs(Z.lm.s),type="h")
k<-sort.list(abs(Z.lm.s),decreasing = T)[1:3]
points(k,abs(Z.lm.s)[k],pch=16,col=2)
text(k+0.5,abs(Z.lm.s)[k],k)

plot(abs(Z.lm.t),type="h")
k<-sort.list(abs(Z.lm.t),decreasing = T)[1:3]
points(k,abs(Z.lm.t)[k],pch=16,col=2)
text(k+0.5,abs(Z.lm.t)[k],k)

plot(Z.lm.c,type="h")
k<-sort.list(Z.lm.c,decreasing = T)[1:3]
points(k,Z.lm.c[k],pch=16,col=2)
text(k+0.5,Z.lm.c[k],k)
par(mfrow=c(1,1))


###### Profile likelihood: viscosity ######
### compute using boxcox command ###
lam_res<-boxcox(Y.lm)

### compute directly ######
m<-100
lambda<-seq(-2,2,length.out=m) # we don't treat lambda=0
n<-length(viscosity$Y)
alpha<-0.05
y_dot<-prod(viscosity$Y)^(1/n)
Y_temp<-outer(viscosity$Y,lambda,function(y,l,y_dot) y_dot^(1-l)*(y^l-1)/l,y_dot)
Y_temp<-data.frame(Y_temp)
sigma2<-sapply(Y_temp,function(k) summary(lm(k~f+p+I(f^2)+I(p^2)+f:p,data=viscosity))$sigma^2)
pl<--n/2*log(sigma2)
plot(lambda,pl,type="l",xlab=expression(lambda),ylab="log-Likelihood")
V<-lambda[which.max(pl)]
H<-max(pl)-0.5*qchisq(1-alpha,1)
abline(v=V,lty=2)
abline(h=H,lty=2)
text(-2,H,"95%")
L_number<-which.min(abs(pl[1:which.max(pl)]-H))
R_number<-which.max(pl)-1+which.min(abs(pl[which.max(pl):m]-H))
abline(v=lambda[L_number],lty=2)
abline(v=lambda[R_number],lty=2)
