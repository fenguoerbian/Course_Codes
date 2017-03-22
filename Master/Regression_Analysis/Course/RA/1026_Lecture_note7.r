require(scatterplot3d)
require(car)
load("RA_Lecture.rda")


###### Hilbert matrix ######
n<-8
i<-j<-1:n
H<-outer(i,j,function(a,b) 1/(a+b-1))
R<-solve(H)
V<-eigen(H)$values
conditional.number<-V[1]/V[n]


###### Multicollinearity: cigarettes example ######
str(cigarettes)
###### The scatter plot: cigarettes ######
with(cigarettes,scatterplot3d(Tar,Nicotine,CO,color=4,type="h",box=F))

###### The scatter plot matrix: cigarettes ######
pairs(cigarettes[,c(2,3,5)],col=4)

###### Three linear models ######
cig.lm1<-lm(CO~Tar,data=cigarettes)
cig.lm2<-lm(CO~Nicotine,data=cigarettes)
cig.lm3<-lm(CO~Tar+Nicotine,data=cigarettes)

summary(cig.lm1)
summary(cig.lm2)
summary(cig.lm3)

anova(cig.lm1,cig.lm3)
anova(cig.lm2,cig.lm3)

###### The fitted values in three linear models: cigarettes ######
par(mfrow=c(2,2))

plot(cigarettes$CO,type="p",pch=1,ylab="CO",ylim=c(0,30))
points(cig.lm1$fitted.values,pch=16,col=2)

plot(cigarettes$CO,type="p",pch=1,ylab="CO",ylim=c(0,30))
points(cig.lm2$fitted.values,pch=17,col=3)

plot(cigarettes$CO,type="p",pch=1,ylab="CO",ylim=c(0,30))
points(cig.lm3$fitted.values,pch=3,col=4)

plot(cigarettes$CO,type="p",pch=1,ylab="CO",ylim=c(0,30))
points(cig.lm1$fitted.values,pch=16,col=2)
points(cig.lm2$fitted.values,pch=17,col=3)
points(cig.lm3$fitted.values,pch=3,col=4)

par(mfrow=c(1,1))


X<-as.matrix(cigarettes[,c(2,3,5)])
Z<-t(X)%*%X
eigen(Z)
diag(solve(Z))


###### The variance inflation factor ######
vif(cig.lm3) #vif1=vif2=21.63

###### Multicollinearity: polynomial regression ######
i<-1:5
j<-0:4
X<-outer(i,j,"^")
X<-data.frame(X)

R1<-summary(lm(X2~X3+X4+X5,data=X))$r.squared
VIF1<-1/(1-R1)
R2<-summary(lm(X3~X2+X4+X5,data=X))$r.squared
VIF2<-1/(1-R2)
R3<-summary(lm(X4~X2+X3+X5,data=X))$r.squared
VIF3<-1/(1-R3)
R4<-summary(lm(X5~X2+X3+X4,data=X))$r.squared
VIF4<-1/(1-R4)

###### Multicollinearity: polynomial regression ######
i<-(-2):2
j<-0:4
X<-outer(i,j,"^")
X<-data.frame(X)

R1<-summary(lm(X2~X3+X4+X5,data=X))$r.squared
VIF1<-1/(1-R1)
R2<-summary(lm(X3~X2+X4+X5,data=X))$r.squared
VIF2<-1/(1-R2)
R3<-summary(lm(X4~X2+X3+X5,data=X))$r.squared
VIF3<-1/(1-R3)
R4<-summary(lm(X5~X2+X3+X4,data=X))$r.squared
VIF4<-1/(1-R4)