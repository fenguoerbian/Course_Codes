

load("DOE_Lecture.rda")

###### Graeco-Latin square design ######
### propellant2 ###
str(propellant2)

### Illustration ###
designX<-model.matrix(~trt+row+col+graceo,data=propellant2,
                        contrasts=list(trt="contr.sum",
                                       row="contr.sum",
                                       col="contr.sum",
                                       graceo="contr.sum"))
crossprod(designX)

### propellant ###
p.aov<-aov(rate~trt+col+row+graceo,data=propellant2)
model.tables(p.aov,se=T)
summary(p.aov)
trt.tukey<-TukeyHSD(p.aov,which="trt")
plot(trt.tukey)
par(mfrow=c(2,2))
plot(p.aov)
par(mfrow=c(1,1))



###### two-way factory ######
str(battery)
b.fit<-aov(life~material*temperature,data=battery)
summary(b.fit)
with(battery,interaction.plot(temperature,material,life,col=1:3,lwd=2))
with(battery,interaction.plot(material,temperature,life,col=1:3,lwd=2))
inter.tukey<-TukeyHSD(b.fit,which="material:temperature")
plot(inter.tukey)
par(mfrow=c(2,2))
plot(b.fit)
par(mfrow=c(1,1))

###### two-way factory, as a linear regression model ######
tdat<-data.frame(A=factor(rep(1:3,times=2)), B=factor(rep(1:3,each=2)))
model.matrix(~A*B,data=tdat)


###### split the temperature ######
summary(b.fit,split=list(temperature=list(line=1,dual=2)))

###### model.tables ######
model.tables(b.fit)
model.tables(b.fit,type = "means")



###### let's assume there is no interaction ######
b.fit<-aov(life~material+temperature,data=battery)
summary(b.fit)

###### two-way factorial, one obsevation per cell ######
str(impurity)
i.fit<-aov(impurity~temperature+pressure,data=impurity)
summary(i.fit)
model.tables(i.fit,se=T)
model.tables(i.fit,"means")
i.tukey<-TukeyHSD(i.fit)
plot(i.tukey)

###### ordered and split ######
i.fit2<-aov(impurity~ordered(temperature)+ordered(pressure),data=impurity)
summary(i.fit2,split=list("ordered(temperature)"=list(l=1,q=2), "ordered(pressure)" = list(l=1,d=2,c=3,q=4) ))
