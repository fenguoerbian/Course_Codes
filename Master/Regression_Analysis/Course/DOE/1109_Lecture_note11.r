require(multcomp)

load("DOE_Lecture.rda")

###### Quantitative factor ######
fiber$weight<-as.numeric(as.character(fiber$weight)) # change factor to numeric
fit1<-lm(strength~weight,data=fiber)
fit2<-lm(strength~weight+I(weight^2),data=fiber)
fit3<-lm(strength~weight+I(weight^2)+I(weight^3),data=fiber)
fit4<-lm(strength~weight+I(weight^2)+I(weight^3)+I(weight^4),data=fiber)

h<-seq(min(fiber$weight),max(fiber$weight),length.out=101)
y1<-predict(fit1,data.frame(weight=h))
y2<-predict(fit2,data.frame(weight=h))
y3<-predict(fit3,data.frame(weight=h))
y4<-predict(fit4,data.frame(weight=h))
Y<-cbind(y1,y2,y3,y4)

with(fiber,plot(weight,strength))
matlines(h,Y,col=2:5,lty=1)

fiber$weight<-as.factor(fiber$weight) # restore the data
fit<-aov(strength~weight,data=fiber)
points(seq(15,35,by=5),model.tables(fit,type="mean")$tables$weight,pch=16,col=2)



###### Quantitative factor ######
### The main difference of the result between factor and ordered factor is due to 
### in R, the default design matrix of factor is contr.treatment,
### while the default design
### split the original data, weight is unordered factor ###
summary(fit,split=list(weight=list(lin=1,quad=2,cub=3,quar=4) ) )

### turn weight into ordered factor ###
fiber$weight2<-ordered(fiber$weight)
str(fiber)
fit.order<-aov(strength~weight2,data=fiber)
summary(fit.order,split=list( weight2=list(lin=1,quad=2,cub=3,quar=4) ) )



###### Multiple comparison Orthogonal contrasts ######
M<-rbind(c(0,0,0,1,-1),c(1,0,1,-1,-1),c(1,0,-1,0,0),c(-1,4,-1,-1,-1))
crossprod(t(M))
fit<-aov(strength~weight,data=fiber)
tab<-summary(fit)
M<-t(apply(M,1,function(u) u/sqrt(sum(u^2))))
mu<-model.tables(fit)
ssc<-mu$n*drop(M%*%as.vector(mu$tables$weight))^2
s2<-tab[[1]][["Mean Sq"]][2]
fc<-ssc/s2
pvalue<-pf(fc,1,tab[[1]][["Df"]][2],lower.tail = F)
cbind(ssc=ssc,msc=ssc,F=fc,pvalue=pvalue)



###### Multiple comparison: Tukey's method: comparing all pairs of means ######
fit.tukey<-TukeyHSD(fit)
plot(fit.tukey)



###### Multiple comparison: the multcomp package ######
fiber$weight<-relevel(fiber$weight,"35")
fit<-aov(strength~weight,data=fiber)
dtest<-glht(fit,linfct=mcp(weight="Dunnett"))
(out<-summary(dtest))
plot(out)
