load("DOE_Lecture.rda")

###### Multi-way factorial ######
str(fillheight)
f.aov<-aov(Y~(A+B+C)^3,data=fillheight)
# summary of the model #
summary(f.aov)
model.tables(f.aov,se=T)
# TukeyHSD #
f.tukey<-TukeyHSD(f.aov)
# interaction plot, type1: fix the third variable level #
with(fillheight[fillheight$C==200,],interaction.plot(A,B,Y,type = "l"))
with(fillheight[fillheight$C==250,],interaction.plot(A,B,Y,type = "l"))
# interaction plot, type2: conmbine the first 2 variables into 1 variable #
AB<-paste(fillheight$A,"-",fillheight$B)
interaction.plot(AB,fillheight$C, fillheight$Y,type="l")

# new model #
f.aov2<-aov(Y~(A+B+C)^2,data=fillheight)
summary(f.aov2)
f.aov2<-aov(Y~A+B+C+A:B,data=fillheight)
summary(f.aov2)
# split the anova #
temp<-fillheight
temp$A<-ordered(temp$A)
temp$B<-ordered(temp$B)
temp$C<-ordered(temp$C)
f2.aov<-aov(Y~A*B+C,data=temp)
summary(f2.aov,split=list(A=list(l=1,q=2))) # only A needs to be splitted 
# the final lm model is A+B+C+A^2+AB
