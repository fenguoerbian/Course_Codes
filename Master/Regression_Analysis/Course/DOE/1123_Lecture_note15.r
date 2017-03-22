load("DOE_Lecture.rda")

###### blocking in a factorial design ######
str(detection)
fit<-aov(intensity~ground*filter+operator,data=detection) # interaction not significant
fit<-aov(intensity~ground+filter+operator,data=detection)
summary(fit)
model.tables(fit,se = T)
plot(TukeyHSD(fit,which="ground"))
summary(fit,split=list(ground=list(L=1,Q=2))) # maybe I should order the factor before split it.
# In fact, only ground is continuous, filter is discrete and can not be ordered
detection.g<-detection
detection.g$ground<-ordered(detection.g$ground)
fit<-aov(intensity~ground+filter+operator,data=detection.g)
summary(fit,split=list(ground=list(L=1,Q=2)))



###### detection, modified experiment ######
str(detection2)
fit<-aov(intensity~ground*filter+day+operator,data=detection2)
summary(fit)
fit<-aov(intensity~ground*filter+operator,data=detection2)
summary(fit)
model.tables(fit,se=T)
plot(TukeyHSD(fit,which="ground:filter"))
detection.g<-detection2
detection.g$ground<-ordered(detection.g$ground)
fit<-aov(intensity~ground*filter+operator,data=detection.g)
summary(fit,split=list(ground=list(L=1,Q=2)))
