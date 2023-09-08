load(url("http://www.mas.ncl.ac.uk/~nrh3/survival/critcare025.RData"))
library(survival)

install.packages("survminer")
library("survminer")
update.packages("survminer")
load(url("http://www.mas.ncl.ac.uk/~nrh3/survival/extra_mas3912_routines.RData"))

summary(critcare)



kmtime = survfit(Surv(time,status)~1,data=critcare)
summary(kmtime)


ggsurvplot(kmtime, type = "hazard", xlab = "Time", ylab = "Hazard")


par(mfrow= c(1,1))

plot(kmtime$surv,lwd=2,col=2,type = "l",ylim=c(0,1)
     ,xlab="survival time, t (in days)",ylab = "Probability",
     main = "Survival function, S(t)")


kmtime$surv
kmtime$surv[30]

kmdepriv = survfit(Surv(time,status)~depriv,data=critcare)
print(kmdepriv)
plot(kmdepriv,lwd=2,col=c(2,3,4,5,6)
     ,xlab="survival time, t (in days)",ylab = "Probability",
     main = "Deprivation Index against survival times")
legend(0,0.5,legend = c("1","2","3","4","5"),col=c(2,3,4,5,6),lty=1,
       title = "index")


x = seq(1,600,1)

par(mfrow= c(1,3))

hist(critcare$age,breaks = 100,main ="Histogram of Age",xlab="Age")
hist(critcare$bmi,breaks = 60,main ="Histogram of BMI", xlab="BMI" )
hist(critcare$apache,breaks = 60,main ="Histogram of Apache II score",xlab = "Apache II score")

sum(critcare$bmi>25) # over 80% of the population are overweight here
sum(critcare$bmi>30) # over 50% "   "       "      "    obese     "
range(critcare$bmi)
range(critcare$apache)

sum(critcare$age<80)- sum(critcare$age<40) 


table(critcare$time)
table(critcare$time,critcare$status)

table(critcare$female) # 2/3 are male
table(critcare$comor) # 1/12 have comorbidities
table(critcare$status) # roughly 57/43 % split, sounds about right 
table(critcare$depend) # 1/12 have dependancy 
table(critcare$depriv) # 50% of population are in bracket 4 or 5
table(critcare$invent) # 45/55% split 

mean(critcare$age)# mean age is 58
sqrt(var(critcare$age))
range(critcare$age)


mean(critcare$bmi) # 30 or above is considered obese
sqrt(var(critcare$bmi))
range(critcare$bmi)


mean(critcare$apache) # Apache 2 score ranges from 0-71
sqrt(var(critcare$apache))
range(critcare$apache)

par(mfrow=c(1,1))


### question 2 

fitdep = survfit(Surv(time,status)~depend,data=critcare)
fitdep$surv
fitdep$surv[29]
fitdep$surv[34]
survdiff(Surv(time,status)~depend,data=critcare)

plot(fitdep,lwd=2,col=c(2,3)
     ,xlab="survival time, t (in days)",ylab = "Probability",
     main = "the survival function ignoring risk factors other than dependencies"
     ,cex.lab=1.5,cex.main=1.5)
legend(27,1,legend = c("0","1"),col=c(2,3),lty=1,
       title = "index")

### question  3

fitinv = survfit(Surv(time,status)~invent,data=critcare)
survdiff(Surv(time,status)~invent,data=critcare)

plot(fitinv,lwd=2,col=c(2,3)
     ,xlab="survival time, t (in days)",ylab = "Probability",
     main = "the survival function ignoring risk factors other than invasive ventilation"
     ,cex.lab=1.5,cex.main=1.5)
legend(27,1,legend = c("0","1"),col=c(2,3),lty=1,
       title = "index")

### comorbidities

fitcomor = survfit(Surv(time,status)~comor,data=critcare)
survdiff(Surv(time,status)~comor,data=critcare)

plot(fitcomor,lwd=2,col=c(2,3)
     ,xlab="survival time, t (in days)",ylab = "Probability",
     main = "the survival function ignoring risk factors other than comorbidities"
     ,cex.lab=1.5,cex.main=1.5)
legend(0,0.25,legend = c("0","1"),col=c(2,3),lty=1,
       title = "index")


#### stratification of depriv 


fitdepriv=coxph(Surv(time,status)~.+strata(depriv), data=critcare)
plot(survfit(fitdepriv),lwd=2,col=c(2,3,4,5,6),fun="cloglog",
     xlab="log(time)",ylab = "log(A(t))",
     main = "Cumulative hazards stratified by deprivation score"
     ,cex.lab=1.5,cex.main=1.5)

legend(1,-0.5,legend = c("1","2","3","4","5"),col=c(2,3,4,5,6),lty=1,
       title = "index")








### question 4

fitcrit = coxph(Surv(time,status)~age+female+bmi,data=critcare)
summary(fitcrit)

### question 5 

age = rep(50,length = 4)
female = c(0,0,0,0)
bmi = rep(25,length=4)

cases = data.frame(age,female,bmi,apache)
plot(survfit(fitcrit2,newdata=cases),mark.time=FALSE,lwd=2,col=c(2,3,4,5)
     ,xlab="survival time, t (in days)",ylab = "Probability",
     main = "Our future patient with varying Apache II Scores"
     ,cex.lab=1.5,cex.main=1.5)
legend(0,0.4,legend = c("20","30","40","50"),col=c(2,3,4,5),lty=1,
       title = "Score")

survfit(fitcrit,newdata=cases)


## apache
fitcrit2 = coxph(Surv(time,status)~age+female+bmi+apache,data=critcare)
summary(fitcrit2)

apache = c(20,30,40,50)



# final analysis 




fitfinal = coxph(Surv(time,status)~age,data=critcare)
summary(fitfinal)


p=1
aic=2*p-2*fitfinal$loglik[2]
schemper(fitfinal)

(aic)


#### schoenfeld residuals 

fitty = coxph(Surv(time,status)~.,data=critcare)
scho = cox.zph(fitty)
par(mfrow=c(2,4))
plot(scho,col=2,lwd=1.5,cex.lab=1.5,cex.main=1.5)
par(mfrow=c(1,1))








