C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW1 Solutions R Code.R

#############
#
# Oring data
#
#############
f = file.choose()
oring = read.csv(f)
summary(oring)
sd(oring$oring_damage)
sd(oring$temp)

#split into two groups and calculate the test statistics
oring$damage = damage = 1*(oring$oring_damage>1)
temp.damage = oring$temp[damage==1]
temp.nodamage = oring$temp[damage==0]

boxplot(temp.damage,temp.nodamage,col=c("red","blue"),names=c("Damage","No Damage"))


##############
#Performed as an unpooled 2-sample t-test
##############
t.test(oring$temp~oring$oring_damage>0)


##############
#or as a permutation test:
##############
meandiff.obs = mean(temp.damage)-mean(temp.nodamage)



#initialize the vector to store the simulated 
nsims=10000
meandiff.sim = rep(NA,nsims)


#and the for loop to do all the work: reordering the x's, and splitting into groups
for(i in 1:nsims){
  damage.sim = sample(damage)
  meandiff.sim[i]=mean(oring$temp[damage.sim==0])-mean(oring$temp[damage.sim==1])
}


hist(meandiff.sim, col="grey",breaks=c((-15):15-0.1))
abline(v=meandiff.obs,col="red",lwd=2)
abline(v=-meandiff.obs,col="red",lwd=2)

mean(meandiff.sim)
sd(meandiff.sim)
#two-sided p-value
mean( abs(meandiff.sim) >= abs(meandiff.obs))









*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW3 Solutions R Code.R
#HW 3 R code

##############
#
# 3) Variance Estimation Simulation
#
##############

nsims=10000
mu=1
sigma=3
n=25
sigma2.hat=s2=rep(NA,nsims)

for(i in 1:nsims){
  sample=rnorm(n,mean=mu,sd=sigma)
  xbar=mean(sample)
  sigma2.hat[i]=sum((sample-mu)^2)/n
  s2[i]=var(sample)

}

mean(sigma2.hat)
mean(s2)

sd(sigma2.hat)
sd(s2)

mean(abs(sigma2.hat-sigma^2)>abs(s2-sigma^2))


hist(sigma2.hat,xlim=c(0,25),col="grey")
hist(s2,xlim=c(0,25),col="grey")

#e
chisq25=rchisq(length(sigma2.hat),df=25)
chisq24=rchisq(length(s2),df=24)
qqplot(chisq25,sigma2.hat)
qqplot(chisq24,s2)



##############
#
# 4/5) Boston Snowfall
#
##############

f = file.choose()
snow = read.csv(f)
summary(snow)
sd(snow$totalsnow)
sd(snow$avgmaxtemp)

#split into two groups and calculate the test statistics
snow$high = high = 1*(snow$avgmaxtemp>59.63)
totalsnow.high = snow$totalsnow[high==1]
totalsnow.low = snow$totalsnow[high==0]

boxplot(totalsnow.high,totalsnow.low,col=c("red","blue"),names=c("High","Low"))

meandiff.obs = mean(totalsnow.high)-mean(totalsnow.low)



#initialize the vector to store the simulated 
nsims=10000
meandiff.sim = rep(NA,nsims)


#and the for loop to do all the work: reordering the x's, and splitting into groups
for(i in 1:nsims){
  high.sim = sample(high)
  meandiff.sim[i]=mean(snow$totalsnow[high.sim==0])-mean(snow$totalsnow[high.sim==1])
}


hist(meandiff.sim, col="grey",breaks=c((-20):20-0.1))
abline(v=meandiff.obs,col="red",lwd=2)
abline(v=-meandiff.obs,col="red",lwd=2)


meandiff.obs
mean(meandiff.sim)
sd(meandiff.sim)
#two-sided p-value
mean( abs(meandiff.sim) >= abs(meandiff.obs))

*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW4 Solutions R Code.R
#HW 4 R code


##############
#
# 2) Boston Snowfall
#
##############

f = file.choose()
snow = read.csv(f)
summary(snow)

#split into two groups and calculate the test statistics
snow$high = high = 1*(snow$avgmaxtemp>59.63)
totalsnow.high = snow$totalsnow[high==1]
totalsnow.low = snow$totalsnow[high==0]



t.test(totalsnow.high,totalsnow.low)
boxplot(totalsnow.high,totalsnow.low,col=c("red","blue"),names=c("High","Low"))






##################
# Old code from HW #3
##################

meandiff.obs = mean(totalsnow.high)-mean(totalsnow.low)



#initialize the vector to store the simulated 
nsims=10000
meandiff.sim = rep(NA,nsims)


#and the for loop to do all the work: reordering the x's, and splitting into groups
for(i in 1:nsims){
  high.sim = sample(high)
  meandiff.sim[i]=mean(snow$totalsnow[high.sim==0])-mean(snow$totalsnow[high.sim==1])
}


hist(meandiff.sim, col="grey",breaks=c((-20):20-0.1))
abline(v=meandiff.obs,col="red",lwd=2)
abline(v=-meandiff.obs,col="red",lwd=2)


meandiff.obs
mean(meandiff.sim)
sd(meandiff.sim)
#two-sided p-value
mean( abs(meandiff.sim) >= abs(meandiff.obs))



*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW5 Solutions R Code.R
#HW 5 R code

#########
# Prob 2
#########
f=file.choose()
textdata=read.csv(f)

attach(textdata)

# Part a: t-test
t.test(texts~female,var.equal=T)

# Part b: visually investigating normality
boxplot(texts~female,col=c("hotpink","blue"),ylab="# Text Messages Sent (per day)",names=c("Female","Male"))
boxplot(logtexts~female,col=c("hotpink","blue"),ylab="Log(#Texts) Sent (per day)",names=c("Female","Male"))

# Part c: t-test after log-transform
logtexts=log(texts)
t.test(logtexts~female,var.equal=T)

# Part d: Wilcoxon Rank Sum test
nF=sum(female==1)
nM=sum(female==0)
ranks=rank(texts)
Rbar=mean(ranks)
s2R=sd(ranks)
W=sum(ranks[female==1])
Z=(W-Rbar*nF)/sqrt(s2R*nF*nM/(nF+nM))
2*(1-pnorm(abs(Z)))


# Part e: Permutation test

meandiff.obs = mean(texts[female==1])-mean(texts[female==0])
meandiff.log.obs = mean(logtexts[female==1])-mean(logtexts[female==0])



#initialize the vector to store the simulated 
nsims=10000
meandiff.sim = rep(NA,nsims)
meandiff.log.sim = rep(NA,nsims)


#and the for loop to do all the work: reordering the x's, and splitting into groups
for(i in 1:nsims){
  female.sim = sample(female)
  meandiff.sim[i]=mean(texts[female.sim==1])-mean(texts[female.sim==0])
  meandiff.log.sim[i]=mean(logtexts[female.sim==1])-mean(logtexts[female.sim==0])
}


hist(meandiff.sim, col="grey",breaks=2*c((-20):22))
abline(v=meandiff.obs,col="red",lwd=2)
abline(v=-meandiff.obs,col="red",lwd=2)


#two-sided p-value
mean( abs(meandiff.sim) >= abs(meandiff.obs))

#And if performed on the log scale
mean( abs(meandiff.log.sim) >= abs(meandiff.log.obs))







#########
# Prob 3
#########
# Part a
set.seed(54321)
nsims=10000
n=50
pvalues3a=rep(NA,nsims)

for(i in 1:nsims){
  sample1=rnorm(50)
  sample2=rnorm(50)
  pvalues3a[i]=t.test(sample1, sample2, alternative="greater", var.equal=TRUE)$p.value
}
hist(pvalues3a,col="pink",main="p-values from a 2-sample pooled t-test \n when both samples are Standard Normal")


# Part b
pvalues3b=rep(NA,nsims)

for(i in 1:nsims){
  sample1=rchisq(10,df=1)
  sample2=rchisq(100,df=3)-2
  pvalues3b[i]=  t.test(sample1, sample2, var.equal=TRUE, alternative="two.sided")$p.value

}
hist(pvalues3b,col="pink",main="p-values from a 2-sample pooled t-test \n when samples are Chi-sq(1) and Chi-sq(2)-1")



# Part c
pvalues3c=rep(NA,nsims)

for(i in 1:nsims){
  x=rt(50,df=5)
  z=rnorm(50)
  y=z-x
  pvalues3c[i]=  t.test(x,y, var.equal=TRUE, alternative="less")$p.value
}
hist(pvalues3c,col="pink",main="p-values from a 2-sample pooled t-test \n when samples are coming from x=t(5) and N(0,1)")

 
# Part d
pvalues3d=rep(NA,nsims)

for(i in 1:nsims){
  x=rt(50,df=10)
  z=rnorm(50)
  y=z-x
  pvalues3d[i]=  t.test(x,y, var.equal=TRUE, paired=TRUE, alternative="less")$p.value
}
hist(pvalues3d,col="pink",main="p-values from a paired t-test \n when samples are coming from x=t(5) and N(0,1)")

alpha=0.05
mean(pvalues3a<alpha)
mean(pvalues3b<alpha)
mean(pvalues3c<alpha)
mean(pvalues3d<alpha)
*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW6 Solutions R Code.R

##################
# Prob 1
##################

f=file.choose()
textdata=read.csv(f)

attach(textdata)

# Wilcoxon Rank Sum test
nF=sum(female==1)
nM=sum(female==0)
ranks=rank(texts)
Rbar=mean(ranks)
s2R=sd(ranks)
W=sum(ranks[female==1])
Z=(W-Rbar*nF)/sqrt(s2R*nF*nM/(nF+nM))
2*(1-pnorm(abs(Z)))



##################
# Prob 2
##################

folder = "C:\\Users\\krader\\Desktop\\Stat 139 - Fall 2015\\Homework\\Homework05\\"
blackoutdata = read.csv(paste(folder,"BlackoutBirths.csv",sep=""))
names(blackoutdata)

set.seed(12345)
nsims = 100000

######
# t Test
######
# Check for "normality"...doesn't look "too" bad, 
# except there is an outlier, but not clear how to tranform
hist(blackoutdata$Aug8th-blackoutdata$Average,col="grey")
hist(log(blackoutdata$Aug8th)-log(blackoutdata$Average),col="grey")

t.test(blackoutdata$Aug8th,blackoutdata$Average,paired=T)
t.test(log(blackoutdata$Aug8th),log(blackoutdata$Average),paired=T)


######
# Signed-Rank Test
######

Diff=blackoutdata$Diff=blackoutdata$Aug8th-blackoutdata$Average
Diff=blackoutdata$Diff= Diff[Diff!=0]
n=length(Diff)
Ranks=rank(abs(Diff))

#observed test statistic
S = sum(Ranks[Diff>0])
Exp.S = sum(Ranks)/2

#####
# since sample size is small and there are ties, 
# we need to resample the ranks to build the reference distribution
#####
S.sim = rep(NA,nsims)

for(i in 1:nsims){
 rand.sign=2*rbinom(n,1,0.5)-1
 S.sim[i] = sum(Ranks[rand.sign>0])
}

#Plotting the reference distribution
hist(S.sim,col="grey",breaks=20)
abline(v=S,col="red",lwd=2)
abline(v=Exp.S-(S-Exp.S),,col="red",lwd=2)

# 2-sided p-value
mean(abs(S.sim-Exp.S) >= abs(S-Exp.S))



######
# Sign Test
######
X=sum(Diff>0)
signtest.p.value=(1-pbinom(X-1,n,0.5))+pbinom(n-X,n,0.5)
signtest.p.value

*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW7 Solutions R Code.R

##############
#
# 2) When Authors Die
#
##############

f=file.choose()
writersdata=read.csv(f)
attach(writersdata)

fit2=aov(agedeath~type,data=writersdata)
summary(fit2)

summarystats=cbind(by(agedeath,type,length),by(agedeath,type,mean),by(agedeath,type,sd))
colnames(summarystats)=c("n","mean","sd")
summarystats

boxplot(agedeath~type,data=writersdata,col=c("red","white","blue"))*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW8 Solutions R Code.R

##############
#
# 1) When Authors Die
#
##############

f=file.choose()
writersdata=read.csv(f)
attach(writersdata)

fit3=aov(agedeath~type,data=writersdata)
summary(fit3)

summarystats=cbind(by(agedeath,type,length),by(agedeath,type,mean),by(agedeath,type,sd))
colnames(summarystats)=c("n","mean","sd")
summarystats

qtukey(0.95,3,120)

TukeyHSD(fit3)




##############
#
# 2) Iron in Food
#
##############

f=file.choose()
irondata=read.csv(f)
attach(irondata)

fit4=aov(iron~food*pot,data=irondata)
summary(fit4)


yhat=irondata$yhat=as.vector(predict(fit4))
plot(yhat~foodgroup,data=irondata,subset=(pot=="Iron"),cex=2,pch=16,ylab="Iron in Food",ylim=c(min(yhat),max(yhat)),xaxt="n",col="darkorange1")
lines(yhat~foodgroup,data=irondata,subset=(pot=="Iron"),col="darkorange1",lwd=5)
mtext(c("Meat","Leg","Veg"),side=1,adj=c(0,0.5,1))

points(yhat~foodgroup,data=iron,subset=(pot=="Clay"),col="royalblue",cex=2,pch=16)
lines(yhat~foodgroup,data=iron,subset=(pot=="Clay"),type="l",col="royalblue",lwd=5)

points(yhat~foodgroup,data=iron,subset=(pot=="Aluminum"),col="forestgreen",cex=2,pch=16)
lines(yhat~foodgroup,data=iron,subset=(pot=="Aluminum"),type="l",col="forestgreen",lwd=5)

##############
#
# 3) Multiple Comparisons Simulation
#
##############

#########
# Part c
#########
set.seed(12)
nsims=10000
alpha=0.05
numgroups=5
I=10
alpha.bonf=alpha/I
alpha.ind=1 - (1-alpha)^(1/I)
q=qtukey(0.95,numgroups,20)/sqrt(2)
alpha.tukey=2*(1-pt(q,df=20))

minpvalues=rep(NA,nsims)

for(i in 1:nsims){
 tstats=rt(I,df=20)
 minpvalues[i]=2*(1-pt(max(abs(tstats)),df=20))
 #minpvalues.tukey[i]=2*(1-ptukey(max(abs(tstats))*sqrt(2),numgroups,20))
}
mean(minpvalues<alpha)
mean(minpvalues<alpha.bonf)
mean(minpvalues<alpha.ind)
mean(minpvalues<alpha.tukey)

#########
# Part d
#########
minpvalues=rep(NA,nsims)

for(i in 1:nsims){
 x=matrix(rnorm(55),ncol=5)
 xbars=apply(x,2,mean)
 minpvalues[i]= t.test(x[,which(xbars==max(xbars))], x[,which(xbars==min(xbars))],var.equal=T)$p.value

}
mean(minpvalues<alpha)
mean(minpvalues<alpha.bonf)
mean(minpvalues<alpha.ind)
mean(minpvalues<alpha.tukey)

*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW9 Solutions R Code.R


#############
#
# Prob 2
#
#############

f=file.choose()
highwaysigns=read.csv(f)
attach(highwaysigns)

fit=lm(distance~age,data=highwaysigns)
summary(fit)
plot(distance~age,data=highwaysigns,pch=16,cex=2)
abline(fit,lwd=3,col="red")


new=data.frame(age = 75)
ci=predict(fit,new,interval="confidence")
ci

pi=predict(fit,new,interval="prediction")
pi


plot(distance~age,data=highwaysigns,pch=16,cex=2,ylim=c(250,600))
abline(fit,lwd=3,col="red")
lines(x=c(75,75),y=pi[2:3],lwd=3,col="blue")
lines(x=c(70,80),y=rep(pi[2],2),lwd=3,col="blue")
lines(x=c(70,80),y=rep(pi[3],2),lwd=3,col="blue")

lines(x=c(75,75),y=ci[2:3],lwd=4,col="orange")
lines(x=c(70,80),y=rep(ci[2],2),lwd=3,col="orange")
lines(x=c(70,80),y=rep(ci[3],2),lwd=3,col="orange")


resids=summary(fit)$residuals
yhats=summary(fit)$fitted.values
qqnorm(resids,cex=2,pch=16)
qqline(resids,lwd=2)
plot(resids~age,cex=2,pch=16)
abline(h=0,lwd=2)


plot(resids~yhats,cex=2,pch=16)




#############
#
# Prob 4
#
#############

f=file.choose()
malebirths=read.csv(f)
attach(malebirths)

summary(fit1<-lm(denmark~year,data=malebirths))
summary(fit2<-lm(netherlands~year,data=malebirths))
summary(fit3<-lm(canada~year,data=malebirths))
summary(fit4<-lm(usa~year,data=malebirths))

combined=c(denmark,netherlands,canada,usa)
combined=combined[!is.na(combined)]
par(mfrow=c(2,2))
ylims=c(min(combined),max(combined))
xlims=c(min(malebirths$year),max(malebirths$year))
plot(denmark~year,data=malebirths,cex=2,ylim=ylims,xlim=xlims,main="Denmark" )
plot(netherlands~year,data=malebirths,cex=2,ylim=ylims,xlim=xlims,main="Netherlands")
plot(canada~year,data=malebirths,cex=2,ylim=ylims,xlim=xlims,main="Canada") 
plot(usa~year,data=malebirths,cex=2,ylim=ylims,xlim=xlims,main="USA" )

*******************************************************************************
C:\Users\veeral\Google Drive\Harvard_Class2015\S-139-Fall\home_work\HWS139_fall_Solution\HW10 Solutions R Code.R

##############
#
# 2) Ragweed
#
##############
library(moments)

f=file.choose()
ragwort=read.csv(f)
attach(ragwort)

par(mfrow=c(2,3))
plot(mass~load,data=ragwort,cex=2,main="Y vs. X")
plot(log(mass)~load,data=ragwort,cex=2,main="log(Y) vs. X")
plot(mass~log(load),data=ragwort,cex=2,main="Y vs. log(X)")
plot(log(mass)~log(load),data=ragwort,cex=2,main="log(Y) vs. log(X)")
abline(lm(log(mass)~log(load))$coef)
plot(sqrt(mass)~log(load),data=ragwort,cex=2,main="sqrt(Y) vs. log(X)")
plot(log(mass)~sqrt(load),data=ragwort,cex=2,main="log(Y) vs. sqrt(X)")

skewness(lm(mass~load)$residuals)
skewness(lm(log(mass)~load)$residuals)
skewness(lm(mass~log(load))$residuals)
skewness(lm(log(mass)~log(load))$residuals)
skewness(lm(sqrt(mass)~log(load))$residuals)
skewness(lm(log(mass)~sqrt(load))$residuals)




##############
#
# 3) HomeSim
#
##############

f=file.choose()
homer=read.csv(f)
fit3=lm(Y~X1+X2+X3+X4+X5+X6,data=homer)
summary(fit3)
plot(fit3$resid~ fit3$fitted)
abline(h=0,lwd=2,col="red")

qqnorm(fit3$resid)
qqline(fit3$resid)


fit4=lm(log(mass)~log(load))
plot(fit3$residuals~log(load),cex=2,main="Scatterplot of Residuals vs. X")
abline(h=0,lwd=2)
hist(fit3$residuals,col="grey",main="Histrogram of Residuals")




##############
#
# 4) Exercise
#
##############

f=file.choose()
exercisedata=read.csv(f)
attach(exercisedata)

summarystats=cbind(by(exercise,classyear,length),by(exercise,classyear,mean),by(exercise,classyear,sd))
colnames(summarystats)=c("n","mean","sd")
summarystats

summary(fit4<-lm(exercise~classyear,data=exercisedata))
anova(fit4)





##############
#
# 5) ER Overcrowding
#
##############

f=file.choose()
ERdata=read.csv(f)
attach(ERdata)

#a
hist(ERvisits,col="grey",breaks=12)
hist(log(ERvisits),col="grey",breaks=12)

#b
summary(fit5b<-lm(ERvisits~Uninsured+Poverty+Unemploy+Noncitizen+Expense+Teenbirth+Smokers,data=ERdata,subset=(ERvisits<700)))

#c
summary(fit5c1<-lm(ERvisits~Uninsured+Poverty+Noncitizen+Expense+Teenbirth+Smokers,data=ERdata,subset=(ERvisits<700)))
summary(fit5c2<-lm(ERvisits~Poverty+Noncitizen+Expense+Teenbirth+Smokers,data=ERdata,subset=(ERvisits<700)))

#f
hist(resid(fit5c2),col="grey",breaks=10)
plot(resid(fit5c2)~fitted(fit5c2),cex=2)
abline(h=0)




























*******************************************************************************
