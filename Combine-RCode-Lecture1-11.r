-------------------------------------------------------------------------------

#Multiple Regression example: Harvard Sq Houses
folder="C:\\Users\\krader\\Desktop\\Stat 139 - Spring 2015\\Lectures\\Unit 11 - Multiple Regression\\"
harvarddata=read.csv(paste(folder,"harvard sq sf houses.csv",sep=""))

round(cor(harvarddata[,-1]),5)

summary(fit<-lm(price~bed+bath+size+lot+age+dist,data=harvarddata))
summary(fit0<-lm(price~size,data=harvarddata))


#ANOVA as a Regression with Binary Predictors


bonedata=read.csv(paste(folder,"bone.csv",sep=""))

anova.fit=aov(bone.density~group,data=bonedata)
summary(anova.fit)

lm.fit=lm(bone.density~group,data=bonedata)
summary(lm.fit)
summary(aov(lm.fit))


#Building towards the interaction term for weight being predicted by height and sex

textdata=read.csv(paste(folder,"stat104_survey_fall2011 - including missing values.csv",sep=""))
names(textdata)
attach(textdata)
summarystats=cbind(by(textdata$text_day,textdata$year,mean,na.rm=T),
by(!is.na(textdata$text_day),textdata$year,sum),
by(textdata$text_day,textdata$year,sd,na.rm=T))
colnames(summarystats)=c("mean","n","sd")
summarystats

summary(fit1<-lm(text_day~year,data=textdata))

summary(fit2<-lm(text_day~year+fastest,data=textdata))

summary(fit3<-lm(weight~height+female,data=textdata))

plot(weight[female==0]~height[female==0],col="blue",cex=2,main="",xlab="height",ylab="weight",ylim=c(95,250),xlim=c(50,80))
points(weight[female==1]~height[female==1],col="red",cex=2,pch="x")
abline(a=fit3$coef[1],b=fit3$coef[2],col="blue",lwd=3)
abline(a=fit3$coef[1]+fit3$coef[3],b=fit3$coef[2],col="red",lwd=3)


#Interaction Term and Plot

summary(fit4<-lm(weight~height*female,data=textdata))

plot(weight[female==0]~height[female==0],col="blue",cex=2,main="",xlab="height",ylab="weight",ylim=c(95,250),xlim=c(50,80))
points(weight[female==1]~height[female==1],col="red",cex=2,pch="x")
abline(a=fit4$coef[1],b=fit4$coef[2],col="blue",lwd=3)
abline(a=fit4$coef[1]+fit4$coef[3],b=fit4$coef[2]+fit4$coef[4],col="red",lwd=3)



#Quadratic

heightsq=height^2
summary(fit5<-lm(weight~height+heightsq,data=textdata))

fakeheight=seq(45,85,.1)
x=data.frame(height=fakeheight,heightsq=fakeheight^2)

yhat=predict(fit5,newdata=x)

plot(weight~height,cex=2,main="",ylim=c(95,250),xlim=c(50,80))
lines(yhat~fakeheight,lwd=2)

summary(fit5<-lm(weight~(height+heightsq)*female,data=textdata))





#Transformations

resids=summary(fit1)$residuals
fitted=predict(fit1)
qqnorm(resids,cex=2)
qqline(resids,lwd=3)

plot(resids~fitted,cex=2)
abline(h=0,lwd=3)

hist(text_day,col="grey",breaks=15)
ln_text=log(text_day+1)
hist(ln_text,col="grey",breaks=15)

plot(ln_text~cellphones,cex=2)
plot(ln_text~fastest_drive,cex=2)
plot(ln_text~haircut,cex=2)
plot(ln_text~senior,cex=2)

ln_haircut=log(haircut+1)
hist(haircut,col="grey",breaks=15)
hist(ln_haircut,col="grey",breaks=15)
plot(ln_text~ln_haircut,cex=2)

ln_phones=log(cellphones+1)
hist(cellphones,col="grey",breaks=15)
hist(ln_phones,col="grey",breaks=15)
plot(ln_text~ln_phones,cex=2)

summary(fit6<-lm(ln_text~ln_phones+ln_haircut+fastest_drive+senior,data=textdata))





*******************************************************************************
-------------------------------------------------------------------------------
###########
#
#
#
###########



# qqplot (slide 50)
set.seed(12345)
x=rnorm(30)
qqnorm(x,pch=16,cex=2)
qqline(x,lwd=3)


x=rt(30,df=3)
qqnorm(x,pch=16,cex=2)
qqline(x,lwd=3)


x=rt(30,df=1)
qqnorm(x,pch=16,cex=2)
qqline(x,lwd=3)



x=rexp(30)
qqnorm(x,pch=16,cex=2)
qqline(x,lwd=3)

*******************************************************************************
-------------------------------------------------------------------------------

##########
# Newton Data Set Analysis
##########

f=read.csv()
SaleData=read.csv(f)
regmodel <- lm(Price/1000 ~ Sqft., data = SaleData)
summary(regmodel)
plot(Price/1000 ~ Sqft., data = SaleData,cex=2,pch=16)
abline(regmodel,lwd=2,col="red")

cor(Price/1000, Sqft., data = SaleData)
cov(Price/1000, Sqft., data = SaleData)

mean(Price/1000)
sd(Price/1000)
mean(Sqft.)
sd(Sqft.)


#########
# Regression after standardization
#########
Sqft.std=(Sqft.-mean(Sqft.))/sd(Sqft.)
Price.std=(Price/1000-mean(Price/1000))/sd(Price/1000)
summary(lm(Price/1000 ~ Sqft., data = SaleData))
cor(Price/1000, Sqft., data = SaleData)
*******************************************************************************
-------------------------------------------------------------------------------

##########
#
# Rats' Bone Density
#
##########



f=file.choose()
data=read.csv(f)
names(data)
model1=aov(data$bone.density~data$group)
summary(model1)
anova(model1)
qf(0.95,2,27)



summarystats=cbind(by(data$bone.density,data$group,mean),
by(data$bone.density,data$group,length),
by(data$bone.density,data$group,sd))
colnames(summarystats)=c("mean","n","sd")
rownames(summarystats)=c("control","lowjump","highjump")
summarystats



##############
# Ex. 1 - No violations
##############
set.seed(12345)
nsims=10000

x=seq(-3,3,0.1)
fx=dnorm(x)
#density


ny1=10
ny2=10
ny3=10
muy1=0
muy2=0
muy3=0
vary1=1
vary2=1
vary3=1

Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rnorm(ny1,muy1,sqrt(vary1))
 y2 = rnorm(ny2,muy2,sqrt(vary2))
 y3 = rnorm(ny3,muy3,sqrt(vary3))
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)



##############
# Ex. 2 - correlated
##############

ny1=10
ny2=10
ny3=10
muy1=0
muy2=0
muy3=0
vary1=1
vary2=1
vary3=1
frac=1/3

Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rnorm(ny1,muy1,sqrt(vary1))
 y2 = rnorm(ny2,muy2,sqrt(vary2))
 y3 = y2*sqrt(frac)+rnorm(ny3,muy3,sqrt(vary3))*sqrt(1-frac)
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)


##############
# Ex. 3 - Diff Variance #1
##############

ny1=10
ny2=10
ny3=10
muy1=0
muy2=0
muy3=0
vary1=1
vary2=1
vary3=9

Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rnorm(ny1,muy1,sqrt(vary1))
 y2 = rnorm(ny2,muy2,sqrt(vary2))
 y3 = rnorm(ny3,muy3,sqrt(vary3))
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)



##############
# Ex. 4 - Diff Variance #2
##############

ny1=50
ny2=50
ny3=10
muy1=0
muy2=0
muy3=0
vary1=1
vary2=1
vary3=9

Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rnorm(ny1,muy1,sqrt(vary1))
 y2 = rnorm(ny2,muy2,sqrt(vary2))
 y3 = rnorm(ny3,muy3,sqrt(vary3))
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)


##############
# Ex. 5 - Diff Variance #3
##############

ny1=50
ny2=50
ny3=10
muy1=0
muy2=0
muy3=0
vary1=1
vary2=1
vary3=1/9

Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rnorm(ny1,muy1,sqrt(vary1))
 y2 = rnorm(ny2,muy2,sqrt(vary2))
 y3 = rnorm(ny3,muy3,sqrt(vary3))
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 6 - Skewed
##############

x=seq(-1,10,0.01)
fx=dexp(x+1,1)
r=rexp(100000,1)-1
hist(r,col="gray",main="Expo(1)-1",cex.main=3,freq=F)
lines(fx~x,type="l",lwd=2,col="blue")

#density

ny1=10
ny2=10
ny3=10
lambda1=1
lambda2=1
lambda3=1


Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rexp(ny1,lambda1)-1
 y2 = rexp(ny2,lambda2)-1
 y3 = rexp(ny3,lambda3)-1
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)


##############
# Ex. 7 - Skewed
##############

ny1=10
ny2=10
ny3=50
muy1=0
muy2=0
vary1=1
vary2=1
lambda3=1

Fstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 y1 = rnorm(ny1,muy1,sqrt(vary1))
 y2 = rnorm(ny2,muy2,sqrt(vary2))
 y3 = rexp(ny3,lambda3)-1
 y = c(y1,y2,y3)
 varbetween = (ny1*(mean(y1)-mean(y))^2+ny2*(mean(y2)-mean(y))^2+ny3*(mean(y3)-mean(y))^2)/(3-1)
 varwithin = ((ny1-1)*var(y1)+(ny2-1)*var(y2)+(ny3-1)*var(y3))/(ny1+ny2+ny3-3)
 Fstats[i] = varbetween/varwithin
 pvals[i] = 1-pf(Fstats[i],df1=3-1,df2=ny1+ny2+ny3-3)
}


hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)


##############
# Multi-way Example
##############

f=file.choose()
texting=read.csv(f)
names(texting)
attach(texting)
logtexts=log(text_day+1)
model1=aov(logtexts~female)
model2=aov(logtexts~classyear)
model3=aov(logtexts~classyear+female)
model4=aov(logtexts~classyear*female)

anova(model1)
anova(model2)
anova(model3)
anova(model4)


##########
# Kruskal-Wallis example
##########
ranks=rank(logtexts)
ni=as.vector(table(classyear))
ribar=as.vector(by(ranks,classyear,mean))
rbar=mean(ranks)
N=length(ranks)
I=length(ni)
K=(N-1)*sum(ni*(ribar-rbar)^2)/sum((ranks-rbar)^2)
K
1-pchisq(K,df=I-1)

kruskal.test(logtexts~classyear)




##############
# Acne and Jelly bean color
##############
set.seed(12345)
jellybeancolors=c("purple","brown","pink","blue","teal","salmon","red","turquoise","magenta","yellow","gey","tan","green","cyan","mauve","beige","lilac","black","peach","orange")
ncolors=length(jellybeancolors)
color=rep(jellybeancolors,10)
n=length(color)
acne=rnorm(n)
anova(aov((acne)~color))

boxplot((acne)~color)

by((acne),color,mean)

t.test(acne[color=="green"],acne[color=="yellow"])

##############
# let's look at all the pairwise t-tests
##############
pvalues=rep(NA,choose(ncolors,2))
counter=1
for(i in 1:(ncolors-1)){
   for(j in (i+1):(ncolors)){
      pvalues[counter]=t.test(acne[color==jellybeancolors[i]],acne[color==jellybeancolors[j]])$p.value
      counter=counter+1
   }
}
hist(pvalues)
sum(pvalues<0.05)

*******************************************************************************
-------------------------------------------------------------------------------
library(coin)
set.seed(02232015)
f=file.choose()
oring=read.csv(f)
names(oring)
attach(oring)
damage=1*(oring_damage>0)
n=length(temp)


#Doing the Rank Sum test manually
ranks=rank(temp)

V=sum(ranks[damage==0])
n0=sum(damage==0)
V.expected=n0*sum(ranks)/n

#Based on a permutation test
nsims=10000
V.sims=rep(NA,nsims)
for(i in 1:nsims){
 V.sims[i]=sum(sample(ranks,n0))
}
hist(V.sims)
(sum(V.sims>V)+sum(V.sims<V.expected-(V-V.expected)))/nsims



#Wilcoxon tests using the approximate distribution, done two different ways

wilcox_test(temp ~ as.factor(damage),data=oring, distribution = "approximate", conf.int = TRUE)
wilcox.test(temp ~ damage,alternative="two.sided", exact=FALSE, conf.int = TRUE)








f=file.choose()
moon=read.csv(f)
names(moon)
attach(moon)

#Doing the signed-Rank test manually
aggdiff.sort=sort(aggdiff)
ranks=tank(aggdiff.sort)
V=sum(ranks[aggdiff.sort>0])



wilcox.test(aggmoon,aggother,paired = TRUE)
S=sum(aggdiff>0)
n=length(aggdiff>0)
binom.test(S,n)

2*(1-pbinom(S-1,n,0.5))

*******************************************************************************
-------------------------------------------------------------------------------

##########
#
# Illustrations of Power
#
##########


nsims=10000

##############
# Ex. 1 - Both Standard Normal
##############

x=seq(-10,10,0.1)
fx=dnorm(x)
#den

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)



##############
# Ex. 2 - One Big Sample, One Medium
##############

nx=50
ny=500
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)



##############
# Ex. 3 - Small Sample Sizes
##############

nx=5
ny=5
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 3 - Skewed Normal
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (rsn(nx,alpha=4)-sqrt(32/17/pi))/sqrt(1-32/17/pi)
 y = (rsn(ny,alpha=4)-sqrt(32/17/pi))/sqrt(1-32/17/pi)
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 4 - Opposite-Skewed Normal
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (rsn(nx,alpha=4)-sqrt(32/17/pi))/sqrt(1-32/17/pi)
 y = (rsn(ny,alpha=-4)+sqrt(32/17/pi))/sqrt(1-32/17/pi)
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 5 - Expo vs. Unif
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (runif(nx)-0.5)*sqrt(12)
 y = rexp(ny,1)-1
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 6 - Expo vs. Unif - Permutation test
##############

nx=500
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (runif(nx)-0.5)*sqrt(12)
 y = rexp(ny,1/3)-3
 response=c(x,y)
 groups=c(rep(0,nx),rep(1,ny))

#initialize the vector to store the simulated 

numsims=1000
ybardiff.sim=rep(NA,numsims)

#and the for loop to do all the work: reordering the x's, and splitting into groups
for(j in 1:numsims){
  groups.sim = sample(groups)
  ybardiff.sim[j]=mean(response[groups.sim==0])-mean(response[groups.sim==1])
}

#two-sided p-value
pvals[i] = mean( abs(ybardiff.sim) >= abs(mean(x)-mean(y)))


}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)













##############
# Ex. 7 - Varying Variance
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)








##############
# Ex. 8 - Varying Variance w/ more obs's in large variance group
##############

nx=50
ny=500
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)





##############
# Ex. 9 - Varying Variance w/ more obs's in small variance group
##############

nx=500
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)





##############
# Ex. 10 - Unpooled Test!
##############

nx=500
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 tstats[i] = (mean(x)-mean(y))/(sqrt(var(x)/nx+var(y)/ny))
 satterthwaite=(var(x)/nx+var(y)/ny)^2/(var(x)^2/nx^2/(nx-1)+var(y)^2/ny^2/(ny-1))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=satterthwaite))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)







##########
#
# Power Curves
#
##########

##############
# 2-sample
##############

time1=Sys.time()
nsims=2000

nx=10
ny=10
mux=0
muy=seq(-2,2,0.02)
varx=1
vary=1
alpha=0.05

t.pooled.power=t.unpooled.power=ranksum.power=signtest.power=normaltest.power=rep(NA,length(muy))


for(j in 1:length(muy)){
  t.pooled.pval=t.unpooled.pval=ranksum.pval=signtest.pval=normaltest.pval=rep(NA,nsims)
  for(i in 1:nsims){
    x = rnorm(nx,mux,sqrt(varx))
    y = rnorm(ny,muy[j],sqrt(vary))
    sp=sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
    tstat.pooled= (mean(x)-mean(y))/(sp*sqrt(1/nx+1/ny))
    t.pooled.pval[i] = 2*(1-pt(abs(tstat.pooled),df=nx+ny-2))

    tstat.unpooled= (mean(x)-mean(y))/(sqrt(var(x)/nx+var(y)/ny))
    satterthwaite=(var(x)/nx+var(y)/ny)^2/(var(x)^2/nx^2/(nx-1)+var(y)^2/ny^2/(ny-1))
    t.unpooled.pval[i] = 2*(1-pt(abs(tstat.unpooled),df=satterthwaite))

    combined=c(x,y)
    ranks=rank(combined)
    Rbar=mean(ranks)
    S.R2=var(ranks)
    V=sum(ranks[1:nx])
    Z=(V-Rbar*nx)/sqrt(S.R2*(nx*ny)/(nx+ny))
    ranksum.pval[i] = 2*(1-pnorm(abs(Z)))

    signtest.pval[i]=fisher.test(combined>=median(combined),c(rep(1,nx),rep(0,ny)))$p.value
    
    #Ignore this method
    #zscores=qqnorm(combined,plot.it=F)$x
    #spz=sqrt(((nx-1)*var(zscores[1:nx])+(ny-1)*var(zscores[(nx+1):(nx+ny)]))/(nx+ny-2))
    #zscores.pooled=(mean(zscores[1:nx])-mean(zscores[(nx+1):(nx+ny)]))/(spz*sqrt(1/nx+1/ny))
    #normaltest.pval[i]=2*(1-pt(abs(zscores.pooled),df=nx+ny-2))
  }
  t.pooled.power[j]=mean(t.pooled.pval<alpha)
  t.unpooled.power[j]=mean(t.unpooled.pval<alpha)
  ranksum.power[j]=mean(ranksum.pval<alpha)
  signtest.power[j]=mean(signtest.pval<alpha)
  #normaltest.power[j]=mean(normaltest.pval<alpha)
}


plot(t.unpooled.power~muy,col="red",lwd=3,ylab="Power",xlab="mu_y-mu_x",type="l")
lines(ranksum.power~muy,col="green",lwd=3)
lines(signtest.power~muy,col="blue",lwd=3)
lines(t.pooled.power~muy,lwd=3,xlab="Power")
abline(h=0.05,lty=2,lwd=2,col="gray")
#lines(normaltest.power~muy,col="darkorange",lwd=3)

time2=Sys.time()
time2-time1

##############
# paired
##############

time1=Sys.time()
nsims=2000

nx=ny=n=10
mux=0
muy=seq(-2,2,0.02)
varx=1
vary=1
alpha=0.05
frac=1/3
t.paired.power=t.unpooled.power=signedrank.power=signtest.power=rep(NA,length(muy))


for(j in 1:length(muy)){
  t.paired.pval=t.unpooled.pval=signedrank.pval=signtest.pval=rep(NA,nsims)
  for(i in 1:nsims){
    x = rnorm(nx,mux,sqrt(varx))
    y = x*sqrt(frac)+rnorm(ny,muy[j],sqrt(vary))*sqrt(1-frac)
    diff = y-x

    tstat.paired= mean(diff)/sqrt(var(diff)/n)
    t.paired.pval[i] = 2*(1-pt(abs(tstat.paired),df=n-1))

    tstat.unpooled= (mean(x)-mean(y))/(sqrt(var(x)/nx+var(y)/ny))
    satterthwaite=(var(x)/nx+var(y)/ny)^2/(var(x)^2/nx^2/(nx-1)+var(y)^2/ny^2/(ny-1))
    t.unpooled.pval[i] = 2*(1-pt(abs(tstat.unpooled),df=satterthwaite))

    ranks = rank(abs(diff))
    W = sum(ranks[diff<0])
    m = sum(diff != 0)
    Z=(W-m*(m+1)/4)/sqrt(m*(m+1)*(2*m+1)/24)
    signedrank.pval[i] = 2*(1-pnorm(abs(Z)))

    X = sum(diff > 0)
    if(X > 0.5*n){signtest.pval[i] = 2*(1-pbinom(X-1,m,0.5))  } 
    if(X <= 0.5*n){signtest.pval[i] = 2*pbinom(X,m,0.5) } 

  }
  t.paired.power[j]=mean(t.paired.pval<alpha)
  t.unpooled.power[j]=mean(t.unpooled.pval<alpha)
  signedrank.power[j]=mean(signedrank.pval<alpha)
  signtest.power[j]=mean(signtest.pval<alpha)
}


plot(t.unpooled.power~muy,col="red",lwd=3,ylab="Power",xlab="mu_y-mu_x",type="l")
lines(signedrank.power~muy,col="green",lwd=3)
lines(signtest.power~muy,col="blue",lwd=3)
lines(t.paired.power~muy,lwd=3,xlab="Power")
abline(h=0.05,lty=2,lwd=2,col="gray")

x11()
plot(t.paired.power-signedrank.power~muy,type="l",lwd=2)

time2=Sys.time()
time2-time1
*******************************************************************************
-------------------------------------------------------------------------------
############################
#
# Download and collect the weather data
#
############################

###########################
# Download data with a for loop :)
###########################

data.list=list()
data.matrix=matrix(NA,nrow=0,ncol=25)
years=2013:2015

for(i in 1:length(years)){
  year=years[i]
  link=paste("http://www.wunderground.com/history/airportfrompws/KBOS/",year,"/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=",year,"&req_city=NA&req_state=NA&req_statename=NA&format=1",sep="")
  data=cbind(year=year,read.csv(link,header=T))
  names(data)[2]="Date"
  data.list[[i]]=data
  data.matrix=rbind(data.matrix,data)

}

data=data.matrix[!is.na(data.matrix$Max.TemperatureF),]
data$Date=as.Date(data$Date)
id=data$id=as.numeric(data$Date)
data$month=as.numeric(substring(data$Date,6,7))
data$year=as.numeric(substring(data$Date,1,4))

sapply(data.list,nrow)

maxtemp=data$maxtemp=data$Max.TemperatureF 
mintemp=data$mintemp=data$Min.TemperatureF 
meantemp=data$meantemp=data$Mean.TemperatureF 


##################
#Need to do some data cleaning
##################

data$day=day=id-min(id)+1
attach(data)
data$date=data$Date
data2=data.frame(data$year,data$date,data$maxtemp,data$day)

names(data2)=c("year","date","maxtemp","day")
dim(data2)

which(data$maxtemp < (-100))

names(data2)=c("year","date","maxtemp","day")
data3=data2[!(data2$maxtemp<= -20),]
names(data3)=c("year","date","maxtemp","day")
dim(data3)

write.csv(data3,"C:\\Users\\krader\\Desktop\\Stat E139 - Fall 2015\\Datasets\\Boston Weather - since 2013.csv",row.names=F,quote=F)



############
# Reading in the file after it is saved
############
f = file.choose()
data = read.csv(f)
n = dim(data)[1] 
data$date=as.character(data$date)

summer15=data[data$date >= "2015-06-01" & data$date <= "2015-08-31",]
summer14=data[data$date >= "2014-06-01" & data$date <= "2014-08-31",]

# Visualize the data
boxplot(summer14$maxtemp,summer15$maxtemp,col=c("rosybrown","green3"))
# As a 2-sample unpooled t-test
t.test(summer14$maxtemp,summer15$maxtemp)
# As a 2-sample pooled t-test
t.test(summer14$maxtemp, summer15$maxtemp, var.equal=T)
# As a 2-sample paired t-test
 t.test(summer14$maxtemp, summer15$maxtemp, paired=T)

# As a Rank Sum test
w.test=wilcox.test(summer14$maxtemp, summer15$maxtemp)
w.test

# As a Resampled/Premutation test
diff.obs=mean(summer14$maxtemp)-mean(summer15$maxtemp)
combined.sample=c(summer14$maxtemp, summer15$maxtemp)

nsims=10000
diff.sim=rep(NA,nsims)
for(i in 1:nsims){
resampled.temp=sample(combined.sample,length(combined.sample))
diff.sim[i]=mean(resampled.temp[1:length(summer14$maxtemp)])-mean(resampled.temp[(length(summer14$maxtemp)+1):length(combined.sample)])
}
mean(abs(diff.sim)>abs(diff.obs))
*******************************************************************************
-------------------------------------------------------------------------------
##############
#
# Central Limit Theorem Simulation
#
##############


set.seed(12345)

#x=0:200/50
x=0:200/200


#Bathtub
fx=dbeta(x,shape1=1/2,shape2=1/2)

#Exponential
#fx=dexp(x,rate=1)

#Standard Normal
#fx=dnorm(x,mean=0,sd=1)

plot(x,fx,type="l",lwd=2)



n = samplesize = 30
nsims = 2000
xbars=rep(NA,nsims)

for(i in 1:nsims){
 sample=rbeta(n,shape1=1/2,shape2=1/2)
 #sample=rexp(n,rate=1)
 #sample=rnorm(n,mean=0,sd=1)
 xbars[i]=mean(sample)
}


par(mfrow=c(2,1))
plot(x,fx,type="l",lwd=3,col="red")
hist(xbars,col="grey",breaks=25)

set.seed(07041776) 
x = rnorm(1000000, mean=1, sd=2)
y = x^2
mean(y)
var(y)
mean(y>5)
*******************************************************************************
-------------------------------------------------------------------------------
####################################
# Performing Fisher's Randomization 
# Test for the Malaraia data
####################################

#Create the variables in the data set
x=c(rep(1,9),rep(0,12))
y=c(rep(1,3),rep(0,6),rep(1,10),rep(0,2))
n=length(x)

#split into two groups and calculate the test statistics
y.v=y[x==1]
y.c=y[x==0]

ybardiff.obs=mean(y.c)-mean(y.v)


#initialize the vector to store the simulated 
nsims=10000
ybardiff.sim=rep(NA,nsims)


#and the for loop to do all the work: reordering the x's, and splitting into groups
for(i in 1:nsims){
  x.sim = sample(x)
  ybardiff.sim[i]=mean(y[x.sim==0])-mean(y[x.sim==1])
}

mean(ybardiff.sim)
var(ybardiff.sim)

hist(ybardiff.sim, col="grey")
hist(ybardiff.sim, col="grey",breaks=c((-10):10/10-0.001))
abline(v=ybardiff.obs,col="red",lwd=2)

#one-sided p-value
mean(ybardiff.sim >= ybardiff.obs)
#two-sided p-value
mean( abs(ybardiff.sim) >= abs(ybardiff.obs))
*******************************************************************************
-------------------------------------------------------------------------------
##############
# Financial Aid Data
##############
f=file.choose()
financial=read.csv(f)
dim(financial)

hist(financial$aid,col="olivegreen",breaks=10)


t.test(financial$aid,mu=24000)



##############
# Birthweight Data
##############
f=file.choose()
bw=read.csv(f)
dim(bw)

boxplot(bw$bwt~bw$smoke,col="chartreuse")
cbind(by(bw$bwt,bw$smoke,mean),by(bw$bwt,bw$smoke,sd),by(bw$bwt,bw$smoke,length))
  
t.test(bw2$bwt~bw2$smoke)
t.test(bw2$bwt~bw2$smoke,var.equal=T)


##############
# Full Moon & Dementia Data
##############
f=file.choose()
dem=read.csv(f)
dim(dem)

dem$diff=dem$aggmoon - dem$aggother

hist(dem$diff,col="lightblue")

summary(dem$diff)
mean(dem$diff)
sd(dem$diff)
length(dem$diff)
sum(is.na(dem$diff))

t.test(dem$aggmoon,dem$aggother,paired=T)
t.test(dementia$diff,mu=0)
*******************************************************************************
-------------------------------------------------------------------------------
install.packages("sn")
library(sn)



f=file.choose()
data=read.csv(f)

x <- data$tuition[data$public==1]
y <- data$tuition[data$public==0]

hist(x, col="light gray", main="Public", xlab="Tuition, in $1000", prob=TRUE, breaks  = 10,cex.main=2,cex.lab=1.5)
# Kernel density
lines(density(x, adjust = 2), col = "blue", lwd=2)
# Approx. normal curve, fitted using MOM
points(seq(min(x), max(x), length.out=500),  dnorm(seq(min(x), max(x), length.out=500),           mean(x), sd(x)), type="l", col="red", lwd=2, lty=2)


hist(y, col="light gray", main="Private", xlab="Tuition, in $1000", prob=TRUE, breaks  = 10,cex.main=2,cex.lab=1.5)

# Kernel density
lines(density(y, adjust = 2), col = "blue", lwd=2)
# Approx. normal curve, fitted using MOM
points(seq(min(y), max(y), length.out=500),  dnorm(seq(min(y), max(y), length.out=500),           mean(x), sd(x)), type="l", col="red", lwd=2, lty=2)



boxplot(x,main="Public", xlab="Tuition",cex.main=2,cex.lab=1.5,col="light gray")
boxplot(y,main="Private", xlab="Tuition",cex.main=2,cex.lab=1.5,col="light gray")

boxplot(data$tuition~data$public,main="Tuition Split by \n 1=Public and 0=Private",col=c("red","blue"))

var.test(data$tuition~data$public)
levene.test(data$tuition~data$public)

##############
#
# QQ Plots
#
##############

qqnorm(x,main="Public",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(x,lwd=3)

qqnorm(y,main="Private",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(y,lwd=3)

set.seed(2172015)
x=rnorm(20)
qqnorm(x,main="Normal Dist.(n = 20)",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(x,lwd=3)

y=rnorm(100)
qqnorm(y,main="Normal Dist. (n = 100)",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(y,lwd=3)

set.seed(2182015)
x=rchisq(20,df=3)
qqnorm(x,main="Normal Dist.(n = 20)",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(x,lwd=3)

y=rchisq(100,df=3)
qqnorm(y,main="Normal Dist. (n = 100)",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(y,lwd=3)


par(mfrow=c(3,3))
for(i in 1:9){
 sample=rnorm(20)
 qqnorm(sample,pch=16)
 qqline(sample)
}



x=rnorm(20)
qqnorm(x,main="Normal Dist.(n = 20)",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(x,lwd=3)

y=rnorm(100)
qqnorm(y,main="Normal Dist. (n = 100)",pch=16,cex=2,cex.main=2,cex.lab=1.5)
qqline(y,lwd=3)


##########
#
# Illustrations of Assumption Violations
#
##########


nsims=10000

##############
# Ex. 1 - Both Standard Normal
##############

x=seq(-10,10,0.1)
fx=dnorm(x)
#den

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)



##############
# Ex. 2 - One Big Sample, One Medium
##############

nx=50
ny=500
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)



##############
# Ex. 3 - Small Sample Sizes
##############

nx=5
ny=5
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 3 - Skewed Normal
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (rsn(nx,alpha=4)-sqrt(32/17/pi))/sqrt(1-32/17/pi)
 y = (rsn(ny,alpha=4)-sqrt(32/17/pi))/sqrt(1-32/17/pi)
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 4 - Opposite-Skewed Normal
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (rsn(nx,alpha=4)-sqrt(32/17/pi))/sqrt(1-32/17/pi)
 y = (rsn(ny,alpha=-4)+sqrt(32/17/pi))/sqrt(1-32/17/pi)
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 5 - Expo vs. Unif
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (runif(nx)-0.5)*sqrt(12)
 y = rexp(ny,1)-1
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 6 - Expo vs. Unif - Small Samples
##############

nx=10
ny=30
mux=0
muy=0
varx=1
vary=1

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = (runif(nx)-0.5)*sqrt(12)
 y = rexp(ny,1)-1
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)




##############
# Ex. 7 - Varying Variance
##############

nx=50
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)








##############
# Ex. 8 - Varying Variance w/ more obs's in large variance group
##############

nx=50
ny=500
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)





##############
# Ex. 9 - Varying Variance w/ more obs's in small variance group
##############

nx=500
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 spooled= sqrt(((nx-1)*var(x)+(ny-1)*var(y))/(nx+ny-2))
 tstats[i] = (mean(x)-mean(y))/(spooled*sqrt(1/nx+1/ny))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=nx+ny-2))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)





##############
# Ex. 10 - Unpooled Test!
##############

nx=500
ny=50
mux=0
muy=0
varx=1
vary=9

tstats=pvals=rep(NA,nsims)

for(i in 1:nsims){
 x = rnorm(nx,mux,sqrt(varx))
 y = rnorm(ny,muy,sqrt(vary))
 tstats[i] = (mean(x)-mean(y))/(sqrt(var(x)/nx+var(y)/ny))
 satterthwaite=(var(x)/nx+var(y)/ny)^2/(var(x)^2/nx^2/(nx-1)+var(y)^2/ny^2/(ny-1))
 pvals[i] = 2*(1-pt(abs(tstats[i]),df=satterthwaite))
}

hist(pvals,col="pink",xlab="two-sided p-value")
abline(v=0.05,col="red",lwd=3)
mean(pvals<0.05)









*******************************************************************************
