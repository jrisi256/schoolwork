##########################################################################################################################################################
#Replication file for Ayers, Hofstetter, Schnakenberg, and Kolody "Is Immigration a Racial Issue? Anglos'Attitudes on Immigraton Policies in a 
#Border County" Social Science Quarterly, 2009
##########################################################################################################################################################

library(here)
library(foreign)
library(Zelig)
ssq <- read.dta(here("hw1/data/ssq.dta"))

##Table 3, column 1, legal immigration

X <- as.data.frame(cbind(ssq$immatt, ssq$dishis2, ssq$pcthis2, ssq$contact, ssq$retecon, ssq$idea, ssq$edu2, ssq$income2, ssq$age, ssq$male, ssq$wteth)) #data frame
names(X) <- c("immatt", "dishis2", "pcthis2", "contact", "retecon", "idea", "edu2", "income2", "age", "male", "wteth")
X <- na.omit(X) #listwise deletion
#ordered logit model
immatt.model <- zelig(as.factor(immatt) ~ dishis2 + pcthis2 + contact + retecon + idea + edu2 + income2+  age+  male, weights=X$wteth, model="ologit", data=X) 

##Table 3, column 2, mexican immigration

X1 <- as.data.frame(cbind(ssq$mexiatt, ssq$dishis2, ssq$pcthis2, ssq$contact, ssq$retecon, ssq$idea, ssq$edu2, ssq$income2, ssq$age, ssq$male, ssq$wteth)) #data frame
names(X1) <- c("mexiatt", "dishis2", "pcthis2", "contact", "retecon", "idea", "edu2", "income2", "age", "male", "wteth")
X1 <- na.omit(X1) #listwise deletion
#ordered logit
mexiatt.model <- zelig(as.factor(mexiatt) ~ dishis2 + pcthis2 + contact + retecon + idea + edu2 + income2+  age+  male, weights=X1$wteth, model="ologit", data=X1)

##Table 3, column 3, amnesty
X2 <- as.data.frame(cbind(ssq$amnesty, ssq$dishis2, ssq$pcthis2, ssq$contact, ssq$retecon, ssq$idea, ssq$edu2, ssq$income2, ssq$age, ssq$male, ssq$wteth)) #data frame
names(X2) <- c("amnesty", "dishis2", "pcthis2", "contact", "retecon", "idea", "edu2", "income2", "age", "male", "wteth")
X2 <- na.omit(X2) #listwise deletion

amnesty.model <- zelig(amnesty ~ dishis2 + pcthis2 + contact + retecon + idea + edu2 + income2+  age+  male, weights=X2$wteth, model="logit", data=X2) # logit model
amnesty.model <- glm(amnesty ~
                        dishis2 +
                        pcthis2 +
                        contact +
                        retecon +
                        idea +
                        edu2 +
                        income2 +
                        age +
                        male,
                     weights = X2$wteth,
                     data = X2,
                     family = "binomial")

# Set values for simulated effects of Latino context (Figure 1, panel B)
#for legal immigration
immatt.pcthis2.set1 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.09842) 
immatt.pcthis2.set2 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.23727) 
immatt.pcthis2.set3 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.35661) 
immatt.pcthis2.setmin <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.01527) 
immatt.pcthis2.setmax <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.6735) 
#for mexican immigration
mexiatt.pcthis2.set1 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.09842) 
mexiatt.pcthis2.set2 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.23727) 
mexiatt.pcthis2.set3 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.35661) 
mexiatt.pcthis2.setmin <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.01527) 
mexiatt.pcthis2.setmax <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.6735) 
#for amnesty
amnesty.pcthis2.set1 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.09842) 
amnesty.pcthis2.set2 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.23727) 
amnesty.pcthis2.set3 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.35661) 
amnesty.pcthis2.setmin <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.01527) 
amnesty.pcthis2.setmax <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), pcthis2=0.6735) 

# Set values for simulated effects of minority contact (Figure 1, Panel C)

#legal immigration
immatt.contact.set1 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-1.6189) 
immatt.contact.set2 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-0.3895) 
immatt.contact.set3 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=2.1008) 
immatt.contact.setmin <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-5.338) 
immatt.contact.setmax <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=5.82) 
#mexican immigration
mexiatt.contact.set1 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-1.6189) 
mexiatt.contact.set2 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-0.3895) 
mexiatt.contact.set3 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=2.1008) 
mexiatt.contact.setmin <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-5.338) 
mexiatt.contact.setmax <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), contact=5.82) 
#amnesty
amnesty.contact.set1 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-1.6189) 
amnesty.contact.set2 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-0.3895) 
amnesty.contact.set3 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), contact=2.1008) 
amnesty.contact.setmin <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), contact=-5.338) 
amnesty.contact.setmax <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), contact=5.82) 

# Set values for simulated effects of Latino aversion (Figure 1, Panel A)

#legal immigration
immatt.dishis2.set0 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), dishis2=0) 
immatt.dishis2.set1 <- setx(immatt.model, fn=list(numeric = mean, ordered = median, others = mode), dishis2=1) 
#mexican immigration
mexiatt.dishis2.set0 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), dishis2=0) 
mexiatt.dishis2.set1 <- setx(mexiatt.model, fn=list(numeric = mean, ordered = median, others = mode), dishis2=1) 
#amnesty
amnesty.dishis2.set0 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), dishis2=0) 
amnesty.dishis2.set1 <- setx(amnesty.model, fn=list(numeric = mean, ordered = median, others = mode), dishis2=1) 

# Simulations for latino context (Figure 1, Panel B)
#legal immigration, 25th percentile for latino context
immatt.pcthis2.sim1 <- sim(immatt.model, immatt.pcthis2.set1) 
immatt.pcthis2.ev1 <- as.data.frame(immatt.pcthis2.sim1$qi$ev)
names(immatt.pcthis2.ev1) <- c("one", "two", "three")
#legal immigration, median latino context
immatt.pcthis2.sim2 <- sim(immatt.model, immatt.pcthis2.set2)
immatt.pcthis2.ev2 <- as.data.frame(immatt.pcthis2.sim2$qi$ev)
names(immatt.pcthis2.ev2) <- c("one", "two", "three")
#legal immigration, 75th percentile for latino context
immatt.pcthis2.sim3 <- sim(immatt.model, immatt.pcthis2.set3)
immatt.pcthis2.ev3 <- as.data.frame(immatt.pcthis2.sim3$qi$ev)
names(immatt.pcthis2.ev3) <- c("one", "two", "three")
#legal immigration, minimum level of latino context
immatt.pcthis2.simmin <- sim(immatt.model, immatt.pcthis2.setmin)
immatt.pcthis2.evmin <- as.data.frame(immatt.pcthis2.simmin$qi$ev)
names(immatt.pcthis2.evmin) <- c("one", "two", "three")
#legal immigration, maximum level of latino context
immatt.pcthis2.simmax <- sim(immatt.model, immatt.pcthis2.setmax)
immatt.pcthis2.evmax <- as.data.frame(immatt.pcthis2.simmax$qi$ev)
names(immatt.pcthis2.evmax) <- c("one", "two", "three")

#mexican immigration, 25th percentile for latino context
mexiatt.pcthis2.sim1 <- sim(mexiatt.model, mexiatt.pcthis2.set1)
mexiatt.pcthis2.ev1 <- as.data.frame(mexiatt.pcthis2.sim1$qi$ev)
names(mexiatt.pcthis2.ev1) <- c("one", "two", "three")
#mexican immigration, median latino context
mexiatt.pcthis2.sim2 <- sim(mexiatt.model, mexiatt.pcthis2.set2)
mexiatt.pcthis2.ev2 <- as.data.frame(mexiatt.pcthis2.sim2$qi$ev)
names(mexiatt.pcthis2.ev2) <- c("one", "two", "three")
#mexican immigration, 75th percentile for latino context
mexiatt.pcthis2.sim3 <- sim(mexiatt.model, mexiatt.pcthis2.set3)
mexiatt.pcthis2.ev3 <- as.data.frame(mexiatt.pcthis2.sim3$qi$ev)
names(mexiatt.pcthis2.ev3) <- c("one", "two", "three")
#mexican immigration, minimum level of latino context
mexiatt.pcthis2.simmin <- sim(mexiatt.model, mexiatt.pcthis2.setmin)
mexiatt.pcthis2.evmin <- as.data.frame(mexiatt.pcthis2.simmin$qi$ev)
names(mexiatt.pcthis2.evmin) <- c("one", "two", "three")
#mexican immigration, maximum level of latino context
mexiatt.pcthis2.simmax <- sim(mexiatt.model, mexiatt.pcthis2.setmax)
mexiatt.pcthis2.evmax <- as.data.frame(mexiatt.pcthis2.simmax$qi$ev)
names(mexiatt.pcthis2.evmax) <- c("one", "two", "three")

#amnesty, 25th percentile for latino context
amnesty.pcthis2.sim1 <- sim(amnesty.model, amnesty.pcthis2.set1)
#amnesty, median latino context
amnesty.pcthis2.sim2 <- sim(amnesty.model, amnesty.pcthis2.set2)
#amnesty, 75th percentile for latino context
amnesty.pcthis2.sim3 <- sim(amnesty.model, amnesty.pcthis2.set3)
#amnesty, minimum level of latino context
amnesty.pcthis2.simmin <- sim(amnesty.model, amnesty.pcthis2.setmin)
#amnesty, maximum level of latino context
amnesty.pcthis2.simmax <- sim(amnesty.model, amnesty.pcthis2.setmax)

# Simulated effects of minority contact (figure 1, panel C)

#legal immigration, 25th percentile for minority contact
immatt.contact.sim1 <- sim(immatt.model, immatt.contact.set1)
immatt.contact.ev1 <- as.data.frame(immatt.contact.sim1$qi$ev)
names(immatt.contact.ev1) <- c("one", "two", "three")
#legal immigration, median minority contact
immatt.contact.sim2 <- sim(immatt.model, immatt.contact.set2)
immatt.contact.ev2 <- as.data.frame(immatt.contact.sim2$qi$ev)
names(immatt.contact.ev2) <- c("one", "two", "three")
#legal immigration, 75th percentile for minority contact
immatt.contact.sim3 <- sim(immatt.model, immatt.contact.set3)
immatt.contact.ev3 <- as.data.frame(immatt.contact.sim3$qi$ev)
names(immatt.contact.ev3) <- c("one", "two", "three")
#legal immigration, minimum minority contact
immatt.contact.simmin <- sim(immatt.model, immatt.contact.setmin)
immatt.contact.evmin <- as.data.frame(immatt.contact.simmin$qi$ev)
names(immatt.contact.evmin) <- c("one", "two", "three")
#legal immigration, maximum minority contact
immatt.contact.simmax <- sim(immatt.model, immatt.contact.setmax)
immatt.contact.evmax <- as.data.frame(immatt.contact.simmax$qi$ev)
names(immatt.contact.evmax) <- c("one", "two", "three")

#mexican immigration, 25th percentile for minority contact
mexiatt.contact.sim1 <- sim(mexiatt.model, mexiatt.contact.set1)
mexiatt.contact.ev1 <- as.data.frame(mexiatt.contact.sim1$qi$ev)
names(mexiatt.contact.ev1) <- c("one", "two", "three")
#mexican immigration, median minority contact
mexiatt.contact.sim2 <- sim(mexiatt.model, mexiatt.contact.set2)
mexiatt.contact.ev2 <- as.data.frame(mexiatt.contact.sim2$qi$ev)
names(mexiatt.contact.ev2) <- c("one", "two", "three")
#mexican immigration, 75th percentile for minority contact
mexiatt.contact.sim3 <- sim(mexiatt.model, mexiatt.contact.set3)
mexiatt.contact.ev3 <- as.data.frame(mexiatt.contact.sim3$qi$ev)
names(mexiatt.contact.ev3) <- c("one", "two", "three")
#mexican immigration, minimum minority contact
mexiatt.contact.simmin <- sim(mexiatt.model, mexiatt.contact.setmin)
mexiatt.contact.evmin <- as.data.frame(mexiatt.contact.simmin$qi$ev)
names(mexiatt.contact.evmin) <- c("one", "two", "three")
#mexican immigration, maximum minority contact
mexiatt.contact.simmax <- sim(mexiatt.model, mexiatt.contact.setmax)
mexiatt.contact.evmax <- as.data.frame(mexiatt.contact.simmax$qi$ev)
names(mexiatt.contact.evmax) <- c("one", "two", "three")

#amnesty, 25th percentile for minority contact
amnesty.contact.sim1 <- sim(amnesty.model, amnesty.contact.set1)
#amnesty, median minority contact
amnesty.contact.sim2 <- sim(amnesty.model, amnesty.contact.set2)
#amnesty, 75th percentile minority contact
amnesty.contact.sim3 <- sim(amnesty.model, amnesty.contact.set3)
#amnesty, minimum minority contact
amnesty.contact.simmin <- sim(amnesty.model, amnesty.contact.setmin)
#amnesty, maximum minority contact
amnesty.contact.simmax <- sim(amnesty.model, amnesty.contact.setmax)

# Simulated effects of latino aversion (figure 1, panel A)

#legal immigration, no latino aversion
immatt.dishis2.sim0 <- sim(immatt.model, immatt.dishis2.set0)
immatt.dishis2.ev0 <- as.data.frame(immatt.dishis2.sim0$qi$ev)
names(immatt.dishis2.ev0) <- c("one", "two", "three")
#legal immigration, latino aversion
immatt.dishis2.sim1 <- sim(immatt.model, immatt.dishis2.set1)
immatt.dishis2.ev1 <- as.data.frame(immatt.dishis2.sim1$qi$ev)
names(immatt.dishis2.ev1) <- c("one", "two", "three")
#mexican immigration, no latino aversion
mexiatt.dishis2.sim0 <- sim(mexiatt.model, mexiatt.dishis2.set0)
mexiatt.dishis2.ev0 <- as.data.frame(mexiatt.dishis2.sim0$qi$ev)
names(mexiatt.dishis2.ev0) <- c("one", "two", "three")
#mexican immigration, latino averion
mexiatt.dishis2.sim1 <- sim(mexiatt.model, mexiatt.dishis2.set1)
mexiatt.dishis2.ev1 <- as.data.frame(mexiatt.dishis2.sim1$qi$ev)
names(mexiatt.dishis2.ev1) <- c("one", "two", "three")
#amnesty, no latino aversion
amnesty.dishis2.sim0 <- sim(amnesty.model, amnesty.dishis2.set0)
#amnesty, latino aversion
amnesty.dishis2.sim1 <- sim(amnesty.model, amnesty.dishis2.set1)

#Producing Figure 1

pcthis2.means <- c(mean(immatt.pcthis2.evmin$three), mean(immatt.pcthis2.ev1$three), mean(immatt.pcthis2.ev2$three), mean(immatt.pcthis2.ev3$three), mean(immatt.pcthis2.evmax$three), mean(mexiatt.pcthis2.evmin$three), mean(mexiatt.pcthis2.ev1$three), mean(mexiatt.pcthis2.ev2$three), mean(mexiatt.pcthis2.ev3$three), mean(mexiatt.pcthis2.evmax$three), mean(amnesty.pcthis2.simmin$qi$ev), mean(amnesty.pcthis2.sim1$qi$ev), mean(amnesty.pcthis2.sim2$qi$ev),  mean(amnesty.pcthis2.sim3$qi$ev), mean(amnesty.pcthis2.simmax$qi$ev)) #all point estimates for latino context

pcthis2.means.mat <- matrix(pcthis2.means, nrow=5, ncol=3) #matrix form for plotting

pcthis2.lb <-  c(quantile(immatt.pcthis2.evmin$three, probs=c(.05)), quantile(immatt.pcthis2.ev1$three, probs=c(.05)), quantile(immatt.pcthis2.ev2$three, probs=c(.05)), quantile(immatt.pcthis2.ev3$three, probs=c(.05)), quantile(immatt.pcthis2.evmax$three, probs=c(.05)), quantile(mexiatt.pcthis2.evmin$three, probs=c(.05)), quantile(mexiatt.pcthis2.ev1$three, probs=c(.05)), quantile(mexiatt.pcthis2.ev2$three, probs=c(.05)), quantile(mexiatt.pcthis2.ev3$three, probs=c(.05)), quantile(mexiatt.pcthis2.evmax$three, probs=c(.05)), quantile(amnesty.pcthis2.simmin$qi$ev, probs=c(.05)), quantile(amnesty.pcthis2.sim1$qi$ev, probs=c(.05)), quantile(amnesty.pcthis2.sim2$qi$ev, probs=c(.05)), quantile(amnesty.pcthis2.sim3$qi$ev, probs=c(.05)), quantile(amnesty.pcthis2.simmax$qi$ev, probs=c(.05))) #lower bounds of confidence intervals for latino context

pcthis2.lb.mat <- matrix(pcthis2.lb, nrow=5, ncol=3) #matrix for plotting

pcthis2.ub <-  c(quantile(immatt.pcthis2.evmin$three, probs=c(.95)), quantile(immatt.pcthis2.ev1$three, probs=c(.95)), quantile(immatt.pcthis2.ev2$three, probs=c(.95)), quantile(immatt.pcthis2.ev3$three, probs=c(.95)), quantile(immatt.pcthis2.evmax$three, probs=c(.95)), quantile(mexiatt.pcthis2.evmin$three, probs=c(.95)), quantile(mexiatt.pcthis2.ev1$three, probs=c(.95)), quantile(mexiatt.pcthis2.ev2$three, probs=c(.95)), quantile(mexiatt.pcthis2.ev3$three, probs=c(.95)), quantile(mexiatt.pcthis2.evmax$three, probs=c(.95)), quantile(amnesty.pcthis2.simmin$qi$ev, probs=c(.95)), quantile(amnesty.pcthis2.sim1$qi$ev, probs=c(.95)), quantile(amnesty.pcthis2.sim2$qi$ev, probs=c(.95)), quantile(amnesty.pcthis2.sim3$qi$ev, probs=c(.95)), quantile(amnesty.pcthis2.simmax$qi$ev, probs=c(.95))) #lower bounds of confidence intervals for latino context

pcthis2.ub.mat <- matrix(pcthis2.ub, nrow=5, ncol=3) #matrix for plotting

contact.means <- c(mean(immatt.contact.evmin$three), mean(immatt.contact.ev1$three), mean(immatt.contact.ev2$three), mean(immatt.contact.ev3$three), mean(immatt.contact.evmax$three), mean(mexiatt.contact.evmin$three), mean(mexiatt.contact.ev1$three), mean(mexiatt.contact.ev2$three), mean(mexiatt.contact.ev3$three), mean(mexiatt.contact.evmax$three), mean(amnesty.contact.simmin$qi$ev), mean(amnesty.contact.sim1$qi$ev), mean(amnesty.contact.sim2$qi$ev),  mean(amnesty.contact.sim3$qi$ev), mean(amnesty.contact.simmax$qi$ev)) #point estimates for minority contact

contact.means.mat <- matrix(contact.means, nrow=5, ncol=3) #matrix

contact.lb <-  c(quantile(immatt.contact.evmin$three, probs=c(.05)), quantile(immatt.contact.ev1$three, probs=c(.05)), quantile(immatt.contact.ev2$three, probs=c(.05)), quantile(immatt.contact.ev3$three, probs=c(.05)), quantile(immatt.contact.evmax$three, probs=c(.05)), quantile(mexiatt.contact.evmin$three, probs=c(.05)), quantile(mexiatt.contact.ev1$three, probs=c(.05)), quantile(mexiatt.contact.ev2$three, probs=c(.05)), quantile(mexiatt.contact.ev3$three, probs=c(.05)), quantile(mexiatt.contact.evmax$three, probs=c(.05)), quantile(amnesty.contact.simmin$qi$ev, probs=c(.05)), quantile(amnesty.contact.sim1$qi$ev, probs=c(.05)), quantile(amnesty.contact.sim2$qi$ev, probs=c(.05)), quantile(amnesty.contact.sim3$qi$ev, probs=c(.05)), quantile(amnesty.contact.simmax$qi$ev, probs=c(.05))) #lower bounds for confidence intervals for minority contact

contact.lb.mat <- matrix(contact.lb, nrow=5, ncol=3) #matrix

contact.ub <-  c(quantile(immatt.contact.evmin$three, probs=c(.95)), quantile(immatt.contact.ev1$three, probs=c(.95)), quantile(immatt.contact.ev2$three, probs=c(.95)), quantile(immatt.contact.ev3$three, probs=c(.95)), quantile(immatt.contact.evmax$three, probs=c(.95)), quantile(mexiatt.contact.evmin$three, probs=c(.95)), quantile(mexiatt.contact.ev1$three, probs=c(.95)), quantile(mexiatt.contact.ev2$three, probs=c(.95)), quantile(mexiatt.contact.ev3$three, probs=c(.95)), quantile(mexiatt.contact.evmax$three, probs=c(.95)), quantile(amnesty.contact.simmin$qi$ev, probs=c(.95)), quantile(amnesty.contact.sim1$qi$ev, probs=c(.95)), quantile(amnesty.contact.sim2$qi$ev, probs=c(.95)), quantile(amnesty.contact.sim3$qi$ev, probs=c(.95)), quantile(amnesty.contact.simmax$qi$ev, probs=c(.95)))  #upper bounds for confidence intervals for minority contact

contact.ub.mat <- matrix(contact.ub, nrow=5, ncol=3) #matrix

dishis2.means <- c(mean(immatt.dishis2.ev0$three), mean(immatt.dishis2.ev1$three),  mean(mexiatt.dishis2.ev0$three), mean(mexiatt.dishis2.ev1$three),  mean(amnesty.dishis2.sim0$qi$ev), mean(amnesty.dishis2.sim1$qi$ev)) #point estimates for latino aversion

dishis2.means.mat <- matrix(dishis2.means, nrow=2, ncol=3)

dishis2.lb <-  c(quantile(immatt.dishis2.ev0$three, probs=c(.05)), quantile(immatt.dishis2.ev1$three, probs=c(.05)),  quantile(mexiatt.dishis2.ev0$three, probs=c(.05)), quantile(mexiatt.dishis2.ev1$three, probs=c(.05)),  quantile(amnesty.dishis2.sim0$qi$ev, probs=c(.05)), quantile(amnesty.dishis2.sim1$qi$ev, probs=c(.05))) #lower bounds

dishis2.lb.mat <- matrix(dishis2.lb, nrow=2, ncol=3)

dishis2.ub <-  c(quantile(immatt.dishis2.ev0$three, probs=c(.95)), quantile(immatt.dishis2.ev1$three, probs=c(.95)),  quantile(mexiatt.dishis2.ev0$three, probs=c(.95)), quantile(mexiatt.dishis2.ev1$three, probs=c(.95)),  quantile(amnesty.dishis2.sim0$qi$ev, probs=c(.95)), quantile(amnesty.dishis2.sim1$qi$ev, probs=c(.95))) #upper bounds

dishis2.ub.mat <- matrix(dishis2.ub, nrow=2, ncol=3)

library(gplots) #load this library for the convenient function for barplots with confidence intervals

#latino aversion barplot (Figure 1, Panel A)
pdf("dishis2barplot.pdf")
par(adj=0)
barplot2(dishis2.means.mat, beside=TRUE, col=c("grey", "grey40"),  main=" ", sub=" ", plot.ci=TRUE, ci.l=dishis2.lb.mat, ci.u=dishis2.ub.mat, plot.grid=TRUE, ylab="Predicted Probability", ylim=c(0, .7))
title(main="A. Latino Aversion")
mtext(text=c("Legal Immigration", "Mexican Immigration", "Amnesty"), side=1, at=c(2,5,8))
legend("topleft", c("No Latino Aversion", "Latino Aversion"), fill=c("grey", "grey40"))
dev.off()

#latino context barplot (Figure 1, Panel B)
pdf("pcthis2barplot.pdf")
par(adj=0)
barplot2(pcthis2.means.mat, beside=TRUE, col=c("white", "grey85", "grey70", "grey40","grey10"),  main=" ", sub=" ", plot.ci=TRUE, ci.l=pcthis2.lb.mat, ci.u=pcthis2.ub.mat, plot.grid=TRUE, ylab="Predicted Probability", ylim=c(0, .7))
title(main="B. Latino Context")
mtext(text=c("Legal Immigration", "Mexican Immigration", "Amnesty"), side=1, at=c(3,9.5,15.5))
legend("topleft", c("1.6 percent Latinos \n in census tract", "10 percent", "24 percent", "36 percent", "67 percent"), fill=c("white", "grey85", "grey70", "grey40","grey10"))
dev.off()
#minority contact barplot (Figure 1, Panel C)
pdf("contactbarplot.pdf")
par(adj=0)
barplot2(contact.means.mat, beside=TRUE, col=c("white", "grey85", "grey70", "grey40","grey10" ),  main=" ", sub=" ", plot.ci=TRUE, ci.l=contact.lb.mat, ci.u=contact.ub.mat, plot.grid=TRUE, ylab="Predicted Probability", ylim=c(0, .7))
title(main="C. Minority Contact")
mtext(text=c("Legal Immigration", "Mexican Immigration", "Amnesty"), side=1, at=c(3,9.5,15.5))
legend("topleft", c("Contact=-5.338", "Contact= -1.619", "Contact=-.389", "Contact=2.101", "Contact=5.820"), fill=c("white", "grey85", "grey70", "grey40","grey10"))
dev.off()


# some more simulations for reporting change in probability in text
sm <- sim(immatt.model, immatt.dishis2.set0, immatt.dishis2.set1)
s <- as.data.frame(sm$qi$fd)
names(s) <- c("one", "two", "three")

mean(s$one); quantile(s$one, probs=c(.05, .95))
mean(s$two); quantile(s$two, probs=c(.05, .95))
mean(s$three); quantile(s$three, probs=c(.05, .95))

sm2 <- sim(mexiatt.model, mexiatt.dishis2.set0, mexiatt.dishis2.set1)
s2 <- as.data.frame(sm2$qi$fd)
names(s2) <- c("one", "two", "three")

mean(s2$one); quantile(s2$one, probs=c(.05, .95))
mean(s2$two); quantile(s2$two, probs=c(.05, .95))
mean(s2$three); quantile(s2$three, probs=c(.05, .95))

sm3 <- sim(immatt.model, immatt.pcthis2.set2, immatt.pcthis2.set3)
s3 <- as.data.frame(sm3$qi$fd)
names(s3) <- c("one", "two", "three")

mean(s3$one); quantile(s3$one, probs=c(.05, .95))
mean(s3$two); quantile(s3$two, probs=c(.05, .95))
mean(s3$three); quantile(s3$three, probs=c(.05, .95))

sm4 <- sim(amnesty.model, amnesty.pcthis2.set2, amnesty.pcthis2.set3)
mean(sm4$qi$fd); quantile(sm4$qi$fd, procs=c(.05, .95))
