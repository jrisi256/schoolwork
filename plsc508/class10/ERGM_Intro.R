####### Introduction to ERGM #######
### In this method tutorial, I will display basic application, diagnostics and interpretation of ERGM.
### The data source is a paper by the developmental psychologist Helen Bott in her 1928 article 
### that used ethological observations of children in a nursery school.
### I downloaded from Zack Almquist's Github Repository "networkdata"

# Preliminary
# rm(list = ls()) # running this line when required to clean the envrionment
## Load necessary packages
require(igraph)
require(network)
require(ergm)
library(here)

## Set WD: setwd("D:/PLSC 508") 
## Load data
load(here("class10", "bott.RData"))

### The Bott data consists of five sets of relations between 11 children, 
### namely: talked to another child; interfered with another child; imitated another child;
### and cooperated with another child.

## I will first use the network illustrates talking between children.

# Network description

net.talk <- bott[[1]]
# Plot the network
set.seed(3.4347e4)
plot(net.talk)
# Display vertex attributes: name and age
net.talk %v% "vertex.names"
net.talk %v% "age.month"

# The youngest child in the talking network  is 26-month old, while the oldest one is 54-month old.

# Apply ERGM 

## First,  applying a model that just has edges in it. This model is essentially the same as a linear model with only intercept.

m0 <- ergm(net.talk ~ edges, verbose = T) # use the default method: maximum pseudolikelihood estimator
summary(m0)

summary(ergm(net.talk ~ edges, estimate = "MLE")) # the MLE method returns the equivalent result

### When no dependence terms are included in the model, MLE and MPLE are the same. 

1/(1 + exp(-coef(m0)[1]))

### The probability of ties between vertices is about 0.68.

## I include an exogenous covariate as a combined dyad level covariate: 
## combined age of the children involved in a dyad in months.

m1 <- ergm(net.talk ~ edges + nodecov('age.month'))
summary(m1)

exp(coef(m1))

### It appears the odds of edges between any two vertices is higher when combined age of the children involved in a dyad is higher.
### In other words older children talked more frequently.

## I include age in months as both an incoming vertex-level convariate and outgoing vertex-level covariate

m2 <- ergm(net.talk ~ edges + nodeicov('age.month') +
             nodeocov('age.month'))
summary(m2)

exp(coef(m2))

### It appears the outgoing effect and incoming effect is both positive.
### However, only the outgoing effect is statistically significant.
### In other words, older children are more likely to send ties. 


## Include age difference in the analysis.
m3 <- ergm(net.talk ~ edges + nodeicov('age.month') +
             nodeocov('age.month') + absdiff('age.month'))
summary(m3)

exp(coef(m3))

### Higher age difference between pairs predict little difference in formation of ties.
### The coefficient of age difference is far from statistical significance.

### The model with additional covariates does not perform better than the model leveraging the combined dyad-level covariate.
### Future analysis would base on 'm1' instead of 'm2'.

## I include endogenous dependence in the model.
## A measure of reciprocity which is defined as the number of pairs in the network 
## in which ties directed from children i to j and children j to i both exist.

set.seed(7.4532e4) ## When endogenous dependence is included in the model, 
## the model could only be estimated by MCMC, and setting the random seed is necessary to replicate results.

m3 <- ergm(net.talk ~ edges + nodecov('age.month') +
             mutual, 
           control = control.ergm(MCMC.samplesize = 8e3,
                                  MCMC.burnin = 1e4,
                                  MCMLE.maxit = 10))

summary(m3)

exp(coef(m3))

### It appears the odds of edges between any two vertices is higher when combined age of the children involved in a dyad is higher,
### and child i is more likely to send a tie to child j if j has sent a tie to i.

# Goodness of fit

gof1 <- gof(m3)
gof1

### Because most p-values for goodness-of-fit statistics (except 2) are way larger than the threshold of marginal statistical significance (0.1),
### the difference between observed and simulated networks is not significant which implies good model fit. 

par(mfrow=c(1,3))
plot(gof1) # I don't know why I could get plots for geodesic distance and edgewise shared partner.

# Evaluate MCMC process to check potential degeneracy
par(mar=c(1,1,1,1))
mcmc.diagnostics(m3)

### Because trace plots do not trend and density plots approximates normal distributions, the MCMC diagnostics looks good. 