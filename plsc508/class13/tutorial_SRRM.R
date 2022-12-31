#load required packages
library(here)
library(amen)
library(msm)
library(lme4)
library(mnormt)
library(misty)
library(coda)
set.seed(114514)
#use bilateral trade data here
#Bilateral exports come from the IMF’s Direction of Trade statistics as well as CIA World Factbook
#The data set contains variables such as 
#AIDS/HIV incidence and deaths
#national Carbon Dioxide emissions values for 2004; 
#GDP
#national Imports and Exports data. 
#Population
load(here("class13", "TradeAndCovData0910.RData"))

#trade data from 1990-2008
out <- as.list(1:19)
yrs <- 1990:2008

#adjustment coefficient w/r to 2005 dollars
adj2005<-c(1.385035773, 1.337611188, 1.306639547, 1.278369379, 1.252004178, 1.226442068, 1.203533839, 1.182648457, 1.169443339, 1.152488373, 1.128071145, 1.103147907, 1.085573607, 1.062699132, 1.0333676, 1, 0.968452634, 0.941502813, 0.921808135)


#We pick a year's data, say, 2005 to run our model
t = 16

#adjust GDP according to 2005 USD
Y = ExDat[[t]]*adj2005[t]
#Let's take a loot at what Y looks like
View(Y)



#construct a Social Relations Model
#A non-random effect version of SRM model takes form Y_{ij} = \mu + a_i + b_j + /epsilon_{i,j}

##Running this command initiates an iterative Markov chain Monte Carlo (MCMC) algorithm that 
##provides Bayesian inference for the parameters in the SRM model.
fit_SRM<-ame(Y,family="nrm")
#Posterior mean estimates, standard deviations, nominal z-scores and p-values may be obtained with the summary command:
summary(fit_SRM)
plot(fit_SRM)


#The second two rows of Figure 2 give posterior predictive goodness of fit summaries for four network statistics: 
# (1) the empirical standard deviation of the row means
# (2) the empirical standard deviation of the column means
# (3) the empirical within-dyad correlation 
# (4) a normalized measure of triadic dependence



#########################################################################
#Social Relations Regression Modeling SRRM
#combines a linear regression model with the covariance structure of the SRM as follows

#Y_{ij} = \beta_d^T * x_{d,i,j} + \beta_r^T * x_{r,i}+ \beta_c^T * x_{c,j} + a_i + b_j + \epsilon_{ij}


#x_{r,i} is a vector of characteristics of node i as a sender, and x_{c,j} is a vector of characteristics of node j as a receiver.
#We refer to x_{d,i,j}, x_{r,i} and x_{c,i} as dyadic, row and column covariates, respectively. 
#In many applications the row and column characteristics are the same so that x_{r,i} = x_{c,i} = x_i   (our case too)

#create an n x n  array of covariates using GDP
Xs = matrix(center(log(CovDat[[t]]$GDPcurrent*adj2005[t])), ncol=1)
View(Xs)
# row and column covariates used in SRRM model
Xd = array(distance[[t]]/max(c(distance[[t]])), dim=c(ncol(Y), ncol(Y), 1))
#fit model with covariate
fit_srrm<-ame(Y,Xd=Xd,Xr=Xs,Xc=Xs,family="nrm")
#summary and plot

summary(fit_srrm)
plot(fit_srrm)


#assess the convergence of MCMC
#Z-scores for a test of equality of means between the first and last parts of the chain
# If larger than 1.75, then something's wrong
geweke.diag(fit_srrm$BETA)
geweke.diag(fit_srrm$VC) 


#It is instructive to compare these results to those that would be obtained under an ordinary
#linear regression model that assumed i.i.d. residual standard error. 
#fit a model without row variance, column variance or dyadic correlation
fit_rm<-ame(Y,Xd=Xd,Xr=Xs,Xc=Xs,family="nrm",rvar=FALSE,cvar=FALSE,dcor=FALSE)
#summary and plot 

summary(fit_rm)
plot(fit_rm)

#The parameter z statistics under this i.i.d. model are almost all bigger than those under the SRM fit.
#This suggests that SRRM model performs better in describing the data that we have
#The plots show, in particular, that the data exhibit much more dyadic correlation than can be explained by the i.i.d. model. 
#In contrast, the SRRM does not show such big discrepancy between model and true values.
#SRRM also did better job in representing row and col mean than the linear regression model. 

#assess the convergence of MCMC
geweke.diag(fit_rm$BETA) 
geweke.diag(fit_rm$VC) 


#########################################################
#SRRM with multiplicative effects


#Y_{ij} = \beta_d^T * x_{d,i,j} + \beta_r^T * x_{r,i}+ \beta_c^T * x_{c,j} + a_i + b_j + u_i^T * v_j + \epsilon_{ij}

#Here, u_i is a vector of latent, unobserved factors or characteristics that describe node i’s behavior
#as a sender, and similarly v_j describes node j’s behavior as a receiver.
#the mean of yi,j depends on how “similar” ui and vj are, as well as the magnitudes of the vectors.


fit_srrm_multi<-ame(Y,Xd=Xd,Xr=Xs,Xc=Xs,family="nrm", R = 2)
#summary and plot

summary(fit_srrm_multi)
plot(fit_srrm_multi)

#This model explains more dyadic correlations of the data compared to the models before. 






