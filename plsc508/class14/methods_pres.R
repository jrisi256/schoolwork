
library(igraph)
library(haven)
library(sna)
library(here)

#install.packages("numDeriv")
library(numDeriv)

## [Lines 10:12] Read in csv files to create data frames
Advice <- read.csv(here("class14", "Advice.csv"), header=F)
reportsto <- read.csv(here("class14", "ReportsTo.csv"), header = F)
X <- read.csv(here("class14", "KrackhardtVLD.csv"))

#Lines 17:21 create Euclidean and structural equivalence matrices, respectively
#Euclidean captures the communication distance for each combination of actors 
#Structural equivalence captures the similarity of each actor to the others wrt to established ties
EuDist_rep<-as.matrix(dist(reportsto,method="euclidean",diag=T,upper=T))
EuDist_adv<-as.matrix(dist(Advice,method="euclidean",diag=T,upper=T))

StrucEqDist_rep <- sedist(as.matrix(reportsto))
StrucEqDist_adv <- sedist(as.matrix(Advice))

#Now we can create and plot an estimated model for the linear network autocorrelation model
#Our observed behavior will be time spent at company (tenure), our observed trait will be age, and we can test for autoregression and autocorrelation using Euclidean and Structural Equivalence differences
est_lnam <- lnam(y=X$Tenure # dependent variable is tenure
                 ,x=cbind(1,X$Age)
                 ,W1=list(EuDist_rep,EuDist_adv)
                 , W2 = list(StrucEqDist_adv,StrucEqDist_rep)) 
plot(est_lnam)


