library(ina)
library(latentnet)
data(ScottishSchool)

control <- control.ergmm(burnin = 150000, interval = 100)

# Interesting how friendship clustering follows smoking patterns
# Notice how the isolates are also not located at the periphery of the plot.
# Instead, they are in the center of the plot. Because they have no edges to
# other nodes, the model has little information as to where they need to be
# placed. Isolates are not anchored in any way by their connections to other
# nodes in the network so the model cannot locate them easily in the latent
# space. In smaller networks or in networks with a large number of isolates,
# the estimation algorithm may not estimate a useful model.
girls.2d.base <- ergmm(Girls ~ euclidean(d = 2), control = control, seed = 12)

girls.2d.x1 <- ergmm(Girls ~ absdiff("smoke") + euclidean(d = 2),
                     control = control,
                     seed = 12)

# Drug use (weed) and smoking seem to be the most influential of the covariates
# that were measured. Sports, alcohol, and sender family effects are all not
# significant.
girls.2d.x2 <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("alcohol") + absdiff("drugs") +
              absdiff("sports") + nodeofactor("family") + euclidean(d = 2),
          control = control,
          seed = 12)

# This model has the lowest BIC score, and it shall be the one used moving
# forward.
girls.2d.x3 <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("drugs") + euclidean(d = 2),
          control = control,
          seed = 12)

########################### Modeling networks with weighted edges
# We can use a Poisson model for counts.
# We can use a Binomial model for proportion of successes.
# We can use a Gaussian model for continuous outcomes.

girls.2d.pois <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("drugs") + euclidean(d = 2),
          response = "mentions",
          family = "Poisson",
          control = control,
          seed = 12)

# With binomial models, we can remedy some of the issues from the Poisson model.
# Namely, we can control the number of successes so to speak. We can ensure the
# model does not make nonsensical predictions where it predicts a weight for an
# edge that is impossible. In our example, a girl can only list another girl as
# a friend a maximum of 3 times (once each year). So we would specify this under
# binomial. This is somewhat of a limitation for binomial models because it
# assumes every node has the same maximum possible number of successes.

girls.2d.binom <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("drugs") + euclidean(d = 2),
          response = "mentions",
          family = "binomial",
          fam.par = list(trials = 3),
          control = control,
          seed = 12)

# We see that with both the poisson and binomial distributions, we increase the
# p-value substantially for smoking. The Poisson and binomial models performs
# very similarly.

############################################################# Cluster models
# Often times, we suspect vertices in a network can be grouped together by some
# characteristic. However, we may not have measured that variable, or we may not
# even be sure what that variable is exactly. We can use the latent position
# cluster model in exploratory analyses to find such clusters.
# In LPCMs, nodes are members of a cluster such that the density within clusters
# is higher than the density across clusters. Furthermore, nodes are associated
# with a latent position in which nodes that are closer together in the latent
# space are more homophilous.

# Adding clusters to these models does not add anything to our understanding
# of variables which give rise to the observed interaction patterns. You may 
# have also noticed that ergmm had a hard time estimating these models. There
# is another R package which is not as feature-rich (nor does it support
# binomial outcomes), but it uses Bayesian methods to better fit the model.
girsl.2d.c2 <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("drugs") +
              euclidean(d = 2, G = 2, mean.var = 1, var = 1),
          response = "mentions",
          family = "binomial",
          fam.par = list(trials = 3),
          control = control,
          seed = 12)

girls.2d.c3 <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("drugs") +
              euclidean(d = 2, G = 3, mean.var = 1, var = 1),
          response = "mentions",
          family = "binomial",
          fam.par = list(trials = 3),
          control = control,
          seed = 12)

library(VBLPCM)
# Begin by running a routine which initializes the model and finds a good
# starting value for the main fitting procedure. Once this routine has been set
# (by also specifying the number of clusters and the number of dimensions in the
# latent space), you feed the results into the model fitting function.

vb.2d.c2.start <- vblpcmstart(Girls, G = 2, d = 2)
vb.2d.c2.fit <- vblpcmfit(vb.2d.c2.start)

vb.2d.c3.start <- vblpcmstart(Girls, G = 3, d = 2)
vb.2d.c3.fit <- vblpcmfit(vb.2d.c3.start)

# From the book, plotting the networks, we can see the clusters are clearly
# identified.

######################################################### Random Effects Models
# Not all variables leading to edge formation are known and accounted for in
# the data. There are often vertex-level features that are associated with edge
# formation but are unknown or unknowable. We now introduce a method for
# modeling unobserved heterogeneity which are known as random effects.

girls.2d.rand <-
    ergmm(Girls ~ absdiff("smoke") + absdiff("drugs") + rsender + rreceiver +
              euclidean(d = 2),
          response = "mentions",
          family = "binomial",
          fam.par = list(trials = 3),
          control = control,
          seed = 12)

# We can identify the ranges of values for the random receiver and sender
# effects (i.e. was an individual more or less likely to receive connections
# given observed exogeneous covariates and observed connections). Notice how one
# girl is an extreme outlier. She is much more likely to be have connections
# formed with her than would expected based on her observed covariates as well
# the latent space she occupies as determined by her connections.

sr <- rbind(data.frame(effect = "sender",
                       re = summary(girls.2d.rand)$pmean$sender),
            data.frame(effect = "receiver",
                       re = summary(girls.2d.rand)$pmean$receiver))

boxplot(re ~ effect, data = sr, horizontal = T, xlab = "Effect Size")

# Finally we have AMEN. Additive and multiplicative effect networks. Similar to
# latent space models with random effects, therre are latent variables which
# model nodal degree tendencies (i.e., additive effects) and latent variables
# which model tendencies toward dyadic interaction (i..e., multiplicative
# effects). Sadly, we have to represent the variables as matrices.

library(amen)
library(abind)

diff.smoke <- as.matrix(dist(get.vertex.attribute(Girls, "smoke"), upper = T))
diff.drugs <- as.matrix(dist(get.vertex.attribute(Girls, "drugs"), upper = T))
# bind dyadic covariates
X <- abind(smoke = diff.smoke, drugs = diff.drugs, along = 3)

# R = 2 specifies a 2-dimensional latent space.
girls.ame.2d <- ame(Girls[,],
                    Xdyad = X,
                    R = 2,
                    symmetric = F,
                    family = "bin",
                    rvar = T,
                    cvar = T,
                    nscan = 500000,
                    burn = 10000,
                    plot = F,
                    print = T,
                    seed = 12)

# Use MCMC diagnostics to ensure the Markov chains for the regression
# coefficients have converged. They implement a test which sees if the mean of
# the Markov chain in the early part of the chain is different from that toward
# the end of the chain. Test statistics can be analyzed as such and the smaller
# they are in magnitude, the greater the evidence for convergence.
library(coda)
beta.mcmc <- as.mcmc(girls.ame.2d$BETA)
geweke.diag(beta.mcmc)

# after having checked for convergence, we  can interpret the model. The Rho
# variable measures reciprocity and the large posterior mean and small posterior
# standard deviation (large and precise estimate) indicate reciprocity is
# important.
summary(girls.ame.2d)
