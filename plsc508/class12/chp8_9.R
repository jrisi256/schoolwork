library(coda)
library(latentnet)

# striking workers, use a 5 point Likert scale to express how often they
# communicated with other workers. If you said you communicated at a level of 3
# or greater, than an undirected tie was said to have formed.
data("Strike", package = "ina")

set.seed(10)
lab <- network.vertex.names(Strike)
plot(Strike, vertex.cex = 1.5, label = lab, edge.col = "gray")

# for getting at distances in the latent space, one can use Euclidean distances,
# squared Euclidean distances, or Manhattan distances.

# two-dimensional latent space model. This will estimate, for our network, 49
# 1 + (2 * 24) parameters. Two latent positions for each vertex and one
# parameter for the intercept.

# model has more than one estimate for some of the variables. We focus on the
# posterior means.
# Occasionally, the graphs for the latent space models and a simple descriptive
# graphs will appear similar. However, under the hood, they are very different
# attempting to do very different things. Force-directed graphs are trying to
# create visually appealing results. Latent space models are trying to
# accurately represent the latent space. Importantly, overlapping vertices in
# latent space models are really important. It means the model is estimating
# these vertices are all essentially the same and occupy the same space in the
# network and same likelihood of forming ties.
strike.2d <- ergmm(Strike ~ euclidean(d = 2), verbose = T, seed = 125)
plot(strike.2d, plot.vars = F, pad = 0)
plot(strike.2d, what = "pmean", plot.vars = F, pad = 0)

# can manually examine each node's latent space location
# notice how Domingo, Eduardo, and Carlos have nearly the same position.
spanish <- c("Alejandro", "Domingo", "Eduardo", "Carlos")
spanish_locations <- loc(strike.2d)[spanish,]
dist(spanish_locations)

set.seed(15)
strike.km <- kmeans(loc(strike.2d), center = 3)
plot(strike.2d, what = "pmean", plot.vars = F, pad = 0)
points(strike.km$centers[, 1], strike.km$centers[, 2], pch = "+", cex = 3)

# autocorrelation plots
# some autocorrelation particularly at small lag values. Around lag 50, the
# autocorrelation diminishes. This is to be expected since the current sample
# is very similar to the previous sample in Markov chains. Ergmm thins out the 
# chain by discarding every tenth step so this helps to attenuate
# autocorrelation. Autocorrelation in Bayesian methods (when in theory samples)
# should be independent represents a high degree of correlation and poor mixing.
par(mar = c(1,1,1,1))
mcmc.diagnostics(strike.2d, which.diags = "acf")
# trace plot
# our trace plots looks pretty good. No bimodal distributions.
# Z.1.1 and 1.2 and so on an so forth refer to the parameters for each node.
# in this case, dimension 1 and 2 for node 1.
# use which.vars to few other parameters for other nodes
# other nodes might show bimodal distributions or autocorrelation suggesting
# our model does not fit very well.
# A trace plot shows the history of a parameter value across iterations of the
# chain. Here we have iterations across the x-axis. It shows you precisely
# where the chain has been exploring.
mcmc.diagnostics(strike.2d, which.diags = "trace")

# in reality, you may need to explore each individual point and each pair of
# points which can be time consuming and tedious. Other tools exist which can
# help expedite this process.
plot(mcmc_distance(strike.2d, 2, 24))

# assessing model fit. We would expect our model to: 1) accurately predict our
# network's density, and 2) predicts individual edges between nodes. Usual
# prediction metrics can be used: F1, AUC, RMSE, etc. One easy way to do this is
# to follow the lead of ERGM and simulate a bunch of models from our model and
# where our network lies.

# as we can see, our network is far outside the normally observed network
# density values from our simulations.
set.seed(123)
densities <- sapply(simulate(strike.2d, 500)[[2]], network.density)
hist(densities, breakds = "FD", xlab = "Network Density")
abline(v = network.density(Strike), col = "black", lty = 1, lwd = 3)

# what about other goodness of fit metrics, also suggests poor fit. The
# estimated models are too dense which suggests distances between vertices is
# too small, on average.
plot(gof(strike.2d, GOF = ~ degree + dist, nsim = 500))
abline(v = quantile(densities, probs = c(0.025, 0.5, 0.975)), lty = 2, lwd = 3)

# predictions
y_star <- predict(strike.2d, type = "pmean")
y <- as.matrix(summary(strike.2d)$model$Yg)
cm <- table(prediction = y_star[lower.tri(y_star)] > 0.5,
            truth = y[lower.tri(y)] == 1)

# we can, like with ERGM, include node and edge level attributes. However, we
# cannot include network level dependence variables. Why? Well LSM assumes
# independence of edges conditional on the latent locations of nodes. Including
# such terms would induce dependence. I do not include the code from the book on
# how to estimate LSM with node and dyad level covariates. But interpreation
# flows similarly to how it would for an ERGM model.

# How do pick the appropriate number of dimensions to model? The authors claim
# that theory should always be used first. In situations where theory offers no
# guidance, one can rely upon predictive performance. Note that usual model
# verification tools like BIC should not be used. Using more and more dimensions
# can cause issues because it causes the average distance between nodes to
# increase, the curse of dimensionality. Theoretically this is not a concern
# but computationally and for issues of convergence, ti can be a concern. As a 
# result, the authors suggest keeping the number of dimensions small.

# some important limitations are related to the assumptions: 1) Ties are
# generated independently conditional on the location of the vertices in the
# latent space and any additional attributes added. 2) we must also correctly
# identify the true number of dimensions in latent space (which is hard to do).
# so with ERGM, we can explicitly model the higher order dependencies. With LSM,
# the higher order dependencies are assumed to be captured in the latent space.
# Explicit vs. implicit. This could be a strength or a weakness depending on
# one's goals.
# LSM's are very computationally demanding, sad.
# Interpretation can be tricky. Particularly when one covariate is highly
# correlated with the latent space.