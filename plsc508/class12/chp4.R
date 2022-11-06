library(ina)
library(ergm)
data("Hookups")

set.seed(5)

m0 <- ergm(Hookups ~ edges +
               # people more likely to hook-up with similarly aged people
               absdiff("birthyear") + 
               # younger people are more likely to hook up
               nodecov("birthyear") +
               # people are more likely to hook up within their working unit
               nodematch("position") +
               # people prefer to hook up within their racial group
               nodematch("race") + 
               # people prefer to hook up with opposite sex (negative)
               nodematch("sex") +
               # earlier characters form more hookup ties
               nodecov("season"))

# larger age gaps leads to less hook ups
# no effect found for age
# same work unit means more hookups
# different sex, way more hookups
# characters introduced earlier in the show have more hookups
# This is just a logistic regression we are estimating

# monte carlo markov chain (MCMC) % is 0 because we have no endogeneous effects
# thus it is not yet needed.

# endogeneous variables, vertex based
# ostar(k), # of k out-stars, sociality
# gwodegree, geometrically weighted out degree, sociality
# istar(k), # of k in-stars, popularity
# gwidegree, geometrically weighted in-degree, popularity
# kstar(k), # of k stars, preferential attachment
# gwdegree, geometrically weighted degree, preferential attachment
# altkstar, geometrically weighted stars, preferential attachment

# endogeneous variables, dyad based
# mutual, # of mutual dyads, reciprocity
# asymmetric, # asymmetric dyads, antireciprocity
# degcor, degree homophily

# higher order endogeneous effects
# triangles, # of triangles, transitivity
# ttriple, # of transitive triads, transitivity
# ctriple, # of 3 cycles, indirect reciprocity
# gwesp, geometrically weighted edgewise transitivity
# gwnsp, geometrically weighted non-edgewise transitivty (anti-transitivity)

# the geometrically weightings help keep degeneracy away. And they also have
# different interpretations. Basically, it weights subsequent connections much
# less than the first few initial ones. E.g., for transitivity, the first
# shared partner between i and j contributes much more to their being a tie
# between i and j than does the second shared partner. And the 10th shared
# partner contributes much less than the 2nd or the 1st.

set.seed(5)
plot(Hookups,
     vertex.col = c("blue", "pink")[1 + (get.vertex.attribute(Hookups, "sex") == "F")],
     label = get.vertex.attribute(Hookups, "name"),
     label.cex = 0.75)

set.seed(510)
m1 <- ergm(Hookups ~ edges +
               # people more likely to hook-up with similarly aged people
               absdiff("birthyear") + 
               # younger people are more likely to hook up
               nodecov("birthyear") +
               # people are more likely to hook up within their working unit
               nodematch("position") +
               # people prefer to hook up within their racial group
               nodematch("race") + 
               # people prefer to hook up with opposite sex (negative)
               nodematch("sex") +
               # earlier characters form more hookup ties
               nodecov("season") +
               # tendency for monogamy
               degree(1) +
               # tendency for cliquishness, triads unlikely to form
               gwdsp(1, fixed = T),
           control = control.ergm(MCMC.samplesize = 5000,
                                  MCMC.burnin = 5000,
                                  MCMLE.maxit = 10))

g1 <- gof(m1)
plot(g1)

# we can include our own endogeneous variables into the ERGM framework
# ergms can also be used to model relationships in bipartite networks
