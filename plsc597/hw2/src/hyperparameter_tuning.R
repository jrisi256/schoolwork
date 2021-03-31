library(mlr)
library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(caret)
library(foreign)
library(parallel)
library(neuralnet)
library(parallelMap)

# Used for setting seeds in parallel computing contexts
set.seed(420, kind = "L'Ecuyer-CMRG")

# Load-in training data
base::load(here("hw2", "output", "scaleTrain"))

# Make the training task
amnestyTaskTrain <- makeClassifTask(data = select(scaleTrain, -wteth),
                                    target = "amnesty")

# Create our learners
svm <- makeLearner("classif.svm", predict.type = "prob")
randforest <- makeLearner("classif.randomForest",
                          importance = T,
                          predict.type = "prob")
neuralnet <- makeLearner("classif.neuralnet", predict.type = "prob")

# We will be using 10-fold cross-validation for hyperparameter tuning
kFold10 <- makeResampleDesc("CV", iters = 10)

# Explore 50 different combos of hyperparameters for SVM
randSearchSvm <- makeTuneControlRandom(maxit = 100)

# Explore 1000 different combos of hyperparameters for random forest
randSearchRf <- makeTuneControlRandom(maxit = 1000)

# parameter space we're exploring for nn is smaller. Be doubly sure of estimates
kFold10nn <- makeResampleDesc("RepCV", fold = 10, reps = 10)

# Tune the hyperparamters for the support vector machine
svmParamSpace <- makeParamSet(
    makeDiscreteParam("kernel", values = c("polynomial", "radial", "sigmoid")),
    makeIntegerParam("degree", lower = 1, upper = 3),
    makeNumericParam("cost", lower = 0.1, upper = 10),
    makeNumericParam("gamma", lower = 0.1, upper = 10))

ptm <- proc.time()
parallelStartSocket(cpus = detectCores())

tunedSvm <- tuneParams(svm,
                       task = amnestyTaskTrain,
                       resampling = kFold10,
                       par.set = svmParamSpace,
                       control = randSearchSvm,
                       measures = list(acc, fpr, fnr))

parallelStop()
proc.time() - ptm

# Tune the hyperparameters for random forest
forestParamSpace <-
    makeParamSet(makeIntegerParam("ntree", lower = 200, upper = 200),
                 makeIntegerParam("mtry", lower = 5, upper = 10),
                 makeIntegerParam("nodesize", lower = 1, upper = 10),
                 makeIntegerParam("maxnodes", lower = 5, upper = 30))

ptm <- proc.time()
parallelStartSocket(cpus = detectCores())

tunedRandForest <- tuneParams(randforest,
                              task = amnestyTaskTrain,
                              resampling = kFold10,
                              par.set = forestParamSpace,
                              control = randSearchRf,
                              measures = list(acc, fpr, fnr))

parallelStop()
proc.time() - ptm

# Tune the hyperparameters for the neural network

# We also vary the number of hidden layers being trained in addition to the
# number of nodes in each layer.
randSearchNn1 <- makeTuneControlGrid()
randSearchNn2 <- makeTuneControlGrid()
randSearchNn3 <- makeTuneControlGrid()
randSearchNn4_5 <- makeTuneControlRandom(maxit = 100)
randSearchL <- list(randSearchNn1, randSearchNn2, randSearchNn3,
                    randSearchNn4_5, randSearchNn4_5)

neuralNetParamSpaces <- map(1:5, function(layers) {
    makeParamSet(makeNumericParam("threshold", lower = 0.8, upper = 0.8),
                 makeDiscreteParam("algorithm", values = c("rprop+")),
                 makeDiscreteParam("act.fct", values = c("logistic")),
                 makeIntegerVectorParam("hidden", len = layers, lower = 1, upper = 4))
})

ptm <- proc.time()
parallelStartSocket(cpus = detectCores())

tunedNeuralNet <- pmap(list(neuralNetParamSpaces, randSearchL),
                       function(paramSpace, randSearch) {
                           tuneParams(neuralnet,
                                      task = amnestyTaskTrain,
                                      resampling = kFold10nn,
                                      par.set = paramSpace,
                                      control = randSearch,
                                      measures = list(acc, fpr, fnr))})

parallelStop()
proc.time() - ptm

# save results
save(tunedSvm, file = here("hw2", "output", "tunedSvm"))
save(tunedRandForest, file = here("hw2", "output", "tunedRandForest"))
save(tunedNeuralNet, file = here("hw2", "output", "tunedNeuralNet"))
