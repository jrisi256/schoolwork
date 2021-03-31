## ---- warning = F, message = F---------------------------------------------------------------------------------------
library(mlr)
library(cspp)
library(kknn)
library(purrr)
library(dplyr)
library(tidyr)
library(xgboost)
library(ggplot2)
library(parallel)
library(reghelper)
library(ggcorrplot)
library(parallelMap)


## --------------------------------------------------------------------------------------------------------------------
vars <- c("infantmortality", "vcrimerate", "propcrimerate", "z_cigarette_taxes",
          "cbeertex", "statemin", "unemployment", "povrate", "cons_fossil",
          "ranney1_sen_dem_prop", "ranney2_hs_dem_prop")

data <-
    get_cspp_data(vars = vars) %>%
    filter(!is.na(infantmortality), !state %in% c("District of Columbia", "Nebraska")) %>%
    select(-st.abb, -stateno, -state_fips, -state_icpsr) %>%
    mutate(across(-state, as.numeric)) %>%
    group_by(year) %>%
    mutate(scaled_fossil = (cons_fossil - mean(cons_fossil)) / sd(cons_fossil),
           state = as.factor(state)) %>%
    ungroup() %>%
    select(-cons_fossil)


## --------------------------------------------------------------------------------------------------------------------
missings <- map(data, ~sum(is.na(.x)))
missingsPrcnt <- unlist(missings) / nrow(data)


## --------------------------------------------------------------------------------------------------------------------
set.seed(420)
train <- data %>% sample_frac(0.70)
test <- anti_join(data, train)


## --------------------------------------------------------------------------------------------------------------------
trainUntidy <- train %>% pivot_longer(c(-infantmortality, -state))
ggplot(trainUntidy, aes(value, infantmortality)) +
    facet_wrap(~ name, scale = "free_x") +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_bw()


## --------------------------------------------------------------------------------------------------------------------
corrMatrix <- cor(select(train, -year, -state), use = "complete.obs")

ggcorrplot(corrMatrix,
           type = "lower",
           method = "circle",
           colors = c("red", "white", "blue"))


## --------------------------------------------------------------------------------------------------------------------
imputeMethod <- imputeLearner("regr.rpart")
trainImputed <- impute(as.data.frame(train),
                       classes = list(numeric = imputeMethod))
testImputed <- reimpute(test, trainImputed$desc)


## --------------------------------------------------------------------------------------------------------------------
# Create the training task on the imputed data not including year or state
infantMortalityTaskTrain <-
    makeRegrTask(data = select(trainImputed$data, -year, -state),
                 target = "infantmortality")

# Create our learners
knn <- makeLearner("regr.kknn")
forest <- makeLearner("regr.randomForest", importance = T)
xgb <- makeLearner("regr.xgboost")

# We will be using 10-fold cross-validation for hyperparameter tuning
kFold10 <- makeResampleDesc("CV", iters = 10)

# For kNN, we will search across the entire parameter space
gridSearch <- makeTuneControlGrid()

# Tree-based algos will explore 100 different combinations of hyperparameters
randSearch <- makeTuneControlRandom(maxit = 100)


## --------------------------------------------------------------------------------------------------------------------
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:50))

tunedKnn <- tuneParams(knn,
                       task = infantMortalityTaskTrain,
                       resampling = kFold10,
                       par.set = knnParamSpace,
                       control = gridSearch,
                       measures = list(rmse, mae))

tunedKnn


## --------------------------------------------------------------------------------------------------------------------
knnTuningData <- generateHyperParsEffectData(tunedKnn)

plotHyperParsEffect(knnTuningData, x = "k",
                    y = "rmse.test.rmse",
                    plot.type = "line") +
    theme_bw()


## --------------------------------------------------------------------------------------------------------------------
forestParamSpace <-
    makeParamSet(makeIntegerParam("ntree", lower = 200, upper = 200),
                 makeIntegerParam("mtry", lower = 5, upper = 10),
                 makeIntegerParam("nodesize", lower = 1, upper = 10),
                 makeIntegerParam("maxnodes", lower = 5, upper = 30))

ptm <- proc.time()

parallelStartSocket(cpus = detectCores())

tunedRandomForest <- tuneParams(forest,
                                task = infantMortalityTaskTrain,
                                resampling = kFold10,
                                par.set = forestParamSpace,
                                control = randSearch,
                                measures = list(rmse, mae))

parallelStop()

proc.time() - ptm

tunedRandomForest


## --------------------------------------------------------------------------------------------------------------------
xgbParamSpace <-
    makeParamSet(makeNumericParam("eta", lower = 0, upper = 1),
                 makeNumericParam("gamma", lower = 0, upper = 10),
                 makeIntegerParam("max_depth", lower = 1, upper = 20),
                 makeNumericParam("min_child_weight", lower = 1, upper = 10),
                 makeNumericParam("subsample", lower = 0.5, upper = 1),
                 makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
                 makeIntegerParam("nrounds", lower = 50, upper = 50))

ptm <- proc.time()

tunedXgb <- tuneParams(xgb,
                       task = infantMortalityTaskTrain,
                       resampling = kFold10,
                       par.set = xgbParamSpace,
                       control = randSearch,
                       measures = list(rmse, mae))

proc.time() - ptm

tunedXgb


## --------------------------------------------------------------------------------------------------------------------
# train our OLS regression
trainedOls <- lm(infantmortality ~ .,
                 data = select(trainImputed$data, -state, -year))

# set hyperparameter for k-nearest neighbor
tunedKnnPars <- setHyperPars(knn, par.vals = tunedKnn$x)
trainedKnn <- train(tunedKnnPars, infantMortalityTaskTrain)

# set hyperparameters and train the random forest
tunedRandomForestPars <- setHyperPars(forest, par.vals = tunedRandomForest$x)
trainedRandomForest <- train(tunedRandomForestPars, infantMortalityTaskTrain)

# set hyperparameters and train xgboost
tunedXgbPars <- setHyperPars(xgb, par.vals = tunedXgb$x)
trainedXgb <- train(tunedXgbPars, infantMortalityTaskTrain)

# Check to see if our MSE converges for random forest
randomForestData <- trainedRandomForest$learner.model
plot(randomForestData)

# Check to see if our MSE converges for XGBoost
xgbData <- trainedXgb$learner.model
ggplot(xgbData$evaluation_log, aes(iter, train_rmse)) +
    geom_line() +
    geom_point() +
    theme_bw()


## --------------------------------------------------------------------------------------------------------------------
predictOls <-
    as_tibble(predict(trainedOls,
                      newdata = select(testImputed, -year, -state))) %>%
    bind_cols(testImputed) %>%
    mutate(error = value - infantmortality,
           error_sq = error ^ 2,
           error_abs = abs(error),
           sum_abs_error = sum(error_abs),
           mae = sum_abs_error / n(),
           sum_sq_error = sum(error_sq),
           rmse = sqrt(sum_sq_error / n()))

predictKnn <-
    as_tibble(predict(trainedKnn,
                      newdata = select(testImputed, -year, -state))) %>%
    mutate(error = truth - response,
           error_sq = error ^ 2,
           error_abs = abs(error),
           sum_abs_error = sum(error_abs),
           mae = sum_abs_error / n(),
           sum_sq_error = sum(error_sq),
           rmse = sqrt(sum_sq_error / n()))

predictRandomForest <-
    as_tibble(predict(trainedRandomForest,
                      newdata = select(testImputed, -year, -state))) %>%
    mutate(error = truth - response,
           error_sq = error ^ 2,
           error_abs = abs(error),
           sum_abs_error = sum(error_abs),
           mae = sum_abs_error / n(),
           sum_sq_error = sum(error_sq),
           rmse = sqrt(sum_sq_error / n()))

predictXgb <-
    as_tibble(predict(trainedXgb,
                      newdata = select(testImputed, -year, -state))) %>%
    mutate(error = truth - response,
           error_sq = error ^ 2,
           error_abs = abs(error),
           sum_abs_error = sum(error_abs),
           mae = sum_abs_error / n(),
           sum_sq_error = sum(error_sq),
           rmse = sqrt(sum_sq_error / n()))

cat("RMSE\n")
cat("ols: ", unique(predictOls$rmse), "\n")
cat("knn: ", unique(predictKnn$rmse), "\n")
cat("random forest: ", unique(predictRandomForest$rmse), "\n")
cat("xgb: ", unique(predictXgb$rmse), "\n")
cat("MAE\n")
cat("ols: ", unique(predictOls$mae), "\n")
cat("knn: ", unique(predictKnn$mae), "\n")
cat("random forest: ", unique(predictRandomForest$mae), "\n")
cat("xgb: ", unique(predictXgb$mae), "\n")


## --------------------------------------------------------------------------------------------------------------------
ggplot(predictOls, aes(x = infantmortality, y = value)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw() +
    labs(title = "OLS predicted vs. truth")

ggplot(predictKnn, aes(x = truth, y = response)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw() +
    labs(title = "kNN predicted vs. truth")

ggplot(predictRandomForest, aes(x = truth, y = response)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw() +
    labs(title = "Random Forest predicted vs. truth")

ggplot(predictXgb, aes(x = truth, y = response)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw() +
    labs(title = "Xgb predicted vs. truth")


## --------------------------------------------------------------------------------------------------------------------
randomForestFeaturesGini <-
    getFeatureImportance(trainedRandomForest)$res %>%
    mutate(model = "randomForestNode")

randomForestFeaturesAcc <-
    getFeatureImportance(trainedRandomForest, type = 1)$res %>%
    mutate(model = "randomForestAcc")

xgboostFeatures <-
    getFeatureImportance(trainedXgb)$res %>%
    mutate(model = "xgboost")

vars <- gsub(".z", "", rownames(beta(trainedOls, y = F)$coefficients))
olsStdFeatures <-
    beta(trainedOls, y = F)$coefficients %>%
    as_tibble() %>%
    mutate(variable = vars,
           model = "olsStdz",
           Estimate = abs(Estimate)) %>%
    filter(variable != "(Intercept)") %>%
    select(Estimate, variable, model) %>%
    rename(importance = Estimate)

features <- bind_rows(randomForestFeaturesGini,
                      xgboostFeatures,
                      randomForestFeaturesAcc,
                      olsStdFeatures)

ggplot(features, aes(x = reorder(variable, importance), y = importance)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ model, scales = "free_x") +
    coord_flip() +
    theme_bw()

