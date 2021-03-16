###############################
### Imputing Missing Values ###
###############################

library(mlr)
library(missMethods)
library(caret)
library(here)

# The data here comes from Dr Jillian Jaeger's paper "Securing Communities or Profits?...
# ...The Effect of Federal-Local Partnerships on Immigration Enforcement".
# This data set a continuous response variable, "total", measuring the total number...
# ...of deportations in a particular US county.
# The other variables capture statistics and conditions within these counties.
# See the Box folder for the data set download and codebook!

load(here("class_presentations",
          "2021-03-16-Regression-GAMS-William-Suman",
          "scomm_replication data.RData"))
Deport <- as.data.frame(x)
rm(x)

Deport <- subset(Deport, select = c(borderstate,le_budget_alljurisdictions,budgetpop,privatefacility,crime,censuspop,fb2010,hisp10,constructiongdp,unauthorized,conservatism,daysactive,total))

# If you wanted to artificially generate NA values, here's how!
#Deport <- delete_MCAR(Deport, 0.3, "censuspop")


# Let's see how our variables interact with the response.

pairs(Deport)

# Now, I impute the data.
# This is estimating values for the NA's based on our chosen learner function.

imputeMethod <- imputeLearner("regr.rpart")
Deport.Imp <- mlr::impute(Deport, target = "total", classes = list(numeric = imputeMethod))

# Time to put the estimated values back into the original data.

Deport.Fill <- reimpute(Deport, Deport.Imp$desc)

# A quick comparison of linear models built from the actual and imputed data sets shows...
# ...our imputed data model produces similar coefficient estimates!
# Both of these models are not good, but all I wanted to show was that the imputed data...
# ...was able to simulate the actual data. I declare this a success!

Deport.Fill.lm <- lm(total ~ borderstate+le_budget_alljurisdictions+budgetpop+privatefacility+crime+censuspop+fb2010+hisp10+constructiongdp+unauthorized+conservatism+daysactive, data = Deport.Fill)
summary(Deport.Fill.lm)
Deport.lm <- lm(total ~ borderstate+le_budget_alljurisdictions+budgetpop+privatefacility+crime+censuspop+fb2010+hisp10+constructiongdp+unauthorized+conservatism+daysactive, data = Deport)
summary(Deport.lm)

plot(Deport.Fill.lm)
plot(Deport.lm)

##############################
### Filtering For Features ###
##############################

# I could certainly improve the model I created by selecting different factors.
# This uses a filter method to select the 4 factors with the greatest correlation...
# ...to the response.
# This could have been wrapped into the imputation process, but I do them separately...
# ...to better illustrate the individual steps.

# I need to define a task and learner to use the necessary mlr functions.
# The task is defining our data set and response variable (total).
# The learner is defining our desired learning method (linear regression).

# I can't use this technique if I have missing data in the response (target).
# I have already imputed all NA's for the other factors, so the only NA's left...
# ...are two NA values in the response. Rather than impute again, I'll remove them.

Deport.Fill <- na.omit(Deport.Fill)
DeportTask <- makeRegrTask(data = Deport.Fill, target = "total")
Learn <- makeLearner("regr.lm")

# I will "filter" the independent variables by their degree of linear correlation...
# ...with the response variable. I want to then choose the factors with the highest...
# ...degree of linear correlation to include in my model.

FilterCheck <- generateFilterValuesData(DeportTask, method = "linear.correlation")

FilterCheck$data
plotFilterValues(FilterCheck)

DeportTask.Filter <- filterFeatures(DeportTask, fval = FilterCheck, abs = 4)

#############################
### Wrapping For Features ###
#############################

# Time for another feature-selection method. This time, with wrapping!
# I want to test the effectiveness of this feature selection against...
# ...my general model with imputed data (Deport.Fill.lm, renamed Full.lm)

library(caTools)
splitdata <- sample.split(Y = Deport.Fill$total, SplitRatio = 0.7)
train.set <- subset(Deport.Fill, splitdata == "TRUE")
test.set <- subset(Deport.Fill, splitdata == "FALSE")

# I use k-fold cross validation, and exhaustive (all iterations) modeling...
# ...limited to 5 total factors for computational efficiency.
# This takes ~1 min to run on two cores, but does not return the same set...
# ...of features each time it is run. Sacrificing reliability for speed here.
# The output gives me the estimated best factors for a model. Nice!

DeportTask <- makeRegrTask(data = train.set, target = "total")
WrapFeatSelType <- makeFeatSelControlExhaustive(max.features = 5)
CV <- makeResampleDesc("CV", iters = 5)
Learn <- makeLearner("regr.rpart")
WrapFeats <- selectFeatures(learner = Learn, task = DeportTask, resampling = CV, control = WrapFeatSelType)
WrapFeats$x #Print-out of selected features

# My iteration of the selectFeatures function gave the following features:
# "borderstate" "censuspop" "fb2010" "unauthorized" "conservatism"

WrapSel.lm <- lm(total ~ borderstate+censuspop+fb2010+unauthorized+conservatism, data = train.set)

Full.lm <- lm(total ~ borderstate+le_budget_alljurisdictions+budgetpop+privatefacility+crime+censuspop+fb2010+hisp10+constructiongdp+unauthorized+conservatism+daysactive, data = train.set)

# Before we actually try to predict the test set with our models, we can compare...
# ...some descriptive statistics. Unfortunately, they are rather mixed.
# I found that the Wrapped Feature model had a higher R Squared (good), and...
# ...a lower MAE (good), but a higher RMSE (bad).
# No one statistic is fully informative of model quality.

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
WrapSel.model <- train(total ~ borderstate+censuspop+fb2010+unauthorized+conservatism, data = train.set, method = "lm", trControl = train.control)
print(WrapSel.model)

Full.model <- train(total ~ borderstate+le_budget_alljurisdictions+budgetpop+privatefacility+crime+censuspop+fb2010+hisp10+constructiongdp+unauthorized+conservatism+daysactive, data = train.set, method = "lm", trControl = train.control)
print(Full.model)

# Now let's compare residuals for the two models when predicting the test set.
# The results are much clearer here:
# Our Wrapped Feature model has significantly smaller Mean Square Error...
# ...suggesting that the model is better at prediction than the "full" model.
# Maybe we can improve this performance in the next section!

PredY <- predict(WrapSel.lm, test.set, type = "response")
Residuals <- PredY - test.set$total
MSE.W <- mean(Residuals^2)
MSE.W
var(Residuals)

PredY <- predict(Full.lm, test.set, type = "response")
Residuals <- PredY - test.set$total
MSE.F <- mean(Residuals^2)
MSE.F
var(Residuals)

###################################
### Generalized Additive Models ###
###################################

library(dplyr)
library(mboost)
library(parallel)
library(parallelMap)

# Let's see if we can improve on a linear model using the 5 features selected previously.
# First, let's make a linear model with the factors and visualize some relationships.

NewDeport <- subset(train.set, select = c("borderstate","censuspop","fb2010","unauthorized","conservatism","total"))

WrapSel.lm <- lm(total ~ borderstate+censuspop+fb2010+unauthorized+conservatism, data = train.set)
summary(WrapSel.lm)
plot(WrapSel.lm)

pairs(NewDeport)

# Looks like this model is not terrible to begin with. But, we should check for interactions.
# The pairs function will create a very dense series of plots, best to open it in a new window!
# I can look at the factor vs factor plots to try and find variables that seem to interact.
# Based on the plots, I choose two potential interactions:
# fb2010 * conservatism
# fb2010 * unauthorized

# If I wanted to include these in my model, I could just add them to the lm formula.

NewDeport.lm <- lm(total ~ borderstate+censuspop+fb2010+unauthorized+conservatism+fb2010*conservatism+fb2010*unauthorized, data = NewDeport)

# But, if I want to use GAM functions to select my factors...
# ...I need to make my interactions into new variables.

ND.Gam <- mutate(NewDeport, FBCon = as.numeric(interaction(fb2010,conservatism)))
plot(ND.Gam$FBCon, ND.Gam$total)

ND.Gam <- mutate(ND.Gam, FBUnauth = as.numeric(interaction(fb2010,unauthorized)))
plot(ND.Gam$FBUnauth, ND.Gam$total)

# You can see by the plots that the interaction terms both appear to have...
# ...some non-random relationship with the response. Great!

# To determine which factors to use in my model, I'm going to use wrapping.
# This code should look familiar, its the same process I used earlier...
# ...but now, my data is huge! This will take a very long time to run.
# When it does finish, we will know which factors to include in our model.
# I might find out my interactions aren't needed, but that's what the test is for!

ND.Gam$borderstate <- ND.Gam$borderstate + rnorm(nrow(ND.Gam))/1000

ND.Task <- makeRegrTask(data = ND.Gam, target = "total")
imputeMethod <- imputeLearner("regr.rpart")
ND.ImputeWrap <- makeImputeWrapper("regr.gamboost", classes = list(imputeMethod))
ND.FeatSelType <- makeFeatSelControlRandom()
CV <- makeResampleDesc("CV", iters = 5)
ND.FeatSelWrapper <- makeFeatSelWrapper(learner = ND.ImputeWrap, resampling = CV, control = ND.FeatSelType)
  
parallelStartSocket(cpus = detectCores()) # Running on all cylinders... er, cores
ND.Gam.Model <- mlr::train(ND.FeatSelWrapper, ND.Task) # This is the actual training process
parallelStop() # Don't want to leave multi-core on permanently!

ND.Gam.Model

GamData <- getLearnerModel(ND.Gam.Model, more.unwrap = TRUE)

# The GAM found polynomial (non-linear) relationships that are shown...
# ...in the partial derivative plots included below.

par(mfrow = c(2,2))
plot(GamData, type = "l")
par(mfrow = c(1,1))
plot(GamData$fitted(), resid(GamData))
qqnorm(resid(GamData))
qqline(resid(GamData))

# To use our GAM to predict the test set, we need to add the interaction...
# ...terms we created previously to the test.set data.

test.set <- mutate(test.set, FBCon = as.numeric(interaction(fb2010,conservatism)))
test.set <- mutate(test.set, FBUnauth = as.numeric(interaction(fb2010,unauthorized)))

# Here I am predicting the test.set responses using the new GAM model.
# The MSE of this new GAM is much lower than any of our previous models. Great!

PredY <- predict(GamData, test.set, type = "response")
Residuals <- PredY - test.set$total
MSE.G <- mean(Residuals^2)
MSE.G
var(Residuals)

# There isn't an easy way to visualize a GAM like we can with most regression models.
# While its good to think of a GAM as non-linear or polynomial regression...
# ...R is doing extra steps to smooth the model that make algebraic expression difficult.
# Therefore, a good way to test the strength of the model is with test sets!