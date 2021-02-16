#PLSC 597 - Methods Tutorial - Classification with Naive Bayes and SVMs

#Tuba Sendinc February 16,2021
 
#In this tutorial, I predict regime type based on support signals sent by major powers, i.e., the US, Russia, China, the UK, and France, for 
#all countries in the world that has a population higher than 500,000.
#The data cover the period between 1950 and 2012.

#In the first part of this tutorial, I predict whether a country is a democracy or an autocracy 
#based on the following support signals signaled by major powers using 
#the Naive Bayes Model (in lines 52-133): arms transfers, economic aid, joint military exercise, 
#nuclear deployment,defense pact, troop deployment, leader visit, and leader statements. 
#Troop deployments and arms transfers are continuous and the rest of the variables are binary.

#In the second part, I predict regime type 
#based on troop deployments and arms transfers sent by major powers using 
#Support Vector Machine Algorithm (in lines 135-228). 
#The predictor variables are continuous in this stage but regime type 
#variable is again binary that scores 1 when a country is a democracy.

#Finally, I compare the performance of Naive Bayes and the best performing version of Support Vector Machine 
#using the same training data and covariates (in lines 231-249). 
#Predictions suggest that SVM performs better than Naive Bayes in predicting democracies. 

#I construct the regime type variable based on the Polity 2 score in the Polity 5 dataset. 
#Polity 2 score is a composite index of regime type that ranges from -10 to 10. 
#A country with a Polity 2 score equal to or greater than 6 is considered a democracy. 
#Countries with a Polity score lower than 6 are autocracies.
#The regime type variable I use here scores 1 when a country has a Polity 2 score equal to or greater than 6 and scores 0 otherwise.
#Data on regime characteristics and more explanation on the Polity 2 score can be found at https://www.systemicpeace.org/inscrdata.html

#I obtain data on support signals from McManus and Neiman (2019) (see, McManus and Neiman (2019) at https://www.cambridge.org/core/journals/international-organization/article/abs/logic-of-offstage-signaling-domestic-politics-regime-type-and-major-powerprotege-relations/F60610A17F299F6072F843510729951E)
#Data on signals by major powers can be found at https://www.prio.org/jpr/datasets/

#the following packages are needed to run the codes.

#calling libraries
library(mlr)
library(here)
library(tidyverse)
library(mlbench)
library(kernlab)
library(readr)
library(parallelMap)
library(parallel)
library(e1071)


#loading and exploring the US signals dataset
signals <- read_csv(here("class_presentations",
                         "2021-02-16-NaiveBayes-SVM-TubaSendinc",
                         "signals.csv"))

#converting data into tibble
signalsTib <- as_tibble(signals)

#using the map_dbl() function to show missing values
map_dbl(signalsTib, ~sum(is.na(.))) #no missing values

#mean of the democracy variable
mean((signals$democracy), na.rm=TRUE) 
#mean of democracy variable is 0.383826, 
#so our prior probability is .38 (38% of observations are democracies)

#subsetting the data for the bar chart
signals_desc = subset(signals, select = c(us_troops_dum, us_arms_dum, us_pact, us_vis, us_ex, us_econaid_dum, us_words_dum, democracy) )

#converting all variables into factor variables; I do this to make the variables appropriate to illustrate in the bar chart:
signals_desc[sapply(signals_desc, is.numeric)] <- lapply(signals_desc[sapply(signals_desc, is.numeric)], as.factor)

# bar chart for US signals of support. I do this only for the US signals to save space.
demUntidy <- gather(signals_desc, "Variable", "Value", -democracy)
ggplot(demUntidy, aes(democracy, fill = Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_bar(position = "fill") +
  theme_bw()

#subsetting the data used in the naive bayes classification
signals = subset(signals, select = -c(ccode, country, year, polity2, us_troops_dum,uk_troops_dum, fra_troops_dum,rus_troops_dum, chi_troops_dum,us_arms_dum,uk_arms_dum, fra_arms_dum, rus_arms_dum,chi_arms_dum,us_words_dum, us_econaid_dum ) )

#converting binary variables into factors. I keep the numeric variables numeric.
names <- c(1:9,11:15,17:21)
signals[,names] <- lapply(signals[,names] , factor)

#training the model for naive bayes classification

#preparing the train and test datasets
set.seed(123) #setting the seed for reproducibility
testrow <- sample(seq_len(nrow(signals)), size = 1500)
test <- signals[testrow, ]
train <- signals[-testrow, ]

#Creating the task: I set the democracy variable as the classification target
demTask <- makeClassifTask(data = train, target = "democracy")

#I supply the classif.naiveBayes to the makeLearner function
bayes <- makeLearner("classif.naiveBayes")
bayesModel <- train(bayes, demTask)

#extract prior probabilities and likelihoods from the naive bayes model
getLearnerModel(bayesModel)

#cross-validating the naive bayes model

#I use 10-fold cross-validation with 5 iterations to evaluate the model performance
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 5,
                          stratify = TRUE)

bayesCV <- resample(learner = bayes, task = demTask,
                    resampling = kFold,
                    measures = list(mmce, acc, fpr, fnr)) #I ask the resample function to show the overall accuracy, false positive, and false negative rates
bayesCV$aggr

#the model correctly predicts 69% of cases in the CV.
#false positive rate is .73
#false negative rate is .05

#calculating the predicted probabilities in the test data
predsNB <- data.frame(predict(bayesModel, newdata = test,type="class"))

#looking at confusion matrix to evaluate the performance of the NB algorithm 
confusionmatrixNB <- table(predsNB$truth, predsNB$response)
prop.table(confusionmatrixNB,1) 
#in this confusion matrix, rows represent the actual cases and the columns represent the predicted cases. row percentages are shown.
#25% of democracies are correctly classified as democracies
#94% of autocracies are correctly classified as autocracies

#laplace smoothing

NBlaplace <- naiveBayes(democracy ~., data=train, laplace = 3)
laplacepred <- predict(NBlaplace, newdata = test,type="class")
confusionmatrixlaplace <-table( test$democracy,laplacepred, dnn=c("Actual","Prediction"))
prop.table(confusionmatrixlaplace, 1) 

#SUPPORT VECTOR MACHINE ALGORITHM

#here, I use a different dataset, because I need continous variables to be able to run the SVM.
#I use arms transfers and troop deployments from the US, the UK, France, Russia and China.
#Arms transfers and troop deployments are continuous variables
#I will try to predict democracy in a country. 
#Democracy is a binary indicator that scores 1 when a country is democracy according to the Polity Project.

#loading and exploring the signals dataset
support_signals <- read_csv(here("class_presentations",
                                 "2021-02-16-NaiveBayes-SVM-TubaSendinc",
                                 "signals.csv"))

#converting the data into tibble
support_signalsTib <- as_tibble(support_signals)

#checking missing values
map_dbl(support_signalsTib, ~sum(is.na(.))) #no missing values

#dropping variables that are not needed in the analysis
support_signals = subset(support_signalsTib, select = c(democracy, us_troops, uk_troops, rus_troops, chi_troops, fra_troops, us_arms, uk_arms, rus_arms, chi_arms, fra_arms) )

#converting democracy variable into a factor variable, this is necessary for the SVM algorithm
support_signals$democracy=as.factor(support_signals$democracy)

#creating the training and test sets
set.seed(123) #setting the seed for reproducibility
testrow2 <- sample(seq_len(nrow(support_signals)), size = 1500)
test2 <- support_signals[testrow2, ]
train2 <- support_signals[-testrow2, ]

#creating the task
demTask2 <- makeClassifTask(data = train2, target = "democracy")

#make learner
svm <- makeLearner("classif.svm")

getParamSet("classif.svm")

#defining the hyperparameter space for tuning
kernels <- c("polynomial", "radial", "sigmoid")

svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", values = kernels),
  makeIntegerParam("degree", lower = 1, upper = 3),
  makeNumericParam("cost", lower = 0.1, upper = 10),
  makeNumericParam("gamma", lower = 0.1, 10))

#defining random search
set.seed(123) #setting seed for reproducibility

randSearch <- makeTuneControlRandom(maxit = 20)

#holdout CV with 2/3 split
cvForTuning <- makeResampleDesc("Holdout", split = 2/3)

#performing hyperparameter tuning

parallelStartSocket(cpus = detectCores() - 1)
tunedSvmPars <- tuneParams("classif.svm", task = demTask2,
                           resampling = cvForTuning,
                           par.set = svmParamSpace,
                           control = randSearch)

parallelStop()

#extracting the winning hyperparameter values from tuning
tunedSvmPars
tunedSvmPars$x #2nd degree polynomial function gave the model that performs the best

#training the model with tuned hyperparameters
#building the model with the best performing combination, i.e., 2nd degree polynomial function
tunedSvm <- setHyperPars(makeLearner("classif.svm"),
                         par.vals = tunedSvmPars$x)

#training the model with the tuned hyperparameter
tunedSvmModel <- train(tunedSvm, demTask2)

#cross-validating the model-building process

#define CV strategy. 3-fold CV.
outer <- makeResampleDesc("CV", iters = 3)

#make wrapped learner
svmWrapper <- makeTuneWrapper("classif.svm", resampling = cvForTuning,
                              par.set = svmParamSpace,
                              control = randSearch)
#nested CV
parallelStartSocket(cpus = detectCores())
cvWithTuning <- resample(svmWrapper, demTask2, resampling = outer)
parallelStop()

#extracting the cross-validation result
cvWithTuning

1-0.3435550
#[1] 0.656445 66% of observations are correctly classified as democracies or nondemocracies.

predsSVM <- data.frame(predict(tunedSvmModel, newdata = test2))

#looking at confusion matrix to evaluate the performance of the SVM algorithm 
confusionmatrixSVM <- table(predsSVM$truth, predsSVM$response)
prop.table(confusionmatrixSVM, 1) 
#19% of democracies correctly classified as democracies
#94% of autocracies correctly classified as autocracies.

#Now, I train a NB model where I use the same covariates used in the SVM above to make a direct comparison of NB and SVM.
bayesModel2 <- train(bayes, demTask2)

#calculating the predicted probabilities of NB
predsNB2 <- data.frame(predict(bayesModel2, newdata = test2,type="class"))
confusionmatrixNB2 <- table(predsNB2$truth, predsNB2$response)
prop.table(confusionmatrixNB2, 1) 
#11% of democracies correctly classified
#96% of autocracies correctly classified

#Results show that SVM is somehow superior.


