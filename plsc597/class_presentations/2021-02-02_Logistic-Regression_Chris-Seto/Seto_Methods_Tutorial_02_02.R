#PLSC 597: Methods Tutorial -- Classification with Logistic Regression
#Christopher Seto
#February 2, 2021

###############################################################################
### RESEARCH QUESTIONS ###
# My tutorial today will investigate the predictors of generalized trust in the American public.
# What variables predict higher trust in others? lower trust?
# How accurately can a logistic regression model classify people by trust level?

### SPECIFIC GOALS ###
# The tutorial will cover the following topics:
# 1) cleaning/preparing data for fitting a logistic regression model
# 2) examining bivariate relationships between predictors and binary response
# 3) training a logistic regression model
# 4) testing model performance with cross validation
# 5) viewing and interpreting model parameters (coefficients)
      # a) logit coefficients
      # b) odds ratios
# 6) making out-of-sample predictions

### THE DATA ###
# Data are from the 2014 Chapman Survey on American Fears (CSAF).
# Annual cross sectional survey of the US (2014-2018) focused on fears, attitudes, public opinion.
# Sampled non-institutionalized US adults, from GFK Knowledge panel. N=1,573
# Available for public download from the PSU ARDA (https://www.thearda.com/Archive/Files/Descriptions/CSAF2014.asp)
# For application example, see (https://journals.sagepub.com/doi/full/10.1177/1462474520960038)
  #VARIABLES I USE TODAY
  #cantrust - asked respondents which they agreed with: (1) "most people can be trusted" (2) "you can't be too careful in life" (3) "it all depends"
  # ppgender - binary indicator of gender
  # ppage - continuous measure of age in years
  # attendan - 8-level measure of frequency of church attendance
  # SAGE_NGB - 4-level measure of how safe respondents feel in their neighborhood

### THE MODEL (based on Rhys 2020) ###
# models the log odds of belonging to a class as a linear function of input variables
# number of classes can be 2 (binomial) or more (multinomial). Today we focus on the binomial case
# ln(p/(p-1)) = B0+B1x1+B2x2+...+Bkxk
# Outputs a probability of class membership.
# Typically, sample is classified as belonging to the class with the highest probability.

###################################################################################
### LOGISTIC REGRESSION TUTORIAL ###
#################################################################################

##### 1) cleaning/preparing data for fitting a logistic regression model

#install the necessary packages

#install.packages("mlr")
#install.packages("tidyverse")

# this one allows me to read in a Stata .dta file
#install.packages("readstata13")

#this one is for some later performance metrics
#install.packages("MLmetrics")

#call libraries
library(here)
library(readstata13)
library(mlr)
library(tidyverse)
library(MLmetrics)

#replace file path with wherever you have stored the data
#setwd("C:/Users/chris/Box/SPRING_2021/Machine Learning/Methods_Tutorial")

# Import data. I use read.dta13 because I already had it as a Stata .dta file
# update the file path to wherever the data are stored on your machine.
data <- read.dta13(here("class_presentations",
                        "2021-02-02_Logistic-Regression_Chris-Seto",
                        "CSAF1_2014.DTA"))

#make the data into a tibble for easier viewing and manipulation
data_tib <- as_tibble(data)

#view tibble -- probably too many variables. Need to clean things up.
data_tib

#First, let's view the distribution of the response variable I'm interested in.
#The first line puts the data into a table and the second line displays the proportion of cases in each table cell
trust_table <- table(data_tib$cantrust)
prop.table(trust_table)

#Because I'd like to focus on binary classification today, I need to recode this variable as binary
#Let's make the most trusting people "1" and everyone else "0".
#This is a bit different from the Rhys (2020) example, where the response is already binary
data_tib$cantrust2 <- recode(data_tib$cantrust, "Most people can be trusted"=1, "You can't be too careful in life"=0, "It all depends"=0)

#Because I focus a lot on religion in my own research, I thought it would be interesting to include religious attendance as a predictor
#It seems plausible that religion might make people more trusting (though findings have been mixed)
#The 2014 CSAF includes an 8-category variable for frequency of religious attendance.
#For simplicity, I recoded this as a binary indicator of at least weekly attendance.
#Different cleaning process than Rhys (2020)
data_tib$church <- fct_collapse(data_tib$attendan,
                            weekly = c("Several times a week", "Weekly"),
                            not_weekly = c("Two to three times a month","Once a month","Several times a year","Once or twice a year","Less than once a year","Never")
)

#I'd also like to include gender, age, and feelings of neighborhood safety (SAFE_NGB) as predictors
#gender and feelings of safety are factor vars, as are trust (my outcome) and my newly created church variable
#I leave age as continuous so that we can later interpret coefficients of both categorical and continuous predictors 

#declare the factors variables
fctrs <- c("cantrust2", "ppgender", "church", "SAFE_NGB")

#create a "clean" dataset which only has the variables I want, and which knows which variables are factors
data_clean <- data_tib %>%
  mutate_at(.vars = fctrs, .funs = factor) %>%
  select(cantrust2, ppage, ppgender, church, SAFE_NGB)

# Rhys (2020) uses mean imputation to fill in missing values
# I don't have that much missingness in my data, so I will just use casewise deletion

#omit cases which have missing data
data_clean <- na.omit(data_clean)

#now we have fully cleaned data. N = 1,525

##### 2) examining bivariate relationships between predictors and binary response

#put cleaned data into a convenient format for plotting
data_for_plots <- gather(data_clean, key = "Variable", value = "Value", -cantrust2)

#violin plots for age (continuous) by trust level
#facet lets us show the distribution of age for each level of trust
data_for_plots %>%
  filter(Variable == "ppage") %>%
  ggplot(aes(cantrust2, as.numeric(Value))) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_bw()

#bar plots for gender, church attendance, and feelings of safety (factor vars)
#similar logic as before, but bar plots are a more appropriate format for factors
data_for_plots %>%
  filter(Variable == "ppgender" | Variable == "church" | Variable == "SAFE_NGB") %>%
  ggplot(aes(Value, fill = cantrust2)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_bar(position = "fill") +
  theme_bw()

#based on these bivariate associations, I expect church and safety to be salient predictors of trust
# Probably not gender. Maybe age. 

###### 3) training a logistic regression model

#Rhys (2020) has extra data to bring in later to use for predicting out of sample
#I don't have that, so I'm going to separate out 400 test cases for this use. 
#Model will be trained and validated using the other 1,125 cases

## set the seed for reproducible random sampling
set.seed(1234)
#randomly sample 400 numbers from a list of integers equal to the number of cases
test_rows <- sample(seq_len(nrow(data_clean)), size = 400)
#any row that was sampled goes into test data
test_data <- data_clean[test_rows, ]
#any row that was not sampled goes into training data
train_data <- data_clean[-test_rows, ]

#Next, we set the task. It is a classification task, using "train_data"
# And we want to classify based on the variable "cantrust2"
#I also specify that "positive" is considered 1. I didn't do this the first time and things were backwards during cross validation.
trust_task <- makeClassifTask(data = train_data, target = "cantrust2", positive = 1)

#make the learner. "classif.logreg" specifies that we want to learn a logistic regression model
#predict.type = "prob" specifies that we want to see predicted probabilities of class membership
#(this will matter later when we use the model to make predictions)
log_reg <- makeLearner("classif.logreg", predict.type = "prob")

#train the model, using the task and learner that we just defined.
model <- train(log_reg, trust_task)

##### 4) testing model performance with cross validation

#specify stratified, 5-fold CV, 20 times
#different fold and reps than Rhys 2020
kFold <- makeResampleDesc(method = "RepCV", folds = 5, reps = 20, stratify = TRUE)

#apply the specified CV procedure
#like Rhys (2020), I ask for overall accuracy (acc), false positive rate (fpr), and false negative rate (fnr)
model_CV <- resample("classif.logreg", trust_task,
                       resampling = kFold,
                       measures = list(acc, fpr, fnr))
model_CV

#Accuracy is not great - the model seems to mostly just classify everyone as non-trusting
#let's compare to that proportion
trust_table <- table(train_data$cantrust2)
prop.table(trust_table)

#probably need some better predictors to improve this. As is, I don't have a lot of inputs that matter for trust

##### 5) viewing and interpreting model parameters (coefficients)

#turn mlr model object into R model object
model_data <- getLearnerModel(model)
summary(model_data)

#a) view coefficients (ie change in log odds per unit change in predictor)
coef(model_data)
#e.g., each year increase in age corresponds to .02 increase in log odds of trust vs non-trust
#e.g., weekly church attenders have an average of .25 higher log odds of trust compared to non-weekly church attenders

#b) view odds ratios (ie factor change in odds per unit change in predictor) and CI (specified by confint)
exp(cbind(Odds_Ratio = coef(model_data), confint(model_data)))
#e.g., people who feel "not at all safe" have 89% lower odds of trust than people who feel "very safe" (calculated by subtracting .11 from 1)

##### 6) making out-of-sample predictions

#We can also use our model to make predictions out of sample. 
#specify that newdata = test_data, the 400 cases we pulled out earlier
pred_data <- data.frame(predict(model, newdata = test_data))

#predicted probabilities are displayed because we asked for this earlier
#as shown, the first person in our test sample has a 79% probability of not trusting 
#and a 21% probability of trusting. As such, they were classified as 0.

#Now, let's compute accuracy rate by comparing the truth and response columns
pred_data <- pred_data %>%
  mutate(correct = case_when(
    truth == response ~ 1,
    truth != response ~ 0
  ))

#divide correct cases by total cases
acc <- sum(pred_data$correct)/nrow(pred_data)
acc

#Same for FPR
#first, create a variable that identifies everything that was true 0, but classified as 1
pred_data <- pred_data %>%
  mutate(fp = case_when(
    (truth == 0 & response == 1) ~ 1,
    (truth == 1 | response == 0) ~ 0
  ))

#the sum of this variable divided by the total true 0s is FPR
fpr <- sum(pred_data$fp)/(nrow(pred_data)-sum(as.numeric(as.character(pred_data$truth))))
fpr

#Same for FNR. create a variable that identifies all true 1s that were classified as 0
pred_data <- pred_data %>%
  mutate(fn = case_when(
    (truth == 1 & response == 0) ~ 1,
    (truth == 0 | response == 1) ~ 0
  ))

#The sum of this variable divided by total true 1s is FNR
fnr <- sum(pred_data$fn)/sum(as.numeric(as.character(pred_data$truth)))
fnr

# Area under precision-recall curve
PRAUC(y_pred = pred_data$prob.1,y_true=pred_data$truth)

# Area under ROC curve
AUC(y_pred = pred_data$prob.1,y_true=pred_data$truth)
################################################################################

#This concludes my tutorial on classification with logistic regression
#if you have follow up questions, please feel free to reach out (chs37@psu.edu)