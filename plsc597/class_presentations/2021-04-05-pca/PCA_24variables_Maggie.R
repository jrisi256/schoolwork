#dataset from paper Casali, N., Feraco, T., Ghisi, M., & Meneghetti, C. (2020). “Andrà tutto bene”: Associations Between Character Strengths, Psychological Distress and Self-efficacy During Covid-19 Lockdown. Journal of Happiness Studies. https://doi.org/10.1007/s10902-020-00321-w
# Character Strengths, Psychological Distress and Self‐efficacy During Covid‐19 Lockdown
# N=944, Italian respondents, online survey

#IV/Predictors

# Author: Meng Qi Liao

#character strength (transcendence, interpersonal, openness and restraint): 24
# 1) Appreciation of beauty 
# 2) Bravery
# 3) Creativity
# 4) Curiosity
# 5) Fairness 
# 6) Forgiveness 
# 7) Gratitude 
# 8) Honesty 
# 9) Hope 
# 10) Humility 
# 11) Humor 
# 12) Judgment 
# 13) Kindness 
# 14) Leadership 
# 15) Love
# 16) Love of learning 
# 17) Perseverance 
# 18) Perspective 
# 19) Prudence 
# 20) Self-regulation 
# 21) Social intelligence 
# 22) Spirituality 
# 23) Teamwork
# 24) Zest 

#Dependent variables

# Psychological distress: DASS-21 (Depression, Anxiety, and Stress Scales-21) 
# Mental Health: GHQ-12 (General Health Questionnaire-12)
# Self efficacy: SEC (Self-Efficacy measure for Covid-19)

#install the packages below first if you haven't done so
library(here)
library(GGally)
library(tidyverse)

#import the dataset
dataset =  read.csv(here("class_presentations","2021-04-05-pca", 'DB.csv'))
data_tib <- as_tibble(dataset)


#creating a new variable, whether they are psychologically distressed or not
#there are three dimensions of distress, if any of the dimension is met (anxiety, stress or depression), I will recode them to 1 (true)
data_tib$indicator <- (data_tib$anxiety + data_tib$depression + data_tib$stress)
#if the indicator is 1,2,or 3 (essentially, bigger than 0), then it means this person has some distress related issues with COVID 19
data_tib$distress <- recode(data_tib$indicator, "0"=0, "1"=1,"2"=1, "3"=1)
#now we have our dependent variable, distress, make it a factor, and clean the dataset to retain only relevant variables
fctrs <- c("distress")
datasetclean = data_tib %>%
  mutate_at(.vars = fctrs, .funs = factor) %>%
  select(distress, Appreciation_of_beauty,	Bravery,	Creativity,	Curiosity,	Fairness,	Forgiveness,	Gratitude,	Honesty,	Hope,	Humilty,	Humor,	Judgment,	Kindness,	Leadership,	Love,	Love_of_learning,	Perseverance,	Perspective,	Prudence,	Self_regulation,	Social_intelligence,	Spirituality,	Teamwork,	Zest)

datasetclean
#find missing values
map_dbl(datasetclean, ~sum(is.na(.)))
#really few cases, so I will just omit the missing values
datasetclean <- na.omit(datasetclean)
datasetclean
#only 1 participant's data is deleted 

#next we can separate the dataset into training set and testset
set.seed(163)
testrow <- sample(seq_len(nrow(datasetclean)), size = 284)
testset <- datasetclean[testrow, ]
trainset <- datasetclean[-testrow, ]

#next we can start to plot the  data with ggpairs(), which will probably take 5 minutes, not recommending running it because we have so many variables...
ggpairs(trainset, mapping = aes(col = distress)) +
  theme_bw()
#therefore, only use the first 6 variables for illustration
ggpairs(trainset[1:7], mapping = aes(col = distress)) +
  theme_bw()

#maybe curiosity, forgiveness, seem to be more useful, but really hard to tell...

#performing PCA
# The center argument controls whether the data is mean-centered before applying PCA, and its default value is TRUE. 
# We should always center the data before applying PCA because this removes the intercept and forces the principal axes to pass through the origin.
# The scale argument controls whether the variables are divided by their standard deviations to put them all on the same scale as each other, and its default value is FALSE.
# Since the items are not on the same scale, so we have to specify the scale to be true
pcatrain <- select(trainset, -distress) %>% 
  prcomp(center = TRUE, scale = TRUE)
pcatrain
#using the summary function, we can get a breakdown of the importance of each of the principal components.
# The Proportion of Variance row tells us how much of the total variance is accounted for by each principal component.
summary(pcatrain)

#Calculating variable loadings to interpreting the principal components
map_dfc(1:24, ~pcatrain$rotation[, .] * sqrt(pcatrain$sdev ^ 2)[.])
#to help us interpret this table, I will save it as a data table
dfloading=map_dfc(1:24, ~pcatrain$rotation[, .] * sqrt(pcatrain$sdev ^ 2)[.])
dfloading = data.frame(dfloading, row.names = c("Appreciation_of_beauty",	"Bravery",	"Creativity",	"Curiosity",	"Fairness",	"Forgiveness",	"Gratitude",	"Honesty",	"Hope",	"Humilty",	"Humor",	"Judgment",	"Kindness",	"Leadership",	"Love",	"Love_of_learning",	"Perseverance",	"Perspective",	"Prudence",	"Self_regulation",	"Social_intelligence",	"Spirituality",	"Teamwork",	"Zest"))
view(dfloading)
#The variable loadings tell us how much each of the original variables correlates with each of the principal components. 
#Here we can use zest and humility as an example, zest is more correlated with the first variable/factor
# while humility is more likely to correlated with the second variable/factor

#Plotting the PCA results
library(factoextra)
#get_pca() function: grab the information from our PCA model so we can apply facto- extra functions to it.
pcaDat <- get_pca(pcatrain)
#The fviz_pca_biplot() function draws a biplot
fviz_pca_biplot(pcatrain, label = "var")
#this biplo simultaneously plots the component scores, and the variable loadings for the first two principal components. 
#The dots show the component scores for each of the participant against the first two principal components, 
#and the arrows indicate the variable loadings of each variable
#unfortunately in this case, we didn't see two clear clusters, and the variables are generally pointing at one direction


#fviz_pca_var() function draws a variable loading plot
# this shows the same variable loading arrows as in the biplot, 
# the axes represent the correlation of each of the variables with each principal component.
fviz_pca_var(pcatrain)


# The fviz_screeplot() function draws a scree plot. 
fviz_screeplot(pcatrain, addlabels = TRUE, choice = "eigenvalue") 
fviz_screeplot(pcatrain, addlabels = TRUE, choice = "variance")
#rule of thumb, keep the principal components that cumulatively explain at least 80% of the variance 
# (then we need all 10 principal components)
#or to retain all principal components with eigenvalues of at least 1
# then we need PCA1, 2,3,4 and 5
#look for an “elbow” in the scree plot and exclude principal components beyond the elbow
# then maybe we only would have 1
# I will retain 5 principal components for now

#see how well the classification perform by mapping the distress/non-distress labels
distressPcatrain <- trainset %>%
  mutate(PCA1 = pcatrain$x[, 1], PCA2 = pcatrain$x[, 2], PCA3 = pcatrain$x[, 3], PCA4 = pcatrain$x[, 4], PCA5 = pcatrain$x[, 5])
#plot the result
ggplot(distressPcatrain, aes(PCA1, PCA2, PCA3, PCA4, PCA5, col = distress)) +
  geom_point() +
  theme_bw()
#you will get warning messages because we have 5 variables
# Add 95% confidence ellipses to the plot of PCA1 versus PCA2
ggplot(distressPcatrain, aes(PCA1, PCA2, PCA3, PCA4, PCA5, col = distress)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()


#see how the classification perform in the new data set, testset
pcatest <-as.data.frame(predict(pcatrain, newdata = testset))
testsetnew <-cbind(testset,pcatest)


#see how well the classification perform by mapping the distress/non-distress labels\
#plot the result
ggplot(testsetnew, aes(PC1, PC2, PC3, PC4, PC5, col = distress)) +
  geom_point() +
  theme_bw()
# Add 95% confidence ellipses to the plot of PCA1 versus PCA2
ggplot(testsetnew, aes(PC1, PC2, PC3, PC4, PC5, col = distress)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()

# these variables did not do a good job classifying distress vs. non distress

## in the next section, I will train two different classification model (simple logistic regression) with the training set, 
# either using all 24 variables or the 5 principal components

# make dataset for glm training
logit_data <- data.frame(trainset,pcatrain$x)

# full observed covariate and PCA models
glm_obs <- glm(distress~Appreciation_of_beauty + Bravery + Creativity + Curiosity + Fairness + Forgiveness + Gratitude +Honesty + Hope + Humilty + Humor + Judgment + Kindness + Leadership + Love + Love_of_learning + Perseverance + Perspective + Prudence + Self_regulation + Social_intelligence + Spirituality + Teamwork + Zest, data=logit_data,family="binomial")
glm_pca5 <- glm(distress~PC1+PC2+PC3+PC4+PC5,data=logit_data,family="binomial")
# glm_pca1 <- glm(distress~PC1,data=logit_data,family="binomial")

#test the model in the testset
library(MLmetrics)
predict(glm_obs, newdata = testsetnew)
probobs = predict(glm_obs, newdata = testsetnew)
prauc_obsnew <- PRAUC(probobs,testsetnew$distress)
prauc_obsnew
#0.3394442

predict(glm_pca5, newdata = testsetnew)
probpca = predict(glm_pca5, newdata = testsetnew)
prauc_pcanew <- PRAUC(probpca,testsetnew$distress)
prauc_pcanew
# 0.3815217

# #elbow selection
# predict(glm_pca1, newdata = testsetnew)
# probpca = predict(glm_pca1, newdata = testsetnew)
# prauc_pcanew <- PRAUC(probpca,testsetnew$distress)
# prauc_pcanew
# # 0.1899556

#PCA with 5 pa seems to improve the classification

#However
# stepwise variable selection via BIC
step_obs <- step(glm_obs,k=log(nrow(logit_data)))
step_pca5 <- step(glm_pca5,k=log(nrow(logit_data)))
# step_pca1 <- step(glm_pca1,k=log(nrow(logit_data)))


# predicted probabilities in the test set
pred_obs <- predict(step_obs,testsetnew,type='response')
pred_pca5 <- predict(step_pca5,testsetnew,type='response')

#calculating PRAUC
prauc_obs_step <- PRAUC(pred_obs,testsetnew$distress)
prauc_pca5_step <- PRAUC(pred_pca5,testsetnew$distress)

        
prauc_obs_step
#0.3748695

prauc_pca5_step
# 0.3134913

#PCA is actually performing worse...
#maybe we could either choose to use PCA or stepwise feature selection
#if we compare that the AUC of model with pca (auc=0.3815217) performs better than the stepwise model(0.3748695)
