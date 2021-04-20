library(here)
library(quanteda)
library(glmnet)

########################

# objective:
# predict the number of retweets using the date of a tweet and the average embedding of non-stopwords in tweets 
  ##basically if given 1 tweet can we predict how many retweets that tweet will get using the average   word embedding value and the date the tweet was posted 
  ## compare word embedding method to a dtm method
# https://psu.app.box.com/s/ktk52t5dyblp2mfnwrsvjyildsducz6c

#######################

# load data
set.seed(1234)
sample_twts <- readRDS(file=here("class_presentations",
                                 "2021-04-19-word-embeddings",
                                 "trump_sample.rds"))


sample_twts <- subset(sample_twts,isRetweet==F)

# Pretrained GLoVE embedding from Stanford NLP
# Trained on Wikipedia, 100d
glove6b <- read.csv(here("class_presentations",
                         "2021-04-19-word-embeddings",
                         "glove_6B_100d_copy.csv"),
                    stringsAsFactors = F,
                    header = F)
  #can take a couple of minutes

glove6b[1:10,1:10]


# create matrix in which to store average embdedding
average_embedding <- matrix(NA,nrow(sample_twts),100)

# tokenize text (breaks down text)
tokensi <- tokens(tolower(sample_twts$text))
tokensi <- tokens_remove(tokensi, stopwords("english"))

# loop through tweets
for(i in 1:nrow(sample_twts)){
  
  # find word vectors that match tweet text
  vector_ids <- match(tokensi[[i]],glove6b[,1])
  
  # if there are any matches, find word vectors
  if( length( which( !is.na(vector_ids ))) >0 ){
    
    # extract word vectors
    mean_vectors <- glove6b[vector_ids, -1]
    
    # mean vector
    mean_vector <- apply(mean_vectors,2,mean,na.rm=T)
    
    # store mean vector in average_embedding matrix
    average_embedding[i,] <- mean_vector
    
  }
  
}

# give some simple column names
colnames(average_embedding) <- paste("d",1:100,sep="")

# use date and average embeddings as predictors
xmat <- cbind(average_embedding,tweet_date = as.numeric(sample_twts$date))

# filter out NAs
sample_twts_na <- sample_twts[!is.na(xmat[,1]), ]
xmat_na <- xmat[!is.na(xmat[,1]), ]

# select first 1000 as test (alread randomly ordered dataframe)
test_sample_twts_na <- sample_twts_na[1:1000, ]
test_xmat_na <- xmat_na[1:1000, ]

train_sample_twts_na <- sample_twts_na[-(1:1000), ]
train_xmat_na <- xmat_na[-(1:1000), ]

# use lasso to fit predictive model
#library(glmnet)
fit_retweets <- cv.glmnet(train_xmat_na,train_sample_twts_na$retweets,alpha=1)

# see which dimensions are selected into the model, and coefficients
coef_embedding <- coef(fit_retweets, s = "lambda.1se")

# what is the mean absolute test error?
predicted_rts <- predict(fit_retweets,newx=test_xmat_na)
mean(abs(predicted_rts-test_sample_twts_na$retweets))
#for each test observation we predict the number of retweets 
  #then calculate the absolute difference b/w predicted number of retweets and observed number of      retweets 
  #then take the average value of that difference

# see how that compares to document term matrix 
# predicting based solely on word counts, not word vectors
#library(quanteda)
dtm <- dfm(tokensi[which( !is.na(xmat[,1]) )])

xmat_dtm <- cbind(dtm,as.numeric(sample_twts_na$date))

train_xmat_dtm <- xmat_dtm[-(1:1000), ]

test_xmat_dtm <- xmat_dtm[1:1000, ]

fit_retweets_dtm <- cv.glmnet(train_xmat_dtm,train_sample_twts_na$retweets,alpha=1)

# see which tokens are selected into the model, and coefficients
coef_dtm <- coef(fit_retweets_dtm, s = "lambda.1se") 

# what is the mean absolute test error?
predicted_rts_dtm <- predict(fit_retweets_dtm,newx=test_xmat_dtm)
mean(abs(predicted_rts_dtm-test_sample_twts_na$retweets))

# embedding vector gets closer by around 48 retweets, on avearage
  #we WANT a lower absolute error
  #word embedding is the better model 
3987.665-4036.575 
