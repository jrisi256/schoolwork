# -*- coding: utf-8 -*-
"""
Created on Fri Oct 21 14:26:13 2016

@author: seguin
"""
import pandas as pd 
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import f1_score
from sklearn.metrics import recall_score
from sklearn.metrics import precision_score
from sklearn.metrics import accuracy_score
from sklearn.model_selection import cross_val_predict
from sklearn.feature_extraction.text import TfidfVectorizer
import statsmodels.api as sm

df = pd.read_csv(r'/Users/czs792/Dropbox/Teaching/SoDA 502 Fall 2022/Code and Data/black names.csv')
df.describe
df['intercept'] = 1 
df.sort_values(by='percent_black', ascending=False)
summary = df.groupby('black_name').mean()
black_names = df[df["percent_black"] > 30]
df['percent_black'] = df['percent_black'].apply(lambda x: x**2 + 5)
df.drop('percent_black', axis=1)


df['ghetto'] = df['comments'].str.contains('ghetto',  na=False, regex=False).astype(int)
df['trashy'] = df['comments'].str.contains('trashy',  na=False, regex=False).astype(int)

y = df['black_name'].values
X = df[['intercept', 'ghetto', 'trashy', 'educ_low', 'educ_med', 'educ_high']].values

# instantiate a logistic regression model, and fit with X and y
model = LogisticRegression()
#model = RandomForestClassifier(n_estimators = 100)
model = model.fit(X, y)
print(model.coef_)

# check the accuracy on the training set
model.score(X, y)
y.mean()


logit_model=sm.Logit(y,X)
result=logit_model.fit()
print(result.summary())

#vectorize the comments
train = df
train = train.dropna(subset=['comments'])
train = train.reset_index()
train['comments'] = train.comments + " _" + train.educ_cat

vectorizer = TfidfVectorizer(analyzer = "word", tokenizer = None, preprocessor = None, stop_words = 'english', max_features = 1000, ngram_range=(1, 3)) 