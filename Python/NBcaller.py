from __future__ import division
import NaiveBayesClassifier
#import nb
import scipy.io
import numpy as np
import random
from sklearn.utils import shuffle
from time import clock, time

start = time()
data = scipy.io.mmread("tweets.mtx").tocsr()
print time()- start
#0 rows, 1 = columns
data = shuffle(data)

K          = data.shape[0]
train      = 0.9
data_train = data#[:train*K]
data_test  = data#[train*K:]

#X      = data_train[:,1:]
#y      = data_train[:,0]
X_test = data_test[:,1:]
y_test = data_test[:,0]

#y      = np.array(y.todense()).flatten()
y_test = np.array(y_test.todense()).flatten()

nbayes = classify.Classifier()
nbayes.load_params('params.npz')
#y_hat = nbayes.fit(X,y,)# False, data_train)
#nbayes.save_params('params')
start = time()
y_hat = nbayes.predict(X_test)
print time() - start
print (sum( 1 - abs( y_test - y_hat ) )/ y_test.size ) * 100 

