from __future__ import division
import classify
import scipy.io
import numpy as np
import random
import matplotlib.pyplot as pl
from sklearn.utils import shuffle


    

data = scipy.io.mmread("tweets_small.mtx").tocsr()
data_small = scipy.io.mmread("tweets_small.mtx").tocsr()
data = shuffle(data)
nbayes = classify.Classifier()

#0 rows, 1 = columns
#K          = data.shape[0]
#train      = 0.9
data_train = data

X      = data_train[:,1:]
y      = data_train[:,0]
X_test = data_small[:,1:]
y_test = data_small[:,0]

y      = np.array(y.todense()).flatten()
y_test = np.array(y_test.todense()).flatten()

#f = open('experiment.txt', 'r+')

nbayes.fit( X,y )
y_hat = nbayes.predict(X_test)
print ( str((sum( 1 - abs( y_test - y_hat ) )/ y_test.size ) * 100 ) )

#f.close()

#print Li1.shape
#Li1 = Li1 + X_test*logtheta[1]  + ( log1theta[1] - X_test*log1theta[1] )



##Li0 = logpi[0] + logtheta[0] + log1theta[0]
#Li1 = logpi[1] + logtheta[1] + log1theta[1]

#p_i0 = np.exp( Li0 - scipy.misc.logsumexp( Li0+Li1 ) )
#p_i1 = np.exp( Li1 - scipy.misc.logsumexp( Li0+Li1 ) )
#logtheta = np.log( Theta_jc )
#log1theta = np.log1p( -1*Theta_jc )
#print logtheta[50:100]
#L = logpi + logtheta + log1theta
#print L.size
#Lic = [ sum(L[0]), sum(L[1]) ] 
#print y.size
#print y_test

#print L
