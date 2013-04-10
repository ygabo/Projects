from __future__ import division
import scipy.io
import numpy as np
import random
from sklearn.utils import shuffle

class Classifier ( object ):
    def __init__ ( self ):
        self.params = [ 'logpi', 'logtheta', 'log1theta' ]

    def inner_fit ( self , X , y ):
        N = len( y )

        # num of happy tweets
        N_1  = np.sum( y )
        # num of sad tweets
        N_0  = N - N_1

        # ratio of happy/sad tweet
        Pi_0 = ( N_0 + 2 / N )
        Pi_1 = ( N_1 + 2 / N )

        #output is an array, N_jc[0] is the count
        #of how many 'obamas' when happy/sad
        N_j0 = (1-y)*X
        N_j1 = y*X

        Theta_j0 = ( ( N_j0 + 1 ) / ( N_0 + 2 ) )
        Theta_j1 = ( ( N_j1 + 1 ) / ( N_1 + 2 ) )

        logpi = [ np.log( Pi_0 ), np.log( Pi_1 ) ]
        self.logpi.append( np.array( logpi ) )
        self.logtheta.append( np.array([ np.log( Theta_j0 ), np.log( Theta_j1 ) ]) )
        self.log1theta.append( np.array( [ np.log1p( -1*Theta_j0 ), np.log1p( -1*Theta_j0 ) ] ) )
        
    def fit ( self , X , y , normal=True, data=0):
        self.logpi = []
        self.logtheta = []
        self.log1theta = []
        
        if normal is True:
            self.inner_fit( X,y )
        else:
            for i in xrange(100):
                data_train = shuffle(data)
                if i == 1:
                    print data_train.shape[0]
                data_train = data_train#[:0.9*data_train.shape[0]]
                X_i = data_train[:,1:]
                y_i = data_train[:,0]
                y_i = np.array(y_i.todense()).flatten()
                self.inner_fit( X_i,y_i )


      #  print self.logtheta[0]


    def predict ( self , X ):
        y_hat       = [0 for x in xrange(X.shape[0])]
        out_y_hat = [0 for x in xrange(X.shape[0])]
        y           = [0 for x in xrange(X.shape[0])]

        #return self.inner_predict( X, 1 )
        
        for i in xrange(1):
            out_y_hat = self.inner_predict( X, i )
            for o in xrange(100):
                y_hat[o] = y_hat[o] + out_y_hat[o]

        print y_hat[:50]
        for i in xrange(X.shape[0]):
            if y_hat[i] > 50:
                y[i] = 1
            else:
                y[i] = 0

        print np.array(y_hat).shape
        return y
        
    def inner_predict ( self , X, i=0 ):
        
        Li0 = self.logpi[i][0]
        Li1 = self.logpi[i][1]
        Li1 += self.logtheta[i][1]*X.T + ( self.log1theta[i][1]*scipy.sparse.eye(X.shape[1],X.shape[0]) - self.log1theta[i][1]*X.T )  
        Li0 += self.logtheta[i][0]*X.T + ( self.log1theta[i][0]*scipy.sparse.eye(X.shape[1],X.shape[0]) - self.log1theta[i][0]*X.T ) 

        LAE = scipy.misc.logsumexp(Li0+Li1)
        pi0 = np.exp( Li0 - LAE )
        pi1 = np.exp( Li1 - LAE )
        y_hat = [0 for x in xrange(X.shape[0])]
        
        for i in xrange(X.shape[0]):
            if pi0.T[i] > pi1.T[i]:
                y_hat[i] = 0
            else:
                y_hat[i] = 1

        return y_hat
    
    def save_params ( self , fname ):
        params = dict ([( p , getattr ( self , p )) for p in self.params ])
        np.savez ( fname , ** params )

    def load_params ( self , fname ):
        params = np.load ( fname )
        for name in self.params :
            setattr ( self , name , params [ name ])

    
