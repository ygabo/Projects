from __future__ import division
import classify
import scipy.io
import numpy as np
import random
import matplotlib.pyplot as pl
from sklearn.utils import shuffle

class Classifier ( object ):
    def __init__ ( self ):
        self.params = [ 'logpi', 'logtheta', 'log1theta' ]

    def fit ( self , X , y ):
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
        self.logpi = np.array( logpi ) 
        self.logtheta = np.array([ np.log( Theta_j0 ), np.log( Theta_j1 ) ])
        self.log1theta = np.array( [ np.log1p( -1*Theta_j0 ), np.log1p( -1*Theta_j0 ) ] )

        save_params( self, 'params' )

    def predict ( self , X ):
        
        Li0 = self.logpi[0]
        Li1 = self.logpi[1]
        Li1 += self.logtheta[1]*X.T + ( self.log1theta[1]*( np.eye(X.shape[1],X.shape[0]) - X.T ) ) 
        Li0 += self.logtheta[0]*X.T + ( self.log1theta[0]*( np.eye(X.shape[1],X.shape[0]) - X.T ) )

        LAE = scipy.misc.logsumexp(Li0+Li1)
        pi0 = np.exp( Li0 - LAE )
        pi1 = np.exp( Li1 - LAE )
        y_hat = [0 for x in xrange(X_test.shape[0])]
        
        for i in xrange(X_test.shape[0]):
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

