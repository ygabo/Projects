import numpy as np
import matplotlib.pyplot as pl

def ridge( X, y, d2 ):

    A = np.dot(X.T, X)
    B = A + ( d2 * np.identity(8) )
    C = np.linalg.inv( B )
    D = np.dot( C, X.T )
    
    theta = np.dot(D, y)

    return theta

def lasso(X, y, d2):

    theta_ = np.zeros( X.shape[1] )
    theta  = ridge( X, y, d2)
    a      = np.zeros( X.shape[0] )
    c      = np.zeros( X.shape[0] )

    #calculate 'a' outside
    for j in xrange( X.shape[1] ):
        for i in xrange( X.shape[0] ):
            a[j] += X[i][j]**2
        a[j] = a[j]*2

    #converge loop
    while np.sum( np.abs( theta - theta_ ) ) > 1e-5:

        #update theta_
        theta_ = theta.copy()

        for j in xrange( X.shape[1] ):
            c[j] = 0

            #calculate 'c'
            for i in xrange( X.shape[0] ):
                c[j] += X[i][j] * ( y[i] - np.dot(theta, X[i]) + theta[j]*X[i][j] )
            c[j] = c[j]*2

            #update theta_j
            if c[j] < ( -1 * d2 ):
                theta[j] = ( c[j] + d2 ) / a[j]
            elif c[j] > d2:
                theta[j] = (c[j] - d2 ) / a[j]
            else:
                theta[j] = 0
        
    return theta

#initialize and load data needed
X   = np.loadtxt('prostate.data')
fig = pl.figure()
d2  = np.logspace(-5, 10, num=100)

mean_sd = []
U,S,VT  = np.linalg.svd(X, full_matrices=False)

#loop through all deltas
#for every delta
#split data to 10 pieces, 1 test, 9 train
#normalize data
#get theta
#use theta on test data (split beforehand into X_test and y_test)
#multiply theta with X_test, answer is y_hat
#error is between y_hat and y_test (a lil more complicated)
#thats one error for one fold, do ten times, with other folds
#using different batch of test and train data
#now have 10 errors for one delta
#compute mean for the ten errors for y axis in plot
#get the standard deviation for the ten errors for the bar size
#get df for x axis in plot
#repeat for all other deltas
for i in xrange( d2.size ):
    #10 folds
    ten_errors = [0]*10
    ten_thetas = [0]*10
    for j in xrange( 10 ):
        #combine two parts of the data
        #ie. if test is rows 10-20
        #combine rows 0-10 and rows 21-97
        data_train = np.vstack((X[0:j*10,:],X[min(j*10+10,97):,:]))
        data_test  = np.copy(  X[j*10:min(j*10+10,97),:] )

        #split into test and train data
        y_train = data_train[:,-1]
        X_train = data_train[:,0:-1]

        y_test = data_test[:,-1]
        X_test = data_test[:,0:-1]

        #standardize data for current fold
        y_mean = np.mean(y_train)
        X_mean = np.mean(X_train, axis=0)
        X_sd   = np.std(X_train, axis=0)
        
        y_train -= y_mean
        X_train -= X_mean
        X_train /= X_sd
        
        y_test -= y_mean
        X_test -= X_mean
        X_test /= X_sd
        
        #get theta
        theta = lasso( X_train, y_train, d2[i])

        #prediction
        y_hat = np.dot( X_test, theta )

        #take note of errors
        ten_errors[j] = np.sum( ( y_hat - y_test )**2 )
        ten_thetas[j] = theta

        #end of fold loop    

    #calculate degree of freedom
    df = np.sum( np.absolute( ten_thetas ) ) 

    #take note of mean of ten errors, SD of ten errors and the current
    #degree of freedom
    mean_sd.append( [np.mean(ten_errors, axis=0), np.std(ten_errors, axis=0), df])

    #end of delta loop

#from a list convert to array
mean_sd = np.array( mean_sd )

#plot error bar
pl.errorbar(mean_sd.T[2], mean_sd.T[0], mean_sd.T[1])
pl.show()
