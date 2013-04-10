import numpy as np
import matplotlib.pyplot as pl

def ridge( X, y, d2 ):

    A = np.dot(X.T, X)
    B = A + ( d2 * np.identity(8) )
    C = np.linalg.inv( B )
    D = np.dot( C, X.T )
    
    theta = np.dot(D, y)

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
    for j in xrange( 10 ):
        #combine two parts of the data
        #eg. if test is rows 10-20
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
        theta = ridge( X_train, y_train, d2[i])

        #prediction
        y_hat = np.dot( X_test, theta )

        #take note of errors
        ten_errors[j] = np.sum( ( y_hat - y_test )**2 )

        #end of fold loop    

    #calculate degree of freedom
    df = 0        
    for u in xrange(8):
        df += S[u] / ( S[u] + d2[i])

    #take note of mean of ten errors, SD of ten errors and the current
    #degree of freedom
    mean_sd.append( [np.mean(ten_errors, axis=0), np.std(ten_errors, axis=0), df])

    #end of delta loop

#from a list convert to array
mean_sd = np.array( mean_sd )

#plot error bar
pl.errorbar(mean_sd.T[2], mean_sd.T[0], mean_sd.T[1])
pl.show()
