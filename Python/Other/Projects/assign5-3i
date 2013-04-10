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

X   = np.loadtxt('prostate.data')
fig = pl.figure()
d2  = np.logspace(-5, 10, num=100)

y = X[:,-1]
X = X[:,0:-1]

y -= np.mean(y)
X -= np.mean(X, axis=0)
X /= np.std(X, axis=0)

dfa = ([])
theta0 = ([])
theta1 = ([])
theta2 = ([])
theta3 = ([])
theta4 = ([])
theta5 = ([])
theta6 = ([])
theta7 = ([])

for i in xrange( d2.size ):

    theta = lasso(X, y, d2[i])
    df    = 0
    
    theta0 = np.append(theta0, theta[0])
    theta1 = np.append(theta1, theta[1])
    theta2 = np.append(theta2, theta[2])
    theta3 = np.append(theta3, theta[3])
    theta4 = np.append(theta4, theta[4])
    theta5 = np.append(theta5, theta[5])
    theta6 = np.append(theta6, theta[6])
    theta7 = np.append(theta7, theta[7])
    
    df = np.sum( np.absolute( theta ) )

    dfa = np.append(dfa,df)
#print zip(dfa, theta1)
pl.plot(dfa, theta0, '-', dfa, theta1, '-',dfa, theta2, '-',dfa, theta3, '-',
        dfa, theta4, '-',dfa, theta5, '-',dfa, theta6, '-',dfa, theta7, '-',)
pl.plot(dfa, theta1, '-')
pl.show()
