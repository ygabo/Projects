import numpy as np
import matplotlib.pyplot as pl

def ridge( X, y, d2 ):

    A = np.dot(X.T, X)
    B = A + ( d2 * np.identity(8) )
    C = np.linalg.inv( B )
    D = np.dot( C, X.T )
    
    theta = np.dot(D, y)

    return theta


X   = np.loadtxt('prostate.data')
fig = pl.figure()
d2  = np.logspace(-5, 10, num=100)

y = X[:,-1]
X = X[:,0:-1]

y -= np.mean(y)
X -= np.mean(X, axis=0)
X /= np.std(X, axis=0)

U,S,VT = np.linalg.svd(X, full_matrices=False)

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

    theta = ridge(X, y, d2[i])
    df    = 0
    
    theta0 = np.append(theta0, theta[0])
    theta1 = np.append(theta1, theta[1])
    theta2 = np.append(theta2, theta[2])
    theta3 = np.append(theta3, theta[3])
    theta4 = np.append(theta4, theta[4])
    theta5 = np.append(theta5, theta[5])
    theta6 = np.append(theta6, theta[6])
    theta7 = np.append(theta7, theta[7])
    
    for j in xrange(8):
        df += S[j] / ( S[j] + d2[i])

    dfa = np.append(dfa,df)

pl.plot(dfa, theta0, '-', dfa, theta1, '-',dfa, theta2, '-',dfa, theta3, '-',
        dfa, theta4, '-',dfa, theta5, '-',dfa, theta6, '-',dfa, theta7, '-',)

pl.show()
