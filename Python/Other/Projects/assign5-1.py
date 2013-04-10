import numpy as np
import matplotlib.pyplot as pl

X   = np.loadtxt('prostate.data')
fig = pl.figure()
     
for i in xrange(9):
    for j in xrange(9):
        if i != j:
            t = fig.add_subplot(9,9,9*j+i+1)
            t.scatter(X.T[j], X.T[i])
            t.xaxis.set_visible(False)
            t.yaxis.set_visible(False)
pl.show()
