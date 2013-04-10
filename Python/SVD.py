import numpy as np
import matplotlib.pyplot as pl

def load_patches(images, n=10000, p=20):
    rand = np.random.RandomState(seed=0)
    X = np.empty((n, p**2))
    m, d, _ = images.shape
    for i in xrange(n):
        x, y = rand.randint(d-p, size=2)
        j = rand.randint(m)
        X[i,:] = images[j, y:y+p, x:x+p].flat
    return X

images = np.load('images.npy')
fig    = pl.figure()
X      = load_patches(images)

pl.set_cmap(pl.cm.Greys_r)
fig.subplots_adjust(hspace=0.4)

#SVD
U,S,VT = np.linalg.svd(X, full_matrices=False)

Xrot   = np.dot(U, S)
lambda_= np.var( Xrot, axis=0)
D      = 1 / np.sqrt(lambda_)
W_T    = np.dot( np.dot(VT.T,D), VT)
W      = W_T.T

for i in xrange(25):
    t = fig.add_subplot( 5,5, i+1 )
    t.imshow( W[i].reshape(20,20) )
    t.set_title(i+1)
    t.xaxis.set_visible(False)
    t.yaxis.set_visible(False)

pl.show()

