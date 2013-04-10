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

#SVD
U,S,VT = np.linalg.svd(X, full_matrices=False)
X_rot  = U * S
lambda_= np.var( X_rot, axis=0)
X_wit  = X_rot / np.sqrt(lambda_)
X_zca  = np.dot(X_wit, VT)

#plot first patch
t = fig.add_subplot(1,2,1)
t.imshow( X[1].reshape(20,20) )
t.set_title('patch X[1]')
t.xaxis.set_visible(False)
t.yaxis.set_visible(False)

#plot whitened version
t = fig.add_subplot(1,2,2)
t.imshow( X_zca[1].reshape(20,20) )
t.set_title('whitened patch X[1]')
t.xaxis.set_visible(False)
t.yaxis.set_visible(False)

pl.show()

