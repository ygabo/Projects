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

fig = pl.figure()
pl.set_cmap(pl.cm.Greys_r)
X = load_patches(images)

U,S,VT = np.linalg.svd(X, full_matrices=False)

sorted_S = np.argsort(S)
sorted_S_desc = sorted_S[::-1]

for i in xrange(25):
    plot_num = 211+i
    t = fig.add_subplot(5,5,i)
    t.imshow( VT[sorted_S_desc[i]].reshape(20,20) )
    t.xaxis.set_visible(False)
    t.yaxis.set_visible(False)
    
pl.show()

