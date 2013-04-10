from numpy import *

def HMM(pX0, theta, phi, y, T):

    #setup
    # S H
    #pX0 = ( [0.4, 0.6 ] )
    #
    # S H
    #theta = ( [ [0.8,0.2], S
    # [0.1,0.9] ] ) H
    #
    # CO CR SL SO W
    #phi = ( [ [ 0.1, 0.2, 0.4, 0, 0.3 ], S
    # [ 0.3, 0 , 0.3, 0.3, 0.1 ] ] ) H
    px = ([0.0,0.0])
    
    for t in xrange(T):
        if t < len(y)+1:
            # first node
            if t == 0:            
                px[0] = pX0[0]
                px[1] = pX0[1]
            else:
            #get X_t|Y_1:t-1
                a = ( theta[0][0] * px[0] ) + ( theta[1][0] * px[1] )
                b = 1 - a
                print ' '
                print t
                print str(a) + ' a'
                print str(b) + ' b'
            #get X_t|Y_1:t
                c = phi[0][y[t-1]] * a
                d = phi[1][y[t-1]] * b
                px[0] = c / (c+d)
                px[1] = d / (c+d)
                       
            print str(px[0]) + ' px0'
            print str(px[1]) + ' px1'
        else:
            a = ( theta[0][0] * px[0] ) + ( theta[1][0] * px[1] )
            b = 1 - a
            #get X_t|Y_1:t
            px[0] = a / (a+b)
            px[1] = b / (a+b)
            print ' '
            print t
            print str(px[0]) + ' px0'
            print str(px[1]) + ' px1'
    return px


pX0 = ( [0.4, 0.6 ] )
theta = ( [ [0.8,0.2], 
          [0.1,0.9] ] ) 
phi = ( [ [ 0.1, 0.2, 0.4, 0, 0.3 ], 
        [ 0.3, 0 , 0.3, 0.3, 0.1 ] ] ) 
y = ([3,3,0,4,2])
T = 9

HMM(pX0, theta, phi, y, T)

