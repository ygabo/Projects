#!/usr/bin/env python
# a bar plot with errorbars
import numpy as np
import matplotlib.pyplot as plt

N = 7
time_1d = (0.516583, 0.265751,   0.132968,   0.072463,   0.041323,   0.026609,   0.030335 )
time_1d = np.array(time_1d)
time_1c = (0.516583, 0.434912,   0.225751,   0.114011,   0.062979,   0.045085,   0.037645 )
time_1c = np.array(time_1c)

time_1b = (0.516583, 5.472948,   5.291123,   5.247304,   5.393257,   6.513357,   5.837692 )
time_1b = np.array(time_1b)

ind = np.arange(N)  # the x locations for the groups
width = 0.35       # the width of the bars

fig = plt.figure()
ax = fig.add_subplot(111)
rects1 = ax.bar(ind, time_1c, width*2, color='g', align='center', yerr=0)


# add some
ax.set_ylabel('Time (s)')
ax.set_xlabel('Threads')
ax.set_title('Speed of count3s 1C (10M elements)')
ax.set_xticks(ind)
ax.set_xticklabels( ('Sequential', '2', '4', '8', '16', '32', '64') )

plt.show()
