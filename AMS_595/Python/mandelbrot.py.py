# -*- coding: utf-8 -*-
"""
Created on Tue Oct 31 15:31:39 2017

@author: 胡琳那
"""
#1 Mandelbrot Set 

import numpy as np
import matplotlib.pyplot as plt

x,y=np.ogrid[-2:1:500j,-1.5:1.5:500j]

c = x + 1j*y
z = 0 

N_max = 50 
for j in range(N_max):
    z = z**2 + c 

some_threshold = 50
mask= (np.abs(z) < some_threshold)

plt.imshow(mask.T, extent=[-2,1,-1.5,1.5]) 
plt.gray() 
plt.savefig ('mandelbrot.png')