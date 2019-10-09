# -*- coding: utf-8 -*-
"""
Created on Tue Oct 31 17:53:54 2017

@author: 胡琳那
"""

#2   Markov Chain 


import numpy as np

np.random.seed(1234)

states = 5
steps = 50
tolerance = 1e-5

P = np.random.rand(states, states)
p = np.random.rand(states)

P /= P.sum(axis=1)[:,np.newaxis]

p /= p.sum()

for k in range(steps):
    p = P.T.dot(p)

p_50 = p
print (p_50)

w, v = np.linalg.eig(P.T)

j_stationary = np.argmin(abs(w - 1.0))
p_stationary = v[:,j_stationary].real
p_stationary /= p_stationary.sum()
print (p_stationary)

if all(abs(p_50 - p_stationary) < tolerance):
    print ("infty-norm")

if np.linalg.norm(p_50 - p_stationary) < tolerance:
    print ("2-norm")
