#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 24 15:44:17 2017

@author: Elent
"""

import numpy as np
import matplotlib.pyplot as plt
import gbrbm
import pandas as pd

## forming the dataset

# importing data of snp and 19 stocks
s0 = pd.read_csv('data/snpFeatures.csv').iloc[:,range(1,21)].values
s1 = pd.read_csv('data/acnFeatures.csv').iloc[:,range(5,21)].values
s2 = pd.read_csv('data/adpFeatures.csv').iloc[:,range(5,21)].values
s3 = pd.read_csv('data/akamFeatures.csv').iloc[:,range(5,21)].values
s4 = pd.read_csv('data/amatFeatures.csv').iloc[:,range(5,21)].values
s5 = pd.read_csv('data/crmFeatures.csv').iloc[:,range(5,21)].values
s6 = pd.read_csv('data/cscoFeatures.csv').iloc[:,range(5,21)].values
s7 = pd.read_csv('data/ctshFeatures.csv').iloc[:,range(5,21)].values
s8 = pd.read_csv('data/ctxsFeatures.csv').iloc[:,range(5,21)].values
s9 = pd.read_csv('data/ebayFeatures.csv').iloc[:,range(5,21)].values
s10 = pd.read_csv('data/fisFeatures.csv').iloc[:,range(5,21)].values
s11 = pd.read_csv('data/googlFeatures.csv').iloc[:,range(5,21)].values
s12 = pd.read_csv('data/hpFeatures.csv').iloc[:,range(5,21)].values
s13 = pd.read_csv('data/intuFeatures.csv').iloc[:,range(5,21)].values
s14 = pd.read_csv('data/MAFeatures.csv').iloc[:,range(5,21)].values
s15 = pd.read_csv('data/nflxFeatures.csv').iloc[:,range(5,21)].values
s16 = pd.read_csv('data/ntapFeatures.csv').iloc[:,range(5,21)].values
s17 = pd.read_csv('data/TFeatures.csv').iloc[:,range(5,21)].values
s18 = pd.read_csv('data/VZFeatures.csv').iloc[:,range(5,21)].values
s19 = pd.read_csv('data/wuFeatures.csv').iloc[:,range(5,21)].values

# combining and scaling the data
DATA = np.column_stack((s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19))

for i in range(0,DATA.shape[1]):
    std = np.std(DATA[:,i])
    mean = np.mean(DATA[:,i])
    DATA[:,i] = (DATA[:,i] - mean)/std

# reforming the data    
price = DATA[:,0]
price2 = np.delete(price,0,axis=0)
data2 = np.delete(DATA,-1,axis=0)
DATA_new = np.column_stack((price2,data2))
    
    
# splitting the dataset into the training set and test set
from sklearn.cross_validation import train_test_split
DATA_training, DATA_test = train_test_split(DATA_new, test_size = 0.25, random_state = 0)

# splitting the training dataset into the validation set and SVM training set
DATA_SVM, DATA_validation = train_test_split(DATA_training, test_size = 0.04, random_state = 0)

#################################################################################################
#################################################################################################

## Calculating SVM+1

data_SVM = DATA_SVM[:,range(0,21)]
data_test = DATA_test[:,range(0,21)]

# regression
from sklearn.svm import SVR
y = data_SVM[:,0]
x = np.delete(data_SVM,0,axis=1)
clf = SVR(C=1.0, epsilon=0.2)
clf.fit(x, y) 

# prediction
yt = data_test[:,0]
xt = np.delete(data_test,0,axis=1)

yp = clf.predict(xt)

# preformance measurement
NMSE = sum((yt-yp)**2)/sum((yt-np.mean(yt))**2)

yt_DA = np.delete(yt,0) - np.delete(yt,-1)
yp_DA = np.delete(yp,0) - np.delete(yt,-1)
y_DA = yt_DA * yp_DA
DA = sum(y_DA>0)/len(y_DA)

print('SVM+1: '+'NMSE: '+str(NMSE)+' DA: '+str(DA*100)+'%')


######################################################################
######################################################################

## Calculating SVM+20

# regression
from sklearn.svm import SVR
Y = DATA_SVM[:,0]
X = np.delete(DATA_SVM,0,axis=1)
CLF = SVR(C=1.0, epsilon=0.2)
CLF.fit(X, Y) 

# prediction
Yt = DATA_test[:,0]
Xt = np.delete(DATA_test,0,axis=1)

Yp = CLF.predict(Xt)

# preformance measurement
NMSE = sum((Yt-Yp)**2)/sum((Yt-np.mean(Yt))**2)

Yt_DA = np.delete(Yt,0) - np.delete(Yt,-1)
Yp_DA = np.delete(Yp,0) - np.delete(Yt,-1)
Y_DA = Yt_DA * Yp_DA
DA = sum(Y_DA>0)/len(Y_DA)

print('SVM+20: '+'NMSE: '+str(NMSE)+' DA: '+str(DA*100)+'%')


######################################################################
######################################################################

## Calculating SVM+DBN

# training DBN
data_training = np.delete(DATA_training,range(0,5),axis=1)
gbrbm = gbrbm.GBRBM(n_visible=data_training.shape[1], n_hidden=2000, learning_rate=0.01, momentum=0.95)
errs = gbrbm.fit(data_training, n_epoches=30, batch_size=90)
plt.plot(errs)
plt.show()

# regression
data_training_new = gbrbm.reconstruct(data_training)
data_training_dbn = np.column_stack((DATA_training[:,range(0,5)],data_training_new))
y = data_training_dbn[:,0]
x = np.delete(data_training_dbn,0,axis=1)
clf_dbn = SVR(C=1.0, epsilon=0.2)
clf_dbn.fit(x,y)

# prediction
data_test = np.delete(DATA_test,range(0,5),axis=1)
data_test_new = gbrbm.reconstruct(data_test)
data_test_dbn = np.column_stack((DATA_test[:,range(0,5)],data_test_new))

yt_dbn = data_test_dbn[:,0]
xt_dbn = np.delete(data_test_dbn,0,axis=1)

yp_dbn = clf_dbn.predict(xt_dbn)


# preformance measurement
NMSE = sum((yt_dbn-yp_dbn)**2)/sum((yt_dbn-np.mean(yt_dbn))**2)

Yt_DA = np.delete(yt_dbn,0) - np.delete(yt_dbn,-1)
Yp_DA = np.delete(yp_dbn,0) - np.delete(yt_dbn,-1)
Y_DA = Yt_DA * Yp_DA
DA = sum(Y_DA>0)/len(Y_DA)

print('SVM+DBN: '+'NMSE: '+str(NMSE)+' DA: '+str(DA*100)+'%')






















