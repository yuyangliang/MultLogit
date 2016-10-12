# -*- coding: utf-8 -*-
"""
Created on Tue Oct 11 08:04:24 2016

@author: Yuyang
"""

import csv
import numpy

#read the parameters
paras = []
with open('paras.csv', 'rb') as mycsvfile:
    data = csv.reader(mycsvfile, csv.QUOTE_NONNUMERIC)
    next(data, None) 
    for row in data:
        paras = map(float, row)
        
#read the test set
test = []
with open('test.csv', 'rb') as mycsvfile:
    data = csv.reader(mycsvfile)
    next(data, None) 
    for row in data:
        test.append(map(float,row))

#simulations
sim = []
for j in range(0, 10):
    PR = []
    for i in range(8, 14):        
        PR.append(math.exp(numpy.dot(test[j], paras[0:8]) + paras[i] + numpy.random.normal(0, math.sqrt(paras[14])) + numpy.random.normal(0, math.sqrt(paras[15]))))
    
    Probs = []
    Probs.append(1 / (sum(PR) + 1))
    for k in range(0, 6):
        Probs.append(PR[k] / (sum(PR) + 1))
    
    sim.append(Probs)