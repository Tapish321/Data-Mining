import  pandas as pd
import numpy as np
import math
from sklearn.preprocessing import LabelEncoder
from neupy import algorithms
df= pd.read_csv('C:/Users/Immi/Desktop/Fall 2017/Data mining/HomeWork_3/abalone.data', delimiter=',', header=None)

df['Age'] = df[8] + 1.5

labelencoder=LabelEncoder()
df[0] = labelencoder.fit_transform(df[0])
filterp =lambda x: 0 if  x > 0 and x <= 9 else 1
df['Age'] = df['Age'].apply(filterp)

