import pandas as pd
import numpy as np
import math
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report,precision_score
from random import seed
seed(50)

def minkowski_distance(x, y, r):

    c = np.power((np.sum((np.power(abs(x - y), r)), 1)), (1 / r))
    return pd.DataFrame(c, columns=['distances'])
def Nearest_Neighbor(data, y, n, r, method='majority vote'):

    if 'majority vote' in method:
        mat = np.matrix(data.iloc[:, 0:4])
        d = minkowski_distance(mat, y, r)
        e = d.nsmallest(n, 'distances')
        dd = data[data.index.isin(e.index)]
        prediction = list(dd[4].mode())

        return prediction
    elif 'Average distance' in method:
        mat = np.matrix(data.iloc[:, 0:4])
        d = minkowski_distance(mat, y, r)
        e = d.nsmallest(n, 'distances')
        dd = data[data.index.isin(e.index)]
        f = pd.concat([dd, e], axis=1, join='inner')
        g = f.groupby(4, as_index=False)['distances'].mean()
        ans = g.nsmallest(1, 'distances')
        prediction = list(ans[4])
        return prediction
data_cols = ['Sepal Length', 'Sepal Width', 'Petal Length', 'Petal Width', 'Class', 'predicted']

data = pd.read_table('C:/Users/Immi/Desktop/Fall 2017/Data mining/HomeWork_3/iris.data',
                     sep=',', header=None)

# Part 1
x = np.matrix(data.iloc[:, 0:4])
y = np.matrix('6.4  2.7  5.3  1.9')

if __name__ == '__main__':

    def Fold_validation(iris, r, k, N, method='majority vote'):
        N = 5  # this is the no of folds
        n = math.ceil(len(iris) / N)
        ind = np.random.permutation(iris.index)
        a = []
        for j in range(N):
            a.append(ind[(j * n):(j * n + n)])

        b = pd.DataFrame()
        for k in a:
            b = b.append(iris.iloc[k])
            # print(k)
        c = np.array_split(b, N)
        n = 3

        predval = []
        original = []
        original_labs = []

        for i in c:
            test = []
            test = i.iloc[:, 0:4]
            test1 = i.iloc[:, 4]
            original_labs.append(test1)
            original.append(i)
            train = iris[~iris.index.isin(test.index)]
            # train1=iris[~iris.index.isin(test1.index)]
            train = train.iloc[:, 0:4]
            test = np.matrix(test)
            train = np.matrix(train)

            for tst in test:

                res = Nearest_Neighbor(data, tst, n, r, method='majority vote')
                predval.append(res)

        from itertools import chain
        predval = (list(chain.from_iterable(predval)))

        predval = pd.Series(predval)

        b['Predicted'] = predval.values
        b.columns = ['seplen', 'sepwid', 'petlen', 'petwid', 'class label', 'predicted']

        b = np.array_split(b, N)

        for i in range(len(b)):
            df = b[i]
            result_act = df['class label']
            result_pred = df['predicted']
            o = accuracy_score(result_act, result_pred)

            p = confusion_matrix(result_act, result_pred)

            print('Accuracy', o, '\n','confusion matrix\n', p)
    k = [3,5,7]
    r = [1,2,5]
    methods = ['majority vote', 'Average distance']
    for method in methods:
        for i in r:
            for j in k:
                jj = ('for', j,'Neighbors and',i,'order')
                print(jj,'\n',method,'\n', Fold_validation(data, i, j, 5, method= method))
               # print(jj,'\n',Fold_validation(iris, i, j, 5, method='Average distance'))
#####################################################################################################################
# Question 2
df= pd.read_csv('C:/Users/Immi/Desktop/Fall 2017/Data mining/HomeWork_3/abalone.data', delimiter=',', header=None)
#print(df.head(5))
df = df.drop(columns = [0], axis=1)
#changing rings to age
df['Age'] = df[8] + 1.5
df = df.drop(columns = [8], axis = 1)
filterp =lambda x: 1 if  x >= 9 else -1
df['Age'] = df['Age'].apply(filterp)

#Adding Augmented matrix
df.insert(0,column = "Aug_mat", value = np.ones(len(df)))
df2 = df.drop('Age',  1)

#creating test and train data
train_data = df.sample(frac = 0.8)
test_data = df[~df.index.isin(train_data.index)]
#Remove Age column from test data
test_preds = test_data["Age"]
test_data = test_data.iloc[:,0:8]
#print(test_data)
train_preds = train_data.iloc[:,-1]

train_data.loc[train_data.Age == -1,['Aug_mat', 1,2,3,4,5,6,7]]= train_data.loc[train_data.Age == -1,
                                    [ 'Aug_mat', 1, 2, 3, 4, 5, 6, 7]].values*-1


b = np.matrix(np.ones(len(train_data))).transpose()
a = np.matrix(np.ones(train_data.shape[1]-1))

eta = 0.1
theta = 1e-3
Data = train_data.iloc[:,0:8]

def Widrow_Hoff(Data, a,b, eta, theta):
    data = np.matrix(Data)
    n = len(data)
    k, k_max = 0, 10**4
    limit = 0
    while limit == 0 and k < k_max:
        k +=1
        for i in range(n):
            #print(data[i], a)
            x = data[i]*a.T
            A_delta = (eta/k)*((b[i]-(x))*data[i])

            if np.linalg.norm(A_delta) > theta:
                a = A_delta + a
            else:
                limit = 1
                break
        return (k, a,i)
W_H = Widrow_Hoff(Data,a,b,eta,theta)
print("Value of Learning rate:", eta)
print("Choice of vector b:", b)
print("Number of iterations:",W_H[0],'\nThe gradient function values:', W_H[1])
print("Number of Updates:", W_H[2])

test_data = np.matrix(test_data)

#test_preds =test_preds.reset_index()
prediction =[]
#print(test_data)
learning_rates = [0.1,0.3,0.5]

for j in range(len(test_data)):


    if test_data[j] * (W_H[1].T) > 0:
        prediction.append(1)

    else:
        prediction.append(-1)

target_names = ['class 0', 'class 1']
print(classification_report(test_preds, prediction, target_names=target_names))
#print(test_preds)
print('Accuracy is:',accuracy_score(test_preds,prediction))
kk = confusion_matrix(test_preds, prediction)
print(precision_score(test_preds,prediction))
confusion = kk
#true positive, #true negatives,#false positve,#false negative
TP, TN, FP, FN = confusion[1][1],confusion[0][0],confusion[0][1],confusion[1][0]


print('The Sensitivity is')
print(TP / float(TP + FN))
print('The Specificity is')
print(TN / float(TN + FP))

