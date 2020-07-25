
from numpy import array
from numpy import mean
from numpy import cov
from numpy.linalg import eig
# define a matrix
A = array([[2.5, 2.4],[0.5, 0.7],[2.2, 2.9],[1.9, 2.2],[3.1, 3.0],[2.3, 2.7],[2, 1.6],[1, 1.1],[1.5, 1.6],
           [1.9, 0.9]])
print(A)
print("-------------------")
# calculate the mean of each column
M = mean(A.T, axis=1)
print(M)
print("-------------------")
# center columns by subtracting column means
C = A - M
print(C)
print("-------------------")
# calculate covariance matrix of centered matrix
V = cov(C.T)
print(V)
print("-------------------")
# eigendecomposition of covariance matrix
values, vectors = eig(V)
print(vectors)
print("-------------------")
print(values)
print("-------------------")
# project data
P = vectors.T.dot(C.T)
print(P.T)

