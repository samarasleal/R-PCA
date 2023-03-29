
# install.packages('acepack')
# install.packages('Hmisc')
library('Hmisc')

## STEP 1

# input file name
filename = 'InputEx.csv'

# read input file
data = read.table(filename, header = T)

## STEP 2

# X matrix
X = t(data)

# X number of rows and columns
m = nrow(X)
n = ncol(X)

## STEP 3

# U: means of X m variables
U = rowMeans(X)
U = t(U)
U = t(U)

## STEP 4

# H vector with n ones
H = rep(1, n)
H = t(H)

# B matrix
B  = X - (U %*% H)

## STEP 5

# covariance and correlation matrix
# Cov = (1 / (n - 1)) * B %*% t(B)
data.cov   = cov(data)
data.rcorr = rcorr(as.matrix(data))
data.cor   = data.rcorr$r

# p-values for the correlation matrix
data.cor.pvalues  = data.rcorr$P

## STEP 6, STEP 7 and STEP 8

data.cor.eigen = eigen(data.cor)
data.cov.eigen = eigen(data.cov)

E.cor = t(data.cor.eigen$values)
E.cov = t(data.cov.eigen$values)

V.cor = t(data.cor.eigen$vectors)
V.cov = t(data.cov.eigen$vectors)

Y.cor = V.cor %*% B
Y.cov = V.cov %*% B

## STEP 9

E.cor.sum = sum(E.cor)
E.cov.sum = sum(E.cov)

Prop.cor.partial = E.cor / E.cor.sum
Prop.cov.partial = E.cov / E.cov.sum

## STEP 10

# number of variables retained
nvar = m

# input for chi-square test
Prop.cor.total = sum(Prop.cor.partial[1:nvar]) / E.cor.sum
Prop.cov.total = sum(Prop.cov.partial[1:nvar]) / E.cov.sum

## PLOTS
# plot(t(X))
# plot(t(B))
# plot(Y.cov[1,], Y.cov[2,])

