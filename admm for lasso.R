library(glmnet)
library(ADMM)
library(ISLR)
data(Hitters)
Hitters=na.omit(Hitters)

# define x and y, as we cannot use y~x notation in glmnet
X=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# grid for lambda values
grid=10^seq(from = 3, to = -4, length=100)  

# fitting lasso 

start.time <- Sys.time()
lasso.mod = glmnet(X, y, alpha=1, lambda=grid, thresh=1e-12)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
for (i in grid) {
  admm <- admm.lasso(X,y,lambda = i)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


library(glmnet)
library(ADMM)
library(biglasso)
data(colon)

# define x and y, as we cannot use y~x notation in glmnet
X=colon$X
y=colon$y
dim(x)
# grid for lambda values
grid=10^seq(from = 3, to = -4, length=100)  

# fitting lasso 

start.time <- Sys.time()
lasso.mod = glmnet(X, y, alpha=1, lambda=1, thresh=1e-12)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
for (i in grid) {
  admm <- admm.lasso(X,y,lambda = i)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
