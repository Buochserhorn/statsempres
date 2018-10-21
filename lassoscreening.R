
# choosing rnorm s.t. data is already mean-centered and with unit variance
y <- rnorm(n = 28)
x0 <- rnorm(n = 28)
x1 <- rnorm(n = 28)

r=0.8
x01 = r*y+sqrt(1-r*r)*x0
cor(x03,y)

r=0.7
x02 <- r*y+sqrt(1-r*r)*x0
cor(x03,y)

r=0.6
x03 <- r*y+sqrt(1-r*r)*x0
cor(x03,y)

x <- cbind(replicate(n = 10000,rnorm(n = 28)),x01,x02,x03)
y <- rnorm(n = 28)
grid=10^seq(from = 4, to = -3, length=100)  
lasso.mod = glmnet(x, y , alpha=1, lambda=grid, thresh=1e-12)
lasso.mod
head(sort(cor(x,y)))


n = 28
r = 0.05

x0 <- rnorm(n = 28)
x1 <- rnorm(n = 28)
y = rnorm(n)


r=0.8
x01 = r*y+sqrt(1-r*r)*x0
cor(x01,y)

r=0.7
x02 <- r*y+sqrt(1-r*r)*x0
cor(x02,y)

r=0.6
x03 <- r*y+sqrt(1-r*r)*x0
cor(x03,y)

r=0.05
x <- cbind(replicate(n = 10000,r*y+sqrt(1-r*r)*rnorm(n)),x01,x02,x03)
tail(sort(cor(x,y)))
grid=10^seq(from = 4, to = -3, length=100)  
lasso.mod = glmnet(x, y , alpha=1, lambda=grid, thresh=1e-12)
lasso.mod



library(MASS)
y <- rnorm(28)
# building covariance matrix
n <- 28
p <- 100
mat <- matrix(data= 0,nrow = p,ncol = p) # normal correlation is 0.05
for (i in 1:p) {
  mat[i,i] <- 1
}
corr <- c(2,3,4,5) # these are gonna correlate strongly with x1
for (i in corr) {
  mat[1,i] <- 0.8
  mat[i,1] <- 0.8
}
notcorr <- c(6:p)
for (i in notcorr) {
  mat[1,i] <- 0.05
  mat[i,1] <- 0.05
}
mat

x <- mvrnorm(n = n, rep(0,p), mat, empirical = TRUE)
cor(x)
x



library(MASS)
# building covariance matrix
n <- 25
p <- 100
mat <- matrix(data= 0,nrow = p,ncol = p) # normal correlation is 0.05
for (i in 1:p) {
  for (j in 1:p) {
    mat[i,j] <- 0.5^{abs(i-j)}
  }
}
X <- mvrnorm(n, rep(0,p), mat, empirical = TRUE)

# constructing ground truth

# number of nonzero values 
pbar <- c(100,1000,5000)
sigma <- 0.1
betastar <- vector(length = length(pbar))
y <- vector(length = length(pbar))
for (i in length(pbar)) {
  betastar[i] <- c(runif(pbar[i]),rep(0,p-pbar[i]))
  y[i] <- X%*%betastar[i]+sigma*rnorm(n)
}
