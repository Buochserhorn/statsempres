
# From the book: "The first variable to enter the model has largest absolute inner-product
# lambda_max=max_j |x_j^T*y|, which also defines the entry value for lambda."

library(glmnet)
library(ISLR)
data(Hitters)
Hitters=na.omit(Hitters)

# define x and y, as we cannot use y~x notation in glmnet
x=model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary

# grid for lambda values # refining to find lambda_max
# grid=10^seq(from = 4, to = -3, length=100)  
# grid=10^seq(from = 0, to = -1, length=100)  
grid=seq(from = 0.57, to = 0.55, length = 100)  
grid=seq(from = 0.5657, to = 0.5659, length = 100)  
grid=seq(from = 0.5658859, to = 0.5658879, length = 100)  


#standardisation
x <- apply(x, 2, function(k) (k-mean(k))/sqrt(sum((k-mean(k))^2)/length(k)))
y <- (y-mean(y))/sd(y) # not necessary to get correct results

# fitting lasso 
lasso.mod = glmnet(x, y, alpha=1, lambda=grid, thresh=1e-12)
lasso.mod # smallest lambda with all zero coef is at lambda = 0.5660 (i.e. this should be lambda_max)
lasso.df <- as.data.frame(do.call(cbind, lasso.mod[c("df","lambda")]))
#lasso.df[20:28,]
lasso.df[5:10,]
lasso.df[54:59,]


# calculating the inner product |y^tx_j| for each column x_j
absinprod <- (y%*%x)/length(y) 
max(absinprod) # according to the book it should be lambda_max=max_j |x_j^T*y|



# ------------------------------------------------------------------------------------------------

# trying the next inequality

# -----------------------------------------------------------------------------------------------

# trying to check |x_j^T*(y-y_lambda^hat)|=lambda for j in the active set and < for j out
grid=10^seq(from = 4, to = -3, length=100)  
lasso.mod = glmnet(x, y, alpha=1, lambda=grid, thresh=1e-12)
lasso.mod # to choose a lambda
(lam <- lasso.mod$lambda[84]) # this is the chosen lambda
lasso.pred=predict(lasso.mod,s=lam,newx=x) # predict to get y.hat
y.hat <- lasso.pred[,1]
lasso.coef=predict(lasso.mod,s=lam,type = "coefficients",newx=x) # to find active set

# calculating inner product 
absinprod2 <- apply(x,2, function(k) abs(sum((y-y.hat)*k))/length(y))

# lasso.coef - to see which predictors are in the active set
# absinprod2 - absolute inner product
# lam - chosen lambda

cbind(lasso.coef,round(c(0,absinprod2),4),round(lam,4))
