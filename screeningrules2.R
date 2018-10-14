
# From the book: "The first variable to enter the model has largest absolute inner-product
# lambda_max=max_j |x_j^T*y|, which also defines the entry value for lambda."
# I tried to verify this with the Hitters dataset, but failed.

library(glmnet)
library(ISLR)
data(Hitters)
Hitters=na.omit(Hitters)

# define x and y, as we cannot use y~x notation in glmnet
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# grid for lambda values # refining to find lambda_max
grid=10^seq(from = 4, to = -3, length=100)  
grid=10^seq(from = 0, to = -1, length=100)  
grid=seq(from = 0.57, to = 0.55, length=100)  

#mean centering
x <- apply(x, 2, function(k) (k-mean(k))/sd(k))
y <- (y-mean(y))/sd(y) # not necessary to get correct results



# double checking
apply(x,2,sum)
sum(y)

# fitting lasso 
lasso.mod = glmnet(x, y, alpha=1, lambda=grid, thresh=1e-12)
lasso.mod # smallest lambda with all zero coef is at lambda = 2.719e+02 (i.e. this should be lambda_max)
# plot(lasso.mod)

# calculating the inner product |y^tx| for each column
absinprod <- (y%*%x)/length(y) 
max(absinprod) # according to the book it should be lambda_max=max_j |x_j^T*y|


# ----------------------------------------

# stuff by Solt
# whats comented has been delt with

# absinprod <- apply(x,2, function(k) abs(sum(y*k))/length(y))
# max(absinprod) # these numbers are waaay bigger than the lambda_max
# you need to divide by the number of observations! (In the book you can also check footnote number 7 on page 127)

# side remark: you can use %*% for matrix multiplication in R and then you do not need to use apply(...)  

# This already helps to get closer. Then one more point 
# is the internal standardisation in glmnet. If you use 
# x <- apply(x, 2, function(k) (k-mean(k))/sd(k))
# y <- (y-mean(y))/sd(y)
# then it surely matches. (you might not have that exact value in your defined 
# grid what you get from the inner products, but just try a new glmnet fit with the 
# found lambda_max up to some rounding of like 2-3 digits... You will see that is 
# the  in or out point for the first variable) 

# JO: Honestly not convinced, numbers are just small now

# I think it is sufficient to scale only the X to have unit variance in each column. 
# JO: Nope,it has to have mean 0, but y does not need to be scaled
# You can try it out what exactly needs to be scaled.
# Side remark: Instead of 

# apply(x, 2, function(k) (k-mean(k))/sd(k))
# apply(x,2,sum)

# you can use colMeans(x), but watch out: 
# x=x-colMeans(x) does not give you mean 0 in the columns. 
# The problem is the order how which the vector  with means is applied to the matrix x.  
# x=t(t(x)-colMeans(x)) would work, but potentially 
# there are also more elegant ways to do it.




# ------------------------------------------------------------------------------------------------

# trying the next equality

# -----------------------------------------------------------------------------------------------

# trying to check |x_j^T*(y-y_lambda^hat)|=lambda for j in the active set and < for j out
grid=10^seq(from = 4, to = -3, length=100)  
lasso.mod = glmnet(x, y, alpha=1, lambda=grid, thresh=1e-12)
lasso.mod # to choose a lambda
(lam <- lasso.mod$lambda[79])
lasso.pred=predict(lasso.mod,s=lam,newx=x) # for y.hat
lasso.coef=predict(lasso.mod,s=lam,type = "coefficients",newx=x) # to find active set
y.hat <- lasso.pred[,1]

# calculating inner product 
absinprod2 <- apply(x,2, function(k) abs(sum((y-y.hat)*k))/length(y))
absinprod2 # e.g. Hits is in the active set and has a value of 8.0163630, for lambda = 0.03

lasso.coef # to see which predictors are in the active set
