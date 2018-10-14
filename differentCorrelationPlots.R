n = 100

par(mfrow=c(1,3))
r = 0.1
x1 = rnorm(n)
x2 = rnorm(n)
y1 = r*x2+sqrt(1-r*r)*x1
plot(y1,x2, main = "rho = 0.1", xlab = "x_j", ylab = "y")
r = 0.5
x1 = rnorm(n)
x2 = rnorm(n)
y1 = r*x2+sqrt(1-r*r)*x1
plot(y1,x2, main = "rho = 0.5", xlab = "x_j", ylab = "y")
r = 0.9
x1 = rnorm(n)
x2 = rnorm(n)
y1 = r*x2+sqrt(1-r*r)*x1
plot(y1,x2, main = "rho = 0.9", xlab = "x_j", ylab = "y")
