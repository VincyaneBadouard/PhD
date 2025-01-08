N = 500 # observations nbr
x1 = rnorm(N) # var exp 1
x2 = rnorm(N) # var exp 2
X = cbind(1, x1, x2) # df with a exp var in each col
beta = c(-1,.2,-.3) # parameter of the X Var
gamma = c(2, -.25, .5) # ?
mu = plogis(X %*% beta) # mu (estimé) suit une logistic distribution
phi = exp(X %*% gamma)
A = mu*phi # parameter for beta distn
B = (1-mu)*phi # parameter for beta distn
y = rbeta(N, A, B) # response var
hist(y, 'FD')
