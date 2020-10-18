f = function(x,y){
    x*y*(0.75*(x^2+y^2)+0.5)
}


V = 1 # change as necessary
m = 2 # f is m-dimensional
N = 1e6 # Number of samples
X = matrix(nrow = m, ncol = N)

for(j in 1:N){
    X[, j] = runif(m)
}

V*(1/N)*sum(f(x = X[1,], y = X[2,]))

