# Quantile Function for a Generalized Pareto Distribution.
qgp = function(p, mu, sigma, xi){
    sigma*((1-p)^-xi - 1)/xi + mu
}

# Example Usage
# qgp(p = 0.8, mu = 0, sigma = 1e4, xi = 0.7)

# Generate random deviates from a Generalized Pareto Distribution.
rgp = function(n, mu, sigma, xi){
    qgp(p = runif(n), mu, sigma, xi)
}

# Example Usage
# plot.ecdf(rgp(n = 10, mu = 0, sigma = 1e4, xi = 0.7))



