# This function computes the log-likelihood function for an IID sample. Named parameter values can be passed using the ellipses.
get_log_likelihood = function(PDF, data, ...){
    parameters = cross(list(...)) # This unpacks the parameters of the model that are passed using the ellipses. Then, this finds the Cartesian cross-product of these parameters so that we can return the log likelihood for any parameter combination.
    num_param_combos = length(parameters)
    
    log_likelihood = vector(mode = "double", length = num_param_combos) #This initializes a vector that will be filled later.
    for(i in 1:num_param_combos){
        parameters[[i]]$x = data
        log_likelihood[i] = sum(log(do.call(PDF, args = parameters[[i]]))) # The do.call() allows us to pass parameters as a list to our probability density function.
    }
    
    log_likelihood
}

# Example usage. Compare to https://en.wikipedia.org/wiki/Likelihood_function#/media/File:LikelihoodFunctionAfterHHT.png
# plot(x = seq(0,1, by = 0.01), 
#      y = exp(get_log_likelihood(PDF = dbinom, data = c(1,1,0), size = 1, prob = seq(0,1, by = 0.01))),
#      type = "l",
#      main = "Likelihood Function",
#      xlab = "Probability of Hs on 1 Flip of Unfair Coin (HHT Observed)",
#      ylab = "Likelihood")