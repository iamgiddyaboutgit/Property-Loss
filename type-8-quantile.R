# Type 8 Quantile by Hand
Q = function(x, probs = seq(0, 1, 0.25)){
    m  = (probs + 1)/3
    n = length(x)
    j = floor(n*probs + m) # This is in the same order as probs.
    gamma_constant = n*probs + m - j # This is in the same order as probs.
    x = sort(x, decreasing = FALSE)
    
    quantiles = vector(mode = "double", length = length(probs))
    # Look where j is too low or too high and adjust the quantiles correspondingly.
    quantiles[j == 0] = min(x)
    quantiles[j == n] = max(x)
    
    # Use formula from Hyndman and Fan (1996) but only where j is not too low or too high.
    k = j[j != 0 & j != n]
    gamma_constant = gamma_constant[j != 0 & j != n]
    quantiles[j != 0 & j != n] = (1 - gamma_constant)*x[k] + gamma_constant * x[k + 1]

    names(quantiles) = paste0(probs*100, "%")
    
    return(quantiles)
}

# Example usage and comparison to base quantile function.
# x = c(1,3,9,4,2,0.7)
# probs = c(0,1,0.2,0.8)
# Q(x, probs)
# quantile(x, probs, type = 8)
