# This function returns the finite mixture PDF for a vector of weights and a set of corresponding PDFs.
# x: a numeric vector at which to evaluate the mixture PDF.
# PDFs: a nested list in which the first level contains the names of the component PDFs and in which the second level is a list containing the weights (named w) and named parameters for each PDF. 

get_mixture_PDF = function(x, PDFs){
    require(purrr)
    
    k = length(PDFs)
    if(sum(unlist(sapply(PDFs, names)) %in% "w") != k){
        stop("Weights (w) must be supplied for each PDF.")
    }
    if(sum(sapply(PDFs, "[[", "w")) != 1){
        stop("Weights (w) must sum to 1.")
    }

    mixture_PDF = matrix(nrow = length(x), ncol = k)
    for(j in 1:k){
        PDFs[[j]]$x = x
        mixture_PDF[, j] = PDFs[[j]]$w * do.call(names(PDFs)[j], args = flatten(PDFs[j])[match.arg(names(flatten(PDFs[j])), choices = formalArgs(names(PDFs)[j]), several.ok = TRUE)])
    }
    
    rowSums(mixture_PDF)
}

####################
# Example Usage
####################
# components = list(
#     dnorm = list(w = 1/3, mean = 1, sd = 0.2), 
#     dnorm = list(w = 1/3, mean = 2.5, sd = 1),
#     dnorm = list(w = 1/3, mean = 4, sd = 0.2)
# )
# 
# x = seq(-1,6,0.1)
# plot(x = x,
#      y = get_mixture_PDF(x = x, PDFs = components),
#      type = "l"
# )
# 
# # Should integrate to about 1
# integrate(get_mixture_PDF, lower = -2, upper = 7, PDFs = components)


