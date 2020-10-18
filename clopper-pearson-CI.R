# This function finds an exact, two-sided confidence interval for the probability of success in a binomial experiment. 
# alpha = significance level
# x = number of successes observed
# n = number of trials
# reference: https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Clopper%E2%80%93Pearson_interval

get_clopper_pearson = function(x, n, alpha){
    CI = c(qbeta(p = alpha/2, shape1 = x, shape2 = n - x + 1),
           qbeta(p = 1 - alpha/2, shape1 = x + 1, shape2 = n - x))
    names(CI) = c(paste0((alpha/2)*100, "%"),
                  paste0((1 - alpha/2)*100, "%"))
    CI
}


