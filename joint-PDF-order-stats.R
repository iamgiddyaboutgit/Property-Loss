#' Joint PDF of order stats
#'
#' Evaluate the joint distribution of two order statistics from a given absolutely continuous distribution. The joint distribution is evaluated at x and y.
#'
#' @param PDF 
#' Probability Density Function
#' @param CDF 
#' Cumulative Distribution Function
#' @param x_j 
#' realized value of the smaller order statistic
#' x_j <= x_k
#' @param x_k 
#' realized value of the larger order statistic
#' @param n 
#' sample size
#' @param j
#' index of smaller order statistic
#' @param k
#' index of larger order statistic
#' @param ...
#' named parameters for the PDF and CDF
#' @return
#' numeric 
#'
#' @references 
#' https://en.wikipedia.org/wiki/Order_statistic#The_joint_distribution_of_the_order_statistics_of_an_absolutely_continuous_distribution

joint_PDF = function(PDF, CDF, x_j, x_k, n, j, k, ...){
    stopifnot(length(x_j) == length(x_k),
              sum(x_j <= x_k) == length(x_j),
              k >= j + 1) 
    parameters = list(...) # Unpack the arguments passed using the ellipses.
    
    parameters$x = x_j # Temporary fix because an argument named x is likely required for the PDF
    PDF_x_j = do.call(PDF, args = parameters[match.arg(names(parameters), choices = formalArgs(PDF), several.ok = TRUE)]) # The do.call allows us to pass a list of parameters to the CDF function. formalArgs finds the names of parameters required by the CDF function. match.arg then finds the names of all of the parameters passed to joint_PDF that are required by the CDF function. These names are then used as an index to the parameters using the [] for subsetting.
    parameters$q = x_j # Temporary fix because an argument named q is likely required for the PDF
    CDF_x_j = do.call(CDF, args = parameters[match.arg(names(parameters), choices = formalArgs(CDF), several.ok = TRUE)])
    
    parameters$x = x_k
    PDF_x_k = do.call(PDF, args = parameters[match.arg(names(parameters), choices = formalArgs(PDF), several.ok = TRUE)]) 
    parameters$q = x_k
    CDF_x_k = do.call(PDF, args = parameters[match.arg(names(parameters), choices = formalArgs(PDF), several.ok = TRUE)]) 
    
    factorial(n)/(factorial(j-1)*factorial(k-j-1)*factorial(n-k))*CDF_x_j^(j-1)*(CDF_x_k - CDF_x_j)^(k - 1 - j) * (1 - CDF_x_k)^(n - k) * PDF_x_j * PDF_x_k
}

joint_PDF(PDF = dnorm, CDF = pnorm, x_j = seq(-9,11,1), x_k = seq(-8,12,1), n = 3, j = 2, k = 3, mean = 1, sd = 10)


# Inequality
all_less_than_equals = function(a, b){
    sum(a <= b) == length(a)
}
x_j = c(1, 3, 9, 5)
x_k = c(2, 4, 10)
all_less_than_equals(x_j, x_k) 
