library(ggplot2)
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
#' the ellipses are used to pass named parameters for the PDF and CDF
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
    
    # scaling_factor = factorial(n)/(factorial(j-1)*factorial(k-j-1)*factorial(n-k)) # this doesn't work for large n
    
    kernel = CDF_x_j^(j-1)*(CDF_x_k - CDF_x_j)^(k - 1 - j) * (1 - CDF_x_k)^(n - k) * PDF_x_j * PDF_x_k
    
    kernel
}

n = 300
x_j = seq(-5,5,0.01)
#x_j = rep(x_j, 100)
x_k = x_j + 0.001
j = 20
k = 21
z = joint_PDF(PDF = dt, CDF = pt, x_j = x_j, x_k = x_k, n = 300, j = 20, k = 21, df = 2)

data = data.frame(x_j,x_k,z)

# Note that this bivariate PDF looks very narrow because the order statistics are highly correlated. At first I thought this was a mistake in generating the x and y-coordinates, but I now believe that this is happening just because of the high correlation.
ggplot(data = data,
       aes(x = x_j,
           y = x_k,
           z = z)) +
    geom_density_2d()

plot_ly(data = data,
        x = ~x_j,
        y = ~x_k,
        z = ~log(z),
        type = "contour")  

sim1 = 1e+5
estimates = matrix(nrow = sim1, ncol = 2, dimnames = list(NULL, c("x_j", "x_k")))
for(i in 1:sim1){
    rand_data = sort(rt(n = n, df = 2), decreasing = FALSE)
    estimates[i, "x_j"] = rand_data[j]
    estimates[i, "x_k"] = rand_data[k]
}
colMeans(estimates)

