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
    parameters = list(...)
    do.call(CDF, args = parameters)
}

joint_PDF(PDF = dnorm, CDF = qnorm, x_j = 1, x_k = 1, n = 3, j = 1, k = 1, x = 1:5, mean = 1, sd = 10, p = 0.8)


