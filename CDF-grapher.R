check_CDF = function(data, alpha_for_DKW, CDF_to_check=NULL, ...){
    
    estimated_parameters = list(...) # Unpack ellipses
    n = length(data)
    epsilon = sqrt(log(2/alpha_for_DKW)/(2*n)) # For simultaneous Dvoretzky–Kiefer–Wolfowitz bounds on the true CDF
    data_ecdf = ecdf(data)
    
    # This gets the simultaneous Dvoretzky–Kiefer–Wolfowitz lower bound at a given value of x.
    DKW_lb = function(x){
        data_ecdf(x) - epsilon
    }
    
    # This gets the simultaneous Dvoretzky–Kiefer–Wolfowitz upper bound at a given value of x.
    DKW_ub = function(x){
        data_ecdf(x) + epsilon
    }
    
    p = ggplot() +
        xlim(min(data),max(data)) + 
        ylim(0,1) +
        stat_function(
            aes(color = "DKW Band"),
            fun = DKW_lb,
            geom = "step"
        ) +
        stat_function(
            aes(color = "DKW Band"),
            fun = DKW_ub,
            geom = "step"
        ) +
        stat_function(
            aes(color = "ECDF"),
            fun = data_ecdf,
            geom = "step"
        ) 
    
        if(!is.null(CDF_to_check)){
            p = p + geom_function(
                aes(color = "Hypothesized CDF"),
                fun = CDF_to_check,
                args = estimated_parameters
            ) 
        }
    
    p = p +
        scale_color_manual(values = c("red", "black", "purple")) +
        labs(title = "Simultaneous 95% Confidence Bands for the True CDF",
             x = "x = Total Loss ($)",
             y = "P(X<=x)") +
        theme(legend.title = element_blank())
    
    return(p)
}
