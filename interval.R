interval <- function(x, confidence){
  
  # Murillo-C David | release: Aug 2021 | version: 1.0
  
  # Description
    # This function helps to calculate the user-desired confidence interval 
    # of the mean from the population, based on a vector of data. 
    # The estimation uses the Student probability model (or T model) which 
    # takes into account the freedom degrees.  
  
  # Arguments
    # x          = Numeric type vector which contains the results of the 
    #              haracters studied in the sample from which is desired to 
    #              obtain the confidence interval 
    #
    # confidence = A number indicating the proportion of confidence that the 
    #              interval should represent. It must be higher than 0 and 
    #              smaller than 1 (e.g., 0.1 or 0.95, where it represents 10% 
    #              and 95% of confidence, respectively) 
  
  # Code
    if(confidence >= 1 | confidence <= 0) stop(   
      'confidence must be a number greater than 0 and smaller than 1' # The code stops if a wrong confidence value was used 
    )
    n <- length(x)                                                    # Obtains the size of the sample
    eem <- sd(x)/sqrt(n)                                              # Calculates the standard error of the mean of the sample
    alpha <- 1 - confidence                                           
    t <- 1 - alpha/2                                                  
    tv <- round(qt(p = t, df = n - 1), 3)                             # Obtains the cuantile value of the Student model  
    intervals <- c(L1 = mean(x) - tv * eem, L2 = mean(x) + tv * eem)  # Calculates and saves both limits 
    intervals <- round(intervals, 2)
    intervals                                               
    
  # Value
    # Two numeric values labeled as L1 and L2, 
    # L1 represents the inferior limit and L2 the superior limit of the 
    # confidence interval
    
  # Example of use
    # it is registered the shell thickness in mm from 15 random snails from a 
    # quadrant, as showed below 
    
      # mm <- c(32.5, 43.6, 21.7, 25.1, 33.7, 26.5, 37.7, 27.0, 31.4, 29.9, 46.3,
      #         24.4, 16.1, 40.9, 23.6)
    
    # The investigator wants to know the real mean value of shell thickness 
    # present in the population of snails with a confidence interval of 90%
      # interval(x = mm, confidence = 0.9)
}
