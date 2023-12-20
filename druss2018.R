# Dataextraction from Druss (2018)
# Attempt from Jakob

# Loading the relevant package herein Tidyverse 
library(tidyverse)

# Setting the relevant options -------------------
# The code benith sets global options in R for various packages and functions
options(pillar.sigfig = 4) # ensure tibble include 4 digits

options(tibble.width = Inf) # This sets the width of output from the tibble 
# package to infinity, meaning that the output will 
# not be truncated or wrapped.

options(dplyr.print_min = 310) # This sets the minimum number of rows that will 
# be printed when using the dplyr package to 310

options(scipen = 10) # This sets the scientific notation precision in R to 10, 
# meaning that numbers will be displayed with 10 digits of precision

# Data from table 3 containing means and standard deviation fore RAS

druss2018 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,1)),
  
  outcome = as.factor(c("RAS")),
  
  N = c(198, 202),
  
  m_pre = c(
    3.72, 3.66
  ),
  
  sd_pre = c(
    0.63, 0.57 
  ),
  
  m_3m = c(
    3.89, 3.70
  ),
  
  sd_3m = c(
    0.55, 0.68
  ),
  
  m_6m = c(
    3.87, 3.75
  ),
  
  sd_6m = c(
    0.61, 0.59
  )
)
