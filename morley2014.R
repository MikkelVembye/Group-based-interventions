# Dataextraction from Morley (2014)
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

# Data from table 2 containing means and standard deviation on page 351

morley2014 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,4)),
  
  outcome = as.factor(rep(c("BSS",
                            "HADS_depression",
                            "HADS_anxiety",
                            "SAS"), each = 2,1
  )),
  
  n = rep(c(
    122, 63
  ), each = 1,4),
  
  m_pre = c(
    9.5, 12.56, # Beck Scale for Suicide Ideation (range 0–38)
    9.3, 9.89,  # HAD Scale Depression (range 0–21)
    12.58, 12.35, # HAD Scale Anxiety (range 0–21)
    61.76, 65.09  # Self-Efficacy Scale (range 0–90)
  ),
  
  sd_pre = c(
    9.61, 8.55,
    6.66, 5.6, 
    6.66, 5.39,
    15.34, 12.55
  ),
  
  m_post = c(
    5.83, 6,
    6.4, 7.1,
    8.83, 9.71,
    65.31, 65.13
  ),
  
  sd_post = c(
    5.58, 6.61,
    5.43, 4.3,
    5.72, 4.3,
    12.49, 9.58
  ),
  
  m_diff = m_post - m_pre
  
  ); morley2014
