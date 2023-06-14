# Data typing from Smith et al. (2021) and Wuthrich & Rapee (2013) 
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

smith2021_wuthrich2013 <- tibble(
  outcome = rep(c("Primary problem",
                  "Loneliness", 
                  "Anxiety", 
                  "Depression", 
                  "GDS", 
                  "GAI", 
                  "PSWQ",
                  "SF12"), 
                each = 2),
  
  group = rep(c("cbt", 
                "waitlist"), 
              each = 1,8),
  
  N_start =  rep(c(27, 35), n_distinct(outcome)), # From Table 2, 2013, p. 783,
  
  N = rep(c(27, 35), n_distinct(outcome)), # From Table 2, 2013, p. 783,
  
  emm_pre = c(
    6.33, 5.81, 
    1.59, 1.36, # Loneliness table 3, 2021
    5.20, 4.58, # Anxiety table 3, 2021
    5.48, 4.98,  # Depression table 3, 2021
    18.14, 16.35, # GDS table 3, 2013
    11.59, 16.35, # GAI table 3, 2013
    53.62, 49.27, # PSWQ table 3, 2013
    37.09, 38.45  # SF12 table 3, 2013
  ),
  
  emm_post = c(
    3.46, 5.15, # Primary problem table 3, 2013
    0.593, 1.24, # Loneliness table 3, 2021
    2.68, 4.01,  # Anxiety table 3, 2021
    2.02, 4.25,   # Depression table 3, 2021
    9.21, 14.38, # GDS table 3, 2013
    5.84, 8.33, # GAI table 3, 2013
    46.54, 47.24, # PSWQ table 3, 2013
    47.79, 43.56  # SF12 table 3, 2013
  ),
  
  se_pre = c(
    0.25, 0.21, # Primary problem table 3, 2013
    0.209, 0.169, # Loneliness table 3, 2021
    0.246, 0.200, # Anxiety table 3, 2021 
    0.272, 0.217,  # Depression table 3, 2021
    1.28, 1.10, # GDS table 3, 2013
    1.10, 0.95, # GAI table 3, 2013
    2.52, 2.20, # PSWQ table 3, 2013
    2.01, 1.73  # SF12 table 3, 2013
    
  ), 
  se_post = c(
    0.29, 0.24, # Primary problem table 3, 2013
    0.169, 0.194, # Loneliness table 2, 2021
    0.307, 0.227, # Anxiety table 2, 2021
    0.319, 0.260,  # Depression table 2, 2021
    1.44, 1.23, # GDS table 3, 2013
    1.20, 1.04, # GAI table 3, 2013
    2.80, 2.41, # PSWQ table 3, 2013
    2.26, 1.97 # SF12 table 3, 2013
  ),
  
  q = rep(c(2, 4, 4, 4, 2, 2 ,2, 2), each = 2) # Number of controlled covariates
)

smith2021_wuthrich2013

