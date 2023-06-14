# Data typing from Craigie & Nathan (2009)
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

# Data from table 2, p. 308 
craigie_and_nathan2009 <- tibble(
  analysis = rep(c(
    "ITT",
    "Completers"), each = 6),
  
  outcome = rep(c(
  "BDI_II", 
  "BAI",
  "Q_LES_Q"), each = 2,2),
  
  treatment = rep(c(
    "individual",
    "group"), each = 1, 6), 
  
  n = c(rep(c(116, 240), each = 1,3),
        rep(c(77, 157), each = 1,3)
        
  ),
  
  # pre-treatment means 
  m_pre = c( 
    # Intention to treat
    30.0, 31.3, # BDI-II
    21.1, 20.2, # BAI 
    43.2, 42.3, # Q-LES-Q
    # Completers (treatment-on-treated)
    30.8, 30.7,
    20.7, 19.6,
    46.4, 42.9
  ), 
  
  # pre-treatment standard deviation 
  sd_pre = c(
    # Intention to treat
    10.4, 11.2,
    12.2, 11.3,
    14.7, 14.1,
    # Completers (treatment-on-treated)
    10.3, 11.1,
    12.0, 11.0,
    13.5, 13.8
  ),
  
  # post-treatment means
  m_post = c(
    # Intention to treat
    16.3, 20.9,
    12.9, 15.6,
    52.3, 50.8,
    # Completers (treatment-on-treated)
    11.7, 17.7,
    9.2, 13.7,
    62.7, 55.3
  ),
  
  # post-treatment means 
  sd_post = c(
    # Intention to treat
    12.2, 13.6,
    11.2, 11.4,
    19.9, 17.9,
    # Completers (treatment-on-treated)
    9.9, 12.4,
    8.3, 10.7,
    14.7, 15.8
  ),
  
  # Calculating the differences
  m_diff = m_post  - m_pre,
  
)

