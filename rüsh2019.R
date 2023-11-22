# Dataextraction from Rüsch (2019)
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

# Data from table 1 containing means and standard deviation on page 335
# The table also contains partial η2 values, p-values and F-test

rusch2019 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,7)),
  
  outcome = as.factor(rep(c("Job_search",
                            "Help_seeking",
                            "SISR",
                            "SSMIS_SF",
                            "CES-D",
                            "BHS",
                            "Secrecy"), each = 2,1
  )),
  
  n_pre = rep(c(
    23, 19
  ), each = 1,7),
  
  m_pre = c(
    3.1, 3.3, # Job-search self-efficacy (1–5)
    3.4, 3.0,  # Help-seeking intentions (1–7)
    15.3, 16.3, # Recovery (SISR: 4–24)
    17.3, 17.3,  # Self-stigma (SSMIS-SF: 5–45)
    39.2, 41.4, # Depressive symptoms (CES-D: 15–60)
    14.9, 14.1, # Hopelessness (BHS: 4–24)
    4.0, 3.7   # Hopelessness (BHS: 4–24)
  ),
  
  sd_pre = c(
    0.8, 0.8,
    1, 1,
    4.1, 4.2,
    4.3, 8.1,
    7.4, 7.2,
    3.6, 4.5, 
    1.2, 1.6
  ),
  
  n_3weeks = rep(c(
    21, 17
  ), each = 1,7),
  
  m_3weeks = c(
    3.2, 3.2, 
    3.6, 3,
    16.1, 15.9, 
    15.3, 16.9, 
    35.2, 39.1,
    13.8, 13.7,
    3.8, 3.7
  ),
  
  sd_3weeks = c(
    0.7, 0.9, 
    1, 0.9, 
    4.2, 3.6, 
    6.2, 8, 
    8.8, 10.2, 
    3.9, 4.6, 
    1, 1.7
  ),
  
  n_6weeks = rep(c(
    18, 17
  ), each = 1,7),
  
  m_6weeks = c(
    3.4, 3.3, 
    3.5, 3.2, 
    17.8, 16.1, 
    14.6, 19.1, 
    31.5, 40.1, 
    12.6, 14.2, 
    3.5, 3.9
  ),
  
  sd_6weeks = c(
    0.8, 0.9,
    1, 1, 
    3.4, 4.4,
    7.9, 8.7,
    8.5, 10.1,
    3.6, 5, 
    1.3, 1.7
  ),
  
  n_12weeks = rep(c(
    20, 13
  ), each = 1,7),
  
  m_12weeks = c(
    3.2, 3.2,
    3.3, 3.1,
    17.3, 15.7,
    15.4, 18.3,
    32, 37.5,
    12.3, 14.4,
    3.7, 4.4
  ),
  
  sd_12weeks = c(
    1, 0.9, 
    0.6, 0.9,
    3.5, 2.6,
    7, 9.6,
    7.6, 9.8,
    4.2, 3,
    0.9, 1.4
  ),
  
  m_diff3 = m_3weeks - m_pre,
  
  m_diff6 = m_6weeks - m_pre,
  
  m_diff12 = m_12weeks - m_pre

); rusch2019

