# Dataextraction from Morton (2012)
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

# Data from table 3 containing means and standard deviation

morton2012 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,12)),
  
  outcome = as.factor(rep(c("BEST_composite",
                            "BEST_BPD_thoughts_&_feelings",
                            "BEST_BPD_negative_behaviors",
                            "BEST_BPD_positive_behaviors",
                            "DASS_stress",
                            "DASS_anxiety",
                            "DASS_depression",
                            "BHS",
                            "AAQ-II",
                            "DERS_total",
                            "FFMQ_total",
                            "ACS_total"
  ),
  each = 2,1)),
  
  n = rep(c(21, 20),
  each = 1,12),
  
  
  m_pre = c(
    44.57, 49.80, # BEST composite
    27, 29.90, # BEST BPD thoughts & feelings
    11.33, 12.25, # BEST BPD negative behaviors
    8.76, 7.35, # BEST BPD positive behaviors
    28.48, 30.3, # DASS stress
    23.33, 24.20,# DASS anxiety
    31.52, 33.7, # DASS depression
    14.40, 15.7, # BHS
    24.10, 22.55,# AAQ-II
    131.76, 134.42, # DERS total
    96.52, 92.15,# FFMQ total
    4.86, 4.93   # ACS total
  ),
  
  sd_pre = c(
    11.16, 12.35,
    7.73, 7.52,
    3.58, 4.98,
    2.62, 3.18,
    10.04, 10.16,
    9.34, 11.76,
    10.86, 8.74,
    4.87, 3.58,
    9.29, 8.32,
    25.15, 19.45,
    23.01, 20.81,
    0.74, 0.68
  ),
  
  m_post = c(
    32.76, 47.42,
    20.47, 28.67,
    7.12, 10.83,
    9.82, 7.08,
    26.55, 31.57,
    19.67, 26.28,
    22.67, 31.00,
    9.7, 16.43,
    35.3, 23.1,
    113.04, 140.04,
    108.81, 90.87,
    4.35, 5.08
  ), 
  
  sd_post = c(
    12.47, 11.00,
    9.45, 7.76,
    2.18, 4.22,
    2.88, 3.06,
    11.58, 9.93,
    11.02, 8.33,
    14.73, 8.51,
    6.34, 3.69,
    10.8, 7.1,
    17.64, 20.88,
    19.11, 20.67,
    0.65, 0.56
  ),
  
  m_diff = m_post - m_pre,

); morton2012 
