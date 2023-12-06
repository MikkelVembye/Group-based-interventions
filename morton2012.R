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


# Regression coefficients extracted from text (pp.537-539)
morton2012_es <- tibble(
  study = "morton2012",
  es_method = "Multilevel model",
  group = rep(c("treatment", # Treatment is ACT + TAU
                "control", # Control is TAU.
                "interaction"), each = 1,9),
  outcome = rep(c("BEST_composite", "BEST_BPD_thoughts_feelings", 
                  "BEST_BPD_negative_behaviors", "DASS_anxiety", "BHS", 
                  "AAQ-II", "DERS_total", "FFMQ_total","ACS_total"), each = 3,1),
  
  beta = c(-11.52, -1.8, 9.71,  # BEST Composite
           -6.6, -0.05, 6.54, # BEST BPD thoughts and feelings
           -1.11, -3.98, 2.87, # BEST BPD negative and behaviors
           -3.92, 3.98, 7.90, # DASS anxiety
           -4.93, 0.62, 5.55, # BHS
           5.55, 0.37, -9.88, # AAQ-II
           -19.17, 4.77, 23.94, # DERS total
           12.30, -0.31, -12.62, # FFMQ total
           -0.54, 0.18, 0.71), # ACS total
  
  se = c(2.75, 3.19, 4.21, 
         1.77, 2.05, 2.72, 
         1.04, 0.90, 1.38,
         2.24, 2.50, 3.36,
         1.27, 1.38, 1.87,
         1.87, 2.65, 3.60,
         5.68, 6.30, 8.48,
         3.71, 4.06, 5.50,
         0.15, 0.16, 0.21),
  
  t = c(2.3, -0.57, 2.3, 
        -3.72, -0.03, 2.41, 
        -1.06, -4.42, 2.08,
        -1.74, 1.59, 2.35,
        -3.88, 0.45, 2.96,
        2.96, 0.139, -2.74,
        -3.38, 0.76, 2.82,
        3.31, -0.08, -2.29,
        -3.63, 1.15, 3.33), # unsure about this
  
  p = c(0.000, 0.575, 0.28, 
        0.001, 0.979, 0.022, 
        0.296, 0.0, 0.046,
        0.091, 0.121, 0.025,
        0.000, 0.656, 0.006,
        0.006, 0.891, 0.010,
        0.002, 0.454, 0.008,
        0.002, 0.939, 0.028,
        0.001, 0.258, 0.002),
  
  d = c(0.99,0.15, 0.81, 
        0.88, 0.01, 0.85, 
        1.05, 0.29, 0.75,
        0.41, 0.41, 0.83,
        0.91, 0.11, 1.02,
        1.02, 0.04, 0.99,
        0.78, 0.19, 0.98,
        0.79, 0.02, 0.80,
        0.89, 0.30, 1.20)
  
); morton2012_es
