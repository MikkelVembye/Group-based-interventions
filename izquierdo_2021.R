# Dataextraction from Izquierdo et al. (2021)
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

# Rows 1-10 is data from table 3, p. 10 and rows 11-22 is from table 4 p. 13. 

izquierdo_2021 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,11)),
  
  n = rep(c(7,
            7),
          each = 1,11),
  
  outcome = rep(c("EE",
                  "OE",
                  "CS",
                  "ACI",
                  "SCI",
                  "POSIT",
                  "NEGAT",
                  "AFFECT",
                  "DISOR",
                  "DISORG",
                  "Total_score"),
                each = 2,1), 
  
  m_pre = c(
    6.57, 10.57, # EE
    6.86, 8.29,  # OE
    13.43, 18.86,# CS
    NA, NA,      # ACI - Basline data not reported
    NA, NA,      # SCI - Basline data not reported
    21.29, 15.14,# POSIT
    10.57, 6.86, # NEGAT
    35.86, 37.14,# AFFECT
    5.00, 3.86,  # DISOR
    4.86, 2.43,  # DISORG
    77.57, 65.43 # Total_score
    ),
  
  sd_pre = c(
    2.44, 3.36,
    2.19, 1.38,
    2.44, 4.34,
    NA, NA,
    NA, NA,
    11.09, 5.84,
    4.24, 2.54,
    5.64, 4.56,
    3.00, 3.18, 
    2.67, 1.90,
    18.30, 10.85),
  
  m_post = c(
    17.86, 10.57,
    16.29, 8.14,
    34.14, 18.71,
    36.57, 11.14,
    4.57, 1.14,
    8.14, 15.71,
    5.43, 7.71,
    16.86, 37.00,
    2.43, 3.86,
    1.29, 1.57,
    34.14, 65.86
  ),
  
  sd_post = c(
    1.86, 3.15,
    3.09, 1.35,
    4.81, 2.87,
    5.32, 3.19,
    0.54, 0.38,
    2.12, 6.40,
    1.27, 3.45,
    2.27, 3.74,
    0.53, 2.54,
    0.49, 0.79,
    3.13, 11.13),
  
  m_3months= c(
    16.00, 9.71,
    16.29, 7.57,
    32.29, 17.29,
    37.57, 11.00,
    4.57, 1.00,
    7.14, 15.71,
    5.00, 8.00,
    13.29, 35.14,
    2.57, 3.71,
    1.00, 1.14,
    29.00, 63.71
    ),
  
  sd_3months = c(
    3.21, 2.98,
    2.63, 2.07,
    5.28, 2.87,
    4.20, 3.21,
    0.53, 0.00,
    1.07, 6.97,
    0.82, 3.56,
    0.95, 4.70,
    0.79, 2.63,
    0.00, 0.38,
    1.83, 11.97
    ),
  
  m_6months = c(
    16.71, 9.71,
    15.71, 6.86,
    32.43, 16.57,
    37.14, 11.57,
    4.29, 1.14,
    7.29, 15.29,
    5.00, 8.29,
    13.00, 36.14,
    2.29, 3.57,
    1.00, 1.29,
    28.57, 64.57
  ),
  
  sd_6months = c(
    2.50, 2.93, 
    3.45, 1.07, 
    5.19, 2.88,
    4.88, 2.94,
    0.76, 0.38,
    1.11, 6.26, 
    0.83, 3.77,
    0.82, 5.43, 
    0.49, 2.15, 
    0.00, 0.49,
    1.81, 9.68
  ),
  

  # Calculate mean_diff, mean_3months_diff, and mean_6months_diff
  mean_diff = m_post - m_pre
  
) |> 
  mutate(
    # Calculating the differences in means for 3 and 6 months compared 
    # to baseline - and taking into account that there is no pre-test for 
    # the scales ACI and SCI
    mean_3months_diff = ifelse(outcome %in% c("ACI", "SCI"), 
                               m_3months - coalesce(m_pre, m_post), 
                               m_3months - m_pre),
    mean_6months_diff = ifelse(outcome %in% c("ACI", "SCI"), 
                               m_6months - coalesce(m_pre, m_post), 
                               m_6months - m_pre)
    ); izquierdo_2021 





