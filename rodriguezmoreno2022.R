# Dataextraction from Rodriguez-Moreno (2022)
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

# Data from table 2 containing means and standard deviation + within effect size
# on page 351

rodriguezmoreno2022 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,7)),
  
  outcome = as.factor(rep(c("CORE_OM",
                            "DERS",
                            "TAS",
                            "Metacognition_self",
                            "Metacognition_others",
                            "Metacognition_decentration",
                            "Metacognition_mastery"), each = 2,1)),
  
  n = rep(c(10, 10), each = 7), 
  
  m_pre = c(
    12.85, 15.24, # CORE-OM Total
    95, 100.9, # DERS Total
    54.2, 51.5, # TAS Total
    4.3, 4.1, # Metacognition Self
    2.1, 2.85, # Metacognition Others
    0.75, 1.15, # Metacognition Decentration 
    2.9, 3.3    # Metacognition Mastery
  ),
  
  sd_pre = c(
    4.6, 6.68,
    16.19, 17.67,
    11.52, 10.99,
    1.32, 0.97,
    0.52, 1.31,
    0.75, 1.03,
    1.07, 0.67
  ),
  
  m_post = c(
    7.68, 11.78,
    76.4, 97.11,
    43.5, 52.63,
    5.75, 3.35,
    2.9, 3.3,
    1.15, 0.7,
    4.18, 3.05
  ),
  
  sd_post = c(
    1.94, 4.59,
    26.78, 29.12,
    11.7, 12.59,
    1.36, 1.27,
    1.07, 0.67,
    1.03, 0.48,
    1.35, 1.23
  ), 
  
  m_diff = m_post - m_pre,
  
  within_group_effect = c( # Within-group effect size baseline – 3-month follow-up
    1.14, 0.64,
    0.74, 0.16,
    1.11, 0.10,
    1.73, 0.56,
    0.99, 0.32,
    0.59, 0.51,
    1.51, 0.20
  )
  
  
  ); rodriguezmoreno2022
  
