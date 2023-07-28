# Data typing from Druss et al. (2010)
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
druss2010 <- tibble(
  
  outcome = rep(c(
    "patient_activation",
    "Physical_activity",
    "medication_adherence",
    "QoL_physical",
    "QoL_mental"
  ), each = 2,1),
  
  treatment = rep(c(
    "HARP",
    "TAU"
  ), each = 1,5),
  
  n = rep(c(
    41,39
  ), each = 1,5),
  
  m_pre = c(
    48.3, 47.6, # patient activation
    150, 154, # Physical_activity
    1.5, 1.5, # medication_adherence
    36.9, 37.0, # QoL_physical
    33.3, 33.9 # QoL_mental
  ),
  
  sd_pre = c(
    11.5, 12.3,
    236, 194,
    1.2, 1.4,
    10.3, 12.5,
    9.13, 9.3
  ),
  
  m_6months = c(
    52.0, 44.9,
    191, 152,
    1.3, 1.6,
    42.9, 40,
    36.8, 37.0
  ),
  
  sd_6months = c(
    10.1, 9.6,
    278, 249,
    1.3, 1.4,
    14.2, 13.7,
    10.0, 11.8
  ),
  
  # Calculating the differences
  m_diff = m_6months  - m_pre,
)

druss2010  
