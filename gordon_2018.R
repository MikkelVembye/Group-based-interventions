# Dataextraction from Gordon et al. (2018)
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

# Data from table 2, p. 124

gordon_2018 <- tibble(
  group = as.factor(rep(c("SCIT", 
                          "Control"), 
                        each = 1,9)),

  n = (rep(c(21,
             15), 
           each = 1,9)),
  
 outcome = as.factor(rep(c(
   "QLS_total",
   "QLS_interpersonal",
   "QLS_ instrumental",
   "BLERT",
   "LSP",
   "SCSQ-ToM",
   "SSPA",
   "DASS-21",
   "AIHQ-Hostility-bias"), 
   each = 2, 1)),
 
 m_pre = c(
   50.24, 45.73, # QLS total
   15.86, 13.40, # QLS interpersonal subscale 
   6.19, 6.00,   # QLS instrumental subscale
   14.43, 14.40, # BLERT
   13.38, 14.53, # LSP
   7.33, 7.07,   # SCSQ-ToM
   65.19, 59.07, # SSPA
   43.33, 53.33, # DASS-21
   8.43, 8.53),  # AIHQ-Hostility-bias
 
 sd_pre = c(
   15.83, 13.85, 
   7.26, 7.18, 
   5.94, 6.21, 
   3.62, 3.15, 
   7.44, 6.12, 
   1.62, 1.79, 
   12.03, 10.57, 
   26.20, 32.73, 
   3.80, 4.24
 ),
 m_post = c(
   56.30, 47.67, 
   17.40, 14.47, 
   6.95, 6.53, 
   15.90, 15.93, 
   10.29, 13.60,
   7.50, 7.07, 
   71.25, 63.57, 
   41.10, 49.13, 
   6.80, 8.87
 ),
 sd_post = c(
   16.34, 13.19, 
   8.33, 6.57, 
   5.48, 6.63, 
   3.62, 2.76, 
   4.59, 6.10, 
   1.53,1.40, 
   8.22, 7.89, 
   25.96, 31.40, 
   1.73, 2.29
 ),
 
 mean_diff = m_post - m_pre,
); gordon_2018

# Between group ANOVA can be seen in table 2 as well 

# ANOVA_F = c(
#  2.503, 0.041, 0.000, 0.019, 0.905, 0.632, 0.039, 0.573
# ),
# ANOVA_p = c(
#  0.123, 0.841, 0.983, 0.892, 0.348, 0.432, 0.845, 0.455
# ),
# ANOVA_g2p = c(
#  0.070, 0.001, 0.000, 0.001, 0.026, 0.019, 0.001, 0.017
# )
#)

