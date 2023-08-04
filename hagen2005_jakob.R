# Data typing from Hagen et al. (2005)
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

# Data from table 2, p. 39 
hagen2005 <- tibble(
  
  outcome = rep(c(
    
  "SCL-90",
  "BAI",
  "BDI",
  "IIP-64-C",
  "SAS-A",
  "SAS-S",
  "YSQ"), each = 2,1
 ),
 
 treatment = rep(c(
     
   "waiting-list", "CBGT"
   ),
   each = 1,7
 ),
 
 n = c(
     17, 15, 
     17, 15, 
     15, 15, 
     17, 14, 
     16, 14, 
     16, 15, 
     16, 14
   ),
 m_pre = c(
     1.19, 1.15,
     20.29, 22.60,
     18.20, 15.40,
     1.35, 1.22,
     73.06, 62.93,
     73.87, 70.80, 
     11.50, 7.21
 ),
 
  sd_pre = c(
      0.61, 0.61,
      10.08, 14.77,
      8.55, 9.86,
      0.72, 0.63,
      20.23, 15.66,
      20.26, 19.16,
      14.15, 8.62
  ) ,
 
 m_post = c(
   1.15, 0.88, 
   22.94, 18.13, 
   18.35, 11.00, 
   1.28, 1.18, 
   60.12, 58.57, 
   69.12, 60.14, 
   10.17, 6.63
 ),
 
 sd_post = c(
   0.47, 0.59,  
   13.46, 12.00,
   9.00, 8.36,
   0.68, 0.76,
   14.17, 18.67,
   13.99, 22.52,
   13.41, 11.16
 ),
 
 m_diff = m_post  - m_pre,

)

hagen2005
