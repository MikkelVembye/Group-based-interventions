# Dataextraction Rosenblum et al. 2014
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

# Extracting data from table 3 p. 85 

Rosenblum2014 <- tibble(
  
  group = as.factor(rep(c(
    "Double Trouble in Recovery (DTR)",
    "waiting list control group"), 
    each = 1,15
  )),
  
  sample = rep(c(
    "Total",
    "New York",
    "Michigan"
  ), each = 10,1),
  
  N = c(
    rep(c(91,70), each = 1,5), # Total 
    
    rep(c(35, 32), each = 1,5), # New York
    
    rep(c(56, 38), each = 1,5) # Michigan
    
  ),
  
  outcome = rep(c(
    "Days any alcohol past 30",
    "Days heavy alcohol past 30",
    "Days any drugs past 30",
    "Days any drugs or alcohol past 30",
    
    "Medication Adherence"
    
   # "RQOL (quality of life)c (total scale)" Only measured at follow-up 
   ), each = 2,3
 ),
 
 m_pre = c(
   4.5, 6.5,
   2.6, 2.1,
   6.9, 7.7,
   9.3, 11.9,
   1.5, 1.5,

   
   1.9, 3.9,
   0.3, 0.8,
   4.2, 7.1,
   4.7, 9.9,
   1.4, 1.4,

   
   6.1, 8.6, 
   4.0, 3.1,
   8.6, 8.2,
   12.2, 13.6,
   1.6, 1.6 

 ),
 
 sd_pre = c(
   8.2, 9.6,
   6.1, 5.1,
   9.3, 10.4,
   10.1, 11.2,
   0.5, 0.5,

   
   4.7, 7.2,
   1.0, 1.8,
   6.3, 10.2,
   6.1, 10.8,
   0.4, 0.4,
 
   
   9.5, 11.0,
   7.4, 6.5,
   10.5, 10.6, 
   11.0, 11.5,
   0.5, 0.5
  
 ), 
 
 m_post = c(
   3.1, 6.1,
   1.1, 2.1, 
   5.5, 8.0,
   6.8, 11.3,
   1.4, 1.4,
#   2.7, 2.0,
   
   1.6, 3.2,
   0.3, 1.0, 
   3.4, 8.0,
   4.5, 10.2,
   1.4, 1.3,
#   3.5, 3.2,
   
   4.0, 8.4,
   1.5, 3.0, 
   6.8, 8.0,
   8.3, 12.2,
   3.5, 1.6
#   2.1, 1.0
 ),

sd_post = c(
  6.4, 8.8,
  2.8, 6.0,
  9.2, 10.6,
  9.8, 11.1,
  0.4, 0.4,
  # 1.8, 1.8,
  
  4.4, 7.2,
  1.3, 4.5,
  7.1, 10.2, 
  7.8, 10.8, 
  0.3, 0.3,
  #  1.5, 1.7,
  
  7.3, 9.5, 
  3.4, 7.0,
  10.2, 11.1,
  10.6, 11.4, 
  0.5, 0.4
# 1.8, 1.3
)

    
)

