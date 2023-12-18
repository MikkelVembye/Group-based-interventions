library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Crawford et al. (2012)
#Draft by Jasmin

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


Crawford2012 <- 
  tibble(
    
    outcome = rep(
      c(
        "Global assessment of functioning", "Positive and negative syndrome scale",
        "Positive symptoms score", "Negative symptoms score", 
        "General symptoms score", "Social functioning", "Wellbeing", 
        "Satisfaction with care", "Morisky score"
      ), 
      each = 6),
    group = rep(rep(c("Standard care", "Actvity groups", "Group art therapy"), 2), n_distinct(outcome)),
    timing = rep(rep(c(12, 24), each = n_distinct(group)), n_distinct(outcome)),
    
    N_baseline = rep(rep(c(137, 140, 140), 2), n_distinct(outcome)),
    N = rep(c(121, 121, 119, 117, 121, 117), n_distinct(outcome)),
    
    m_pre = c(
      rep(c(44.9, 45.0, 44.8), 2),
      rep(c(72.6, 75.3, 74.3), 2),
      rep(c(17.3, 18.2, 18), 2),
      rep(c(18.5, 18.7, 18.7), 2),
      rep(c(36.8, 37.6, 37.6), 2),
      rep(c(8.1,  9., 8.6), 2),
      rep(c(64.5, 59.1, 58.3), 2),
      rep(c(24.9, 23.8, 24.8), 2),
      rep(c(1.2, 1.2, 1.0), 2)
    ), 
    
    sd_pre = c(
      rep(c(12.6, 12.7, 13.1), 2),
      rep(c(21.5, 22., 23.7), 2),
      rep(c(5.6, 6.8, 6.9), 2),
      rep(c(7.5, 7., 7.1), 2),
      rep(c(11.3, 12.5, 12.5), 2),
      rep(c(4.7, 4.8, 4.2), 2),
      rep(c(20.6, 19.5, 21.1), 2),
      rep(c(5.7, 6.2, 5.7), 2),
      rep(c(1.3, 1.3, 1.2), 2)
    ),
    
    m_post = c(
      45.7, 45.5, 44.9, # GLobal assessment of functioning 12 month
      46.8, 46.4, 45.6, # GLobal assessment of functioning 24 month
      
      71.2, 69.6, 72.7, # Positive and negative syndrome scale 12 m
      68.1, 66.9, 69.2, # Positive and negative syndrome scale 24 m
      
      16.7, 16.1, 17.3, # Positive symptoms score 12m
      16.1, 15.6, 16.8, # Positive symptoms score 24m
      
      18.2, 17.3, 18.4, # Negative symptoms score 12m
      17.2, 16.4, 16.9, # Negative symptoms score 24m
      
      36.3, 35.9, 37,   # General symptoms score 12m
      34.9, 34.9, 35.3, # General symptoms score 24m
      
      8.5, 8.1, 8.3, # Social functioning 12m
      8.1, 8, 8.2,   # Social functioning 24m
      
      64.1, 63.6, 59.6, # Wellbeing 12m 
      68.1, 66.1, 65.1, # Wellbeing 24m 
      
      24.3, 25., 23.6,  # Satisfaction with care 12m
      24.2, 24.9, 23.1, # Satisfaction with care 24m
      
      0.7, 0.6, 0.7, # Morisky score 12m
      0.6, 0.5, 0.6  # Morisky score 24m
      
    ),
    
    sd_post = c(
      14.4, 14.1, 14.6, 
      12.8, 13.6, 13.1, 
      
      24.6, 23.2, 27.3,
      20.7, 23.3, 21.8,
      
      6.3, 5.9, 7.6,
      5.5, 6.4, 6.5,
      
      7.7, 7.2, 8,
      7.3, 6.8, 7.1, 
      
      13, 12.7, 14,
      11.3, 12.4, 11.4,
      
      4.9, 4.6, 5.,
      4.8, 4.5, 4.8,
      
      23.7, 23.2, 20.8,
      18.8, 18.4, 18.6,
      
      6.4, 5.2, 6.5,
      5.3, 5., 5.9,
      
      1.1, .9, 1.,
      .9, .9, .9
      
    )
    
  ); Crawford2012

View(Crawford2012)