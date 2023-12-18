# Data typing from Sacks (2011)
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

# This study makes estimations based on two different analysis based on their
# propensity scores (what i read as the level of control) - i will keep them
# in the same tibble - "sample" indicates different propencity category. 
#Extracted from table 4 p. 1682 and only means plus standard deviation is taken.

sack_2011 <- tibble(
  
  group = rep(c("treatment",
                "control"), 
              each = 1,16),
  
  sample = rep(c("low/medium propensity", 
                 "high proensity"), each = 16,1),
  
  
  
  outcome = rep(c(
    "alcohol_ intoxication",
    "drug_use",
    "BDI_total",
    "GSI_total",
    "SF-36_mental_health",
    "SF-36_social_functioning",
    "Healt_rating",
    "SF-36_physical_health"
  ), each = 2,2),
  
  
  N = rep(c(23,10), 
          each = 16,1),
  
  m_pre = c(# Low/medium propensity 
            6.8, 6.5, # alcohol
            12.9, 17.8, # drugs
            16.1,  18.9, # BDI
            45.3, 47.7, # GSI
            42.4, 37.6, # SF-36 mental
            46.6, 38.5, # SF-36 social functioning 
            3.0, 3.0, # Health rating
            44.9, 47.2, # SF-36 physical health
            
            # High propensity
            6.3, 7.5,
            15.9, 8.3,
            15.3, 15.3,
            42.0, 41.8,
            43.5, 43.1,
            13.2, 43.6,
            3.0, 3.5,
            42.0, 42.6
            
            ),
  
  
  sd_pre = c(2.4, 2.2,
             7.2, 8.7,
             6.7, 10.0,
             4.0, 9.0,
             11.8, 13.0,
             11.3, 13.7,
             1.2, 1.1,
             9.0, 10.3,
             
             2.3, 0.7,
             7.5, 5.7,
             3.7, 11.9,
             10.3, 7.4,
             10.4, 9.1,
             13.9, 11.3,
             1.0, 0.6,
             10.4, 14.0),
  
  
  m_post = c(
    2.7, 0.6,
    0.6, 2.1,
    13.1, 8.2,
    38.6, 36.4,
    46.5, 47.2,
    44.9, 46.5,
    2.9, 3.0,
    45.5, 47.6,
    
    
    0.4, 1.0,
    0.4, 1.3,
    10.0, 16.5, 
    39.0, 42.3,
    49.5, 41.6, 
    47.5, 46.3,
    3.1, 4.3,
    42.8, 48.8
  ),
  
  sd_post = c(
    2.1, 1.8,
    1.9, 4.7,
    11.9, 6.6,
    9.2, 10.1, 
    14.2, 13.7,
    12.6, 11.5,
    1.2, 1.0,
    7.3, 10.1, 
    
    1.1, 1.4,
    0.9, 1.9,
    10.9, 8.4,
    7.7, 5.9,
    11.1, 13.2, 
    12.0, 7.7,
    1.0, 0.5,
    12.9, 6.6
  )
  
  
); sack_2011
