# Dataextraction from Valiente et al. (2022)
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

# Data from table 3 and table (p. 4)  containing means, standard deviation. It also contains
# difference Morris' d and p

valiente2022  <- tibble(
  group = rep(c("Multicomponent PPI", # Intervention
                "Treatment as usual"), # Control
              each = 1,16),
  
  outcome = rep(c(
                  # Table 3
                  "SPWB Autonomy",          #Scales of Psychological Well-Being (SPWB)
                  "SPWB Positive_relationship", 
                  "SPWB Self_acceptance",
                  "SPWB Enviormental mastery",
                  "SPWB Purpose_in_life",
                  "SPWB Personal_Growth",
                  "SWLS",                  # Satisfaction With Life Scale (SWLS)
                  
                  # Table 4
                  "Somatization",         # Symptom checklist-90-Revised (SCL-90-R)
                  "Obsessive–compulsive", 
                  "Interpersonal sensibility",
                  "Depression", 
                  "Anxiety", 
                  "Hostility", 
                  "Phobic anxiety",
                  "Paranoid ideation", 
                  "Psychoticism"
                  
                  ), 
                each = 2,1),
  
  N = rep(c(52,61),
          each = 1,16),
  
  
  m_pre = c(
    
    # Table 3
    19.5, 20.1,
    34.7, 33.9,
    29.9, 31.6,
    30.6, 31.4,
    34.4, 34.4,
    35.3, 35.7,
    17.4, 18.2,
   
    # Table 4
    1.00, 1.02,
    1.66, 1.62,
    1.42, 1.41,
    1.51, 1.47,
    1.12, 1.31,
    0.75, 0.63,
    0.89, 1.09,
    1.15, 1.15,
    1.14, 0.99
    
  ),

 
  
  m_post = c(
    # Table 3
    19.9, 19.7,
    35.7, 34.4,
    31.7, 31.1,
    32.7, 30.8,
    34.9, 34.2,
    35.5, 35.5,
    19.0, 19.2,
    
    
    # Table 4
    0.90, 0.96,
    1.50, 1.48,
    1.28, 1.27,
    1.30, 1.40,
    1.07, 1.14,
    0.71, 0.61,
    0.91, 1.07,
    1.18, 1.21,
    0.99, 0.98
  )
  
)
  