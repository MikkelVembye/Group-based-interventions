# Data typing from Gutman et al. (2019)
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

# Data from table 2, p. 67

gutman_2019_raw <- tibble(
  
  group = as.factor(rep(c("intervention", "control"), each = 10)),
  
  studid = 1:20,
  
  pretest_PSS = c(
    25, 37, 34, 33, 19, 25, 36, 34, 32, 35, # intervention participants
    25, 30, 31, 34, 26, 21, 32, 34, 27, 35 # control group
  ),
  
  posttest_PSS = c(21, 32, 30, 29, 15, 23, 32, 27, 28, 30,
                   27, 30, 33, 41, 30, 27, 32, 36, 33, 36), 
  
  pretest_WHOQOL = c(
    121, 114, 84, 89, 82, 83, 86, 70, 119, 93, 
    97, 113, 98, 93, 86, 93, 98, 57, 91, 101
  ), 
  
  postest_WHOQOL = c(
   125, 118, 95, 89, 87, 91, 93, 77, 125, 107,
   100, 112, 95, 90, 88, 85, 100, 43, 74, 102
  )
) %>% 
    mutate(
      growthtest_PSS = posttest_PSS-pretest_PSS,
      growthtest_WHOQOL = postest_WHOQOL-pretest_WHOQOL,
      
      # Standardizing variables  
      m_pre_sd_PSS = (pretest_PSS - mean(pretest_PSS))/sd(pretest_PSS),
      
      m_post_sd_PSS = (posttest_PSS - mean(posttest_PSS))/sd(posttest_PSS),
      
      m_pre_sd_WHOQOL = (pretest_WHOQOL - mean(pretest_WHOQOL))/sd(pretest_WHOQOL),
      
      m_post_sd_WHOQOL = (postest_WHOQOL - mean(postest_WHOQOL))/sd(postest_WHOQOL)
      
      
    
); gutman_2019_raw



gutman_2019_mean <-tibble(
  
  group = as.factor(rep(c("intervention", "control"), each = 1,2)),
  
  outcome = as.factor(rep(c("PSS", "WHOQOL"), each = 2,1)),
  
  m_pre = c(35,29.5, 94.1, 92.7),
  
  sd_pre = c(5.92, 4.60, 17.59, 14.44),
  
  m_post = c(26.7, 32.5, 100.7, 88.9),
  
  sd_post = c(5.45, 4.35, 16.97, 19.24),
  
  m_diff = m_post  - m_pre,
  
); gutman_2019_mean



