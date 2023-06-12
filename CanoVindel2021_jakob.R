# Data typing from Cano_Vindel et al. 2021 
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


cano_vindel2021 <- tibble(
  
  sample = rep(c("ITT",
                 "Per_protocol"),
               each = 20,1 
  ),
  
  
  
  
  group = rep(c("TAU", 
                "TAU-GCBT"), 
              each = 1,20)
  ,
  
  outcome = rep(c("GAD_7",
                  "GAD_7",
                  "PHQ_9",
                  "PHQ_9",
                  "PHQ_15",
                  "PHQ_15",
                  "working_life_Function",
                  "working_life_Function",
                  "Socal_life_Function",
                  "Socal_life_Function",
                  "Family_life_Function",
                  "Family_life_Function",
                  "Physical_QoL",
                  "Physical_QoL",
                  "Psychological_QoL",
                  "Psychological_QoL",
                  "Social_QoL",
                  "Social_QoL",
                  "Environment_QoL",
                  "Environment_QoL"), 
                each = 1,2)
  ,
  
  n = c(rep(c(534, 527), each = 1,10),
        rep(c(534, 527, 316, 315, 238, 273, 204, 229, 180, 208), each = 1,2)
    )
  ,
  
  
  m_pre = c(12.1, 12.5, # ITT GAD-7 Anxiety Baseline
            13.5, 13.7, # ITT PHQ9 Depression
            14.0, 14.3, # ITT PHQ-15
            3.5, 3.6, # ITT working life
            4.6, 4.7, # ITT social life
            4.6, 4.8, # ITT Family life
            22.4, 22.1, # ITT Physical
            16.9, 16.9, # ITT Psychological
            9.1, 9.1, # ITT Social
            25.3, 25.7, # ITT Environment
            
            # Baseline
            12.1, 12.5, # Per-protocol GAD-7 Anxiety 
            13.5, 13.7, # Per-protocol PHQ9 Depression
            14.0, 14.3, # Per-protocol PHQ-15
            3.5, 3.6, # Per-protocol working life
            4.6, 4.7, # Per-protocol social life
            4.6, 4.8, # Per-protocol Family life
            22.4, 22.1, # Per-protocol Physical 
            16.9, 16.9, # Per-protocol Psychological
            9.1, 9.1, # Per-protocol Social
            25.3, 25.7), # Per-protocol Environment
  
  
  sd_pre = c(4.7, 4.6, # ITT GAD-7 Standard deviation 
            5.4, 5.3, # ITT PHQ9 Depression
            4.8, 4.9, # ITT PHQ-15
            3.1, 3.2, # ITT working life
            3.0, 3.0, # ITT social life
            3.1, 3.0, # ITT Family life
            4.3, 4.3, # ITT Physical
            3.8, 3.8, # ITT Psychological
            2.4, 2.4, # ITT Social
            4.5, 4.6, # ITT Environment
            
            # Baseline
            4.7, 4.6, # Per-protocol GAD-7 Anxiety 
            5.4, 5.3, # Per-protocol PHQ9 Depression
            4.8, 4.9, # Per-protocol PHQ-15
            3.1, 3.2, # Per-protocol working life
            3.0, 3.0, # Per-protocol social life
            3.1, 3.0, # Per-protocol Family life
            4.3, 4.3, # Per-protocol Physical 
            3.8, 3.8, # Per-protocol Psychological
            2.4, 2.4, # Per-protocol Social
            4.5, 4.6), # Per-protocol Environment
            
  
  m_post = c(9.5, 6.8, # ITT GAD-7 Anxiety post-treatment
             10.8, 8.0, # ITT PHQ9 Depression
             11.7, 9.9, # ITT PHQ-15
             3.0, 2.6, # ITT working life
             4.1, 3.2, # ITT social life
             3.9, 3.1, # ITT Family life
             23.2, 24.7, # ITT Physical 
             17.7, 19.2, # ITT Psychological
             9.4, 9.8, # ITT Social
             25.7, 27.2, # ITT Environment
            
            10.2, 6.0, # post
            11.7, 9.9,
            12.1, 9.1,
            3.1, 2.4,
            4.1, 2.9,
            4.0, 2.8,
            22.7, 25.1,
            17.4, 19.9,
            9.2, 10.0,
            25.5, 27.8),
  
  
  sd_post = c(5.4, 4.7, # ITT GAD-7 Anxiety post-treatment
              6.4, 5.7, # ITT PHQ9 Depression
              5.2, 5.4, # ITT PHQ-15
              3.1, 3.2, # ITT working life
              3.1, 3.0, # ITT social life
              3.1, 2.9, # ITT Family life
              4.5, 4.6, # ITT Physical 
              3.9, 4.9, # ITT Psychological
              3.1, 3.1, # ITT Social
              5.3, 5.6, # ITT Environment
              
              5.5, 4.3, # post
              6.6, 5.2,
              5.2, 5.3,
              3.1, 2.9,
              3.1, 2.8,
              3.1, 3.1,
              4.6, 4.7,
              4.2, 4.0,
              2.4, 2.7,
              4.7, 4.8),
  
  mean_diff = m_post - m_pre,
  
  
  m_3months = c(8.7, 7.3, # ITT GAD-7 Anxiety 3 months
                10.2, 8.4, # ITT PHQ9 Depression
                11.4, 10.1,# ITT PHQ-15
                2.7, 2.4, # ITT PHQ-15 
                3.5, 3.2, # ITT social life
                3.5, 3.1, # ITT Family life
                23.5, 24.2, # ITT Physical 
                18.1, 18.9, # ITT Psychological
                9.5, 9.7, # ITT Social
                26.1, 26.9, # ITT Environment 
                
                8.9, 6.7, # Per protocol
                10.3, 7.8, # Per protocol
                11.7, 9.5, # Per protocol
                2.6, 2.5, # Per protocol
                3.4, 3.1, # Per protocol
                3.5, 3.0, # Per protocol
                23.2, 24.4, # Per protocol
                18.0, 19.3, # Per protocol
                9.3, 9.8, # Per protocol
                26.1, 27.5), # Per protocol 
  
  
  sd_3months = c(5.3, 5.0, # ITT GAD-7 Standard deviation 
                  6.4, 5.7, # ITT PHQ9 Depression
                  5.1, 5.3, # ITT PHQ-15
                  3.0, 3.0, # ITT working life
                  3.1, 2.9, # ITT social life
                  3.1, 3.1, # ITT Family life
                  4.6, 4.8, # ITT Physical
                  3.9, 4.2, # ITT Psychological
                  2.3, 2.3, # ITT Social
                  4.9, 5.1, # ITT Environment
                  
                  5.4, 4.9, # Per-protocol GAD-7 Anxiety 
                  6.5, 6.0, # Per-protocol PHQ9 Depression
                  5.0, 5.4, # Per-protocol PHQ-15
                  3.0, 2.9, # Per-protocol working life
                  3.2, 2.9, # Per-protocol social life
                  3.1, 3.0, # Per-protocol Family life
                  4.8, 4.9, # Per-protocol Physical 
                  4.0, 4.2, # Per-protocol Psychological
                  2.2, 2.4, # Per-protocol Social
                  4.8, 5.2), # Per-protocol Environment
  
  mean_diff_3months = m_3months - m_pre,
 
  
  m_6months = c(8.6, 6.9, # ITT GAD-7 Anxiety 6 months
                9.9, 7.9, # ITT PHQ9 Depression
                11.1, 9.8, # ITT PHQ-15
                2.7, 2.1, # ITT working life
                3.4, 2.7, # ITT social life
                3.6, 2.7, # ITT Family life
                23.6, 24.3, # ITT Physical
                18.5, 18.9, # ITT Psychological
                9.5, 9.7, # ITT Social
                26.5, 27.1, # ITT Environment
                
                8.8, 6.2, # 6 months
                10.0, 7.3,
                11.5, 9.2,
                2.8, 1.9,
                3.6, 2.6,
                3.6, 2.6,
                23.1, 24.7,
                18.3, 19.3,
                9.6, 9.8,
                26.4, 27.7),
  
  sd_6months = c(5.4, 5.1, # ITT GAD-7 Standard deviation 
                 6.4, 6.1, # ITT PHQ9 Depression
                 5.3, 5.6, # ITT PHQ-15
                 3.0, 2.9, # ITT working life
                 3.2, 3.1, # ITT social life
                 3.2, 3.1, # ITT Family life
                 4.5, 4.4, # ITT Physical
                 3.8, 3.9, # ITT Psychological
                 2.5, 2.2, # ITT Social
                 4.8, 4.8, # ITT Environment
                 
                 5.7, 4.9, # Per-protocol GAD-7 Anxiety 
                 6.6, 6.1, # Per-protocol PHQ9 Depression
                 5.3, 5.7, # Per-protocol PHQ-15
                 3.0, 2.7, # Per-protocol working life
                 3.2, 2.8, # Per-protocol social life
                 3.1, 2.7, # Per-protocol Family life
                 4.8, 4.9, # Per-protocol Physical 
                 4.2, 4.2, # Per-protocol Psychological
                 2.5, 2.2, # Per-protocol Social
                 4.8, 5.0), # Per-protocol Environment
  

  mean_diff_6months =  m_6months - m_pre,
  
  m_12months =c(8.3, 6.6, # ITT GAD-7 Anxiety 12 months
                9.4, 7.8, # ITT PHQ9 Depression
                10.7, 9.4, # ITT PHQ-15
                3.1, 2.4, # ITT working life
                3.8, 2.9, # ITT social life
                3.8, 2.8, # ITT Family 
                24.2, 26.4, # ITT Physical
                18.7, 19.9, # ITT Psychological
                9.7, 11.1, # ITT Social
                27.5, 31.2, # ITT Environment
                
                8.7, 5.8, # 12 months
                10.7, 9.4,
                11.7, 8.8,
                3.3, 2.0,
                4.0, 2.6,
                3.9, 2.5,
                22.7, 25.6,
                18.3, 20.2,
                9.3, 10.0,
                26.6, 28.3),
  
  sd_12months = c(5.7, 5.4, # ITT GAD-7 Standard deviation 
                  6.3, 5.9, # ITT PHQ9 Depression
                  5.6, 5.6, # ITT PHQ-15
                  3.3, 3.2, # ITT working life
                  3.4, 3.4, # ITT social life
                  3.3, 3.2, # ITT Family life
                  5.1, 5.3, # ITT Physical
                  4.4, 4.6, # ITT Psychological
                  2.2, 2.6, # ITT Social
                  5.0, 5.3, # ITT Environment
                  
                  5.8, 5.3, # Per-protocol GAD-7 Anxiety 
                  6.5, 6.2, # Per-protocol PHQ9 Depression
                  5.6, 5.7, # Per-protocol PHQ-15
                  3.3, 2.7, # Per-protocol working life
                  3.3, 3.1, # Per-protocol social life
                  3.3, 2.8, # Per-protocol Family life
                  5.1, 5.3, # Per-protocol Physical 
                  4.4, 4.6, # Per-protocol Psychological
                  2.2, 2.6, # Per-protocol Social
                  5.0, 4.6), # Per-protocol Environment
  
  
  mean_diff_12months = m_12months - m_pre,
  
  )
  
cano_vindel2021
  