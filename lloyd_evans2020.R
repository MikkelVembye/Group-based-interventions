# Dataextraction from Lloyd-Evans (2020)
# Attempt from Jakob

# Loading the relevant package herein Tidyverse 
library(tidyverse)
library(estmeansd)


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

# From table 2 on page 9

# Creating a tibble containing data with IQR, and later i will estimate the mean 
# and SD from this information using bc.mean.sd (Box-Cox method) 

lloyd_Evans2020_IQR <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,9)),
  
  outcome = as.factor(rep(c("DJG-total", 
                            #"DJG-social", removed
                            "DJG-Emotional", 
                            "GAD-7",
                            "WEMWBS", 
                            "LSNS6", 
                            "RGUK", 
                            "ReQoL-10", 
                            "EQ-VAS", 
                            #"days in acute", removed
                            #"inpatient bed days", removed
                            "kept appointments"
                            #"missed appointments" removed
                            
# DJG-social, days-in-acute, inpatient bed days, missed appointments: are all 
# removed because there was not enough variance between the IQR to get a an estimate
# Their numbers are therefore also removed further down. 
                            
                            
                          ),
                          each = 2,1)
  ),
  
  n_pre = rep(c(30,10),
          each = 1,9),
  
  firstq_pre = c(
    10, 9,   #  Loneliness: De Jong-Gierveld (DJG) Loneliness Scale
    #5, 4,
    5, 5,    # DJG: Emotional Loneliness subscale score
    15, 11,  # Generalized Anxiety Disorder Questionnaire (GAD-7)
    20, 24,  # Warwick-Edinburgh Mental Well-being Scale (WEMWBS) 
    4, 9,    # Social network: Lubben Social Network scale (LSNS6)
    5, 8.8,  # Perceived social capital: Resource Generator UK (RGUK)
    4, 5,    # Recovering Quality of Life Questionnaire (ReQoL-10)
    29, 30,  # Self-rated health using EQ-Visual Analogue Scale (EQ VAS)
    #0, 0,
    #0, 0,
    3, 1     # Community service kept appointments
    #0, 0
  ), 
  
  median_pre = c(
    11, 10.5,
    #5, 5,
    6, 6,
    19, 16,
    26.5, 30,
    7, 11.5,
    9.5, 13,
    9, 9.5,
    35, 47.5,
    #0, 0,
    #0, 0,
    6.5, 6.5
    #0, 0
  ),
  
  thirdq_pre = c(
    11, 11,
    #5, 5,
    6, 6,
    21, 18,
    32, 34,
    9, 15,
    12, 18.3,
    14, 15,
    50, 50, 
    #0,0,
    #0,0,
    11, 17
    #1, 1
  ),
  
  n_post = rep(c(25,10),
              each = 1,9),
  
  firstq_post = c(
    8, 7,
    #4, 4,
    4, 4,
    10.5, 11,
    23, 23,
    6, 6,
    6, 6.5,
    8, 10,
    30, 35,
    #0,0,
    #0,0,
    2, 1
    #0,0
  ),

  median_post = c(
    9, 10,
    #5, 4,
    5, 6,
    14, 13.5,
    29.5, 31,
    7.5, 11,
    9, 13,
    14.5, 13.5,
    40, 52.5,
    #0,0,
    #0,0,
    3.5, 8
    #0, 1.5
    
  ),
  
  thirdq_post = c(
    11, 11,
    #5, 5, 
    6, 6,
    17.5, 16, 
    34.5, 37,
    11, 15,
    12.3, 22.3,
    19, 19,
    60, 60,
    #0,0,
    #0,0,
    10, 10
    #1, 3
  )
) |> 
  rowwise() |> 
  mutate (pre = list(bc.mean.sd(
    q1.val = firstq_pre,
    med.val = median_pre,
    q3.val = thirdq_pre,
    n = n_pre
  )[c("est.mean", "est.sd")])) |> 
  unnest_wider(pre, names_sep = "_") |> 
  rowwise() |> 
  mutate(post = list(bc.mean.sd(
    q1.val = firstq_post,
    med.val = median_post,
    q3.val = thirdq_post,
    n = n_post
  )[c("est.mean", "est.sd")]))|> 
  unnest_wider(post, names_sep = "_") |> 
  rename(
    m_pre = pre_est.mean,
    sd_pre = pre_est.sd,
    m_post = post_est.mean,
    sd_post =  post_est.sd); lloyd_Evans2020_IQR
  

# Creating a tibble with the rest of the data which is measured by mean and SD
lloyd_Evans2020_means <- tibble(          
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,3)),
  
  outcome = as.factor(rep(c("PHQ-9",
                            "TBD",
                            "EQ-5D-5L"),
  each = 2,1)
  ),
  
  n_pre = rep(c(30,10),
          each = 1,3),
  
  m_pre = c(
    21.6, 21.1,  # Depression: Patient Health Questionnaire (PHQ-9)
    32.7, 38.4,  # Activity: Time Budget Diary (TBD)
    0.283, 0.400 # EuroQol Health Questionnaire (EQ-5D-5L) Index value
  ),
  
  sd_pre = c(
    5.3, 4.5,
    9.6, 11.2,
    0.40, 0.24
  ),
  
  n_post = rep(c(25,10),
               each = 1,3),
  
  m_post = c(
    16.4, 18.8,
    36.9, 35,
    0.472, 0.453
  ),
  
  sd_post = c(
    6.8, 4.8,
    12.9, 14.4,
    0.33, 0.236
    
  )
); lloyd_Evans2020_means


# Combing the obejct so i have the all the data from table 2 in one obejct
lloyd_Evans2020 <- lloyd_Evans2020_IQR|> 
  full_join(lloyd_Evans2020_means); lloyd_Evans2020


