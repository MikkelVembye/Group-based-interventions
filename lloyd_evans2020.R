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
                        each = 1,13)),
  
  outcome = as.factor(rep(c("DJG-total", 
                            "DJG-social",
                            "DJG-Emotional",
                            "GAD-7",
                            "WEMWBS",
                            "LSNS6",
                            "RGUK",
                            "ReQoL-10",
                            "EQ-VAS",
                            "days in acute",
                            "inpatient bed days",
                            "kept appointments",
                            "missed appointments")
                          ,
                          each = 2,1)
  ),
  
  n = rep(c(30,10),
          each = 1,13),
  
  firstq_pre = c(
    10, 9,
    5, 4,
    5, 5,
    15, 11,
    20, 24,
    4, 9,
    5, 8.8,
    4, 5,
    29, 30,
    0, 0,
    0, 0,
    3, 1,
    0, 0
  ), 
  
  median_pre = c(
    11, 10.5,
    5, 5,
    6, 6,
    19, 16,
    26.5, 30,
    7, 11.5,
    9.5, 13,
    9, 9.5,
    35, 47.5,
    0, 0,
    0, 0,
    6.5, 6.5,
    0, 0
  ),
  
  thirdq_pre = c(
    11, 11,
    5, 5,
    6, 6,
    21, 18,
    32, 34,
    9, 15,
    12, 18.3,
    14, 15,
    50, 50, 
    0,0,
    0,0,
    11, 17,
    1, 1
  ),
  
  firstq_post = c(
    8, 7,
    4, 4,
    4, 4,
    10.5, 11,
    23, 23,
    6, 6,
    6, 6.5,
    8, 10,
    30, 35,
    0,0,
    0,0,
    2, 1,
    0,0
  ),

  median_post = c(
    9, 10,
    5, 4,
    5, 6,
    14, 13.5,
    29.5, 31,
    7.5, 11,
    9, 13,
    14.5, 13.5,
    40, 52.5,
    0,0,
    0,0,
    3.5, 8,
    0, 1.5
    
  ),
  
  thirdq_post = c(
    11, 11,
    5, 5, 
    6, 6,
    17.5, 16, 
    34.5, 37,
    11, 15,
    12.3, 22.3,
    19, 19,
    60, 60,
    0,0,
    0,0,
    10, 10,
    1, 3
  )
  
) |> 
group_by(group, outcome) |> # calculating mean and SD based on the IQR information 
                            # using the Box-Cox method: bc.mean.sd 
  mutate(pre = bc.mean.sd(q1.val = firstq_pre, 
                         med.val = median_pre, 
                         q3.val = thirdq_pre,
                         n = n),
         post = bc.mean.sd(q1.val = firstq_post, 
                    med.val = median_post, 
                    q3.val = thirdq_post,
                    n = n)) |> 
  mutate(
    m_pre = pre$mean,
    sd_pre = pre$sd,
    m_post = post$mean,
    sd_post = post$sd
  ) |> 
  select(-pre, -post)  



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
  
  n = rep(c(30,10),
          each = 1,3),
  
  m_pre = c(
    21.6, 21.1,
    0.283, 0.400,
    0.472, 453
  ),
  
  sd_pre = c(
    5.3, 4.5,
    0.40, 0.24,
    9.6, 11.2
  ),
  m_post = c(
    16.4, 18.8,
    32.7, 38.4,
    36.9, 35
  ),
  
  sd_post = c(
    6.8, 4.8,
    0.33, 0.236,
    12.9, 14.4
  )
)


# Combing the obejct so i have the all the data from table 2 in one obejct
lloyd_Evans2020 <- lloyd_Evans2020_IQR %>% 
  full_join(lloyd_Evans2020_means); lloyd_Evans2020

