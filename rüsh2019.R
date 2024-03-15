# Dataextraction from Rüsch (2019)
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

# Data from table 1 containing means and standard deviation on page 335
# The table also contains partial η2 values, p-values and F-test

Rüsch2019 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,14)),
  
  timing = rep(c("6w", 
                 "12w"), 
               each = 14,1),

  outcome = as.factor(rep(c("Job_search", 
                            "Help_seeking", 
                            "SISR",
                            "SSMIS_SF",
                            "CES-D", 
                            "BHS",  
                            "Secrecy"), 
                          each = 2,2 )
                      ),
  
  N = c(rep(c(
    18, 17), # 6 weeks 
     times = 7), 
        rep(c(
    
    20, 13), # 12 weeks
    times = 7)),
  
  N_start = rep(c(
    23, 19
  ), each = 1,14),
  
  m_pre = rep(c(
    3.1, 3.3, # Job-search self-efficacy (1–5)
    3.4, 3.0,  # Help-seeking intentions (1–7)
    15.3, 16.3, # Recovery (SISR: 4–24)
    17.3, 17.3,  # Self-stigma (SSMIS-SF: 5–45)
    39.2, 41.4, # Depressive symptoms (CES-D: 15–60)
    14.9, 14.1, # Hopelessness (BHS: 4–24)
    4.0, 3.7   # Link’s 5-item Secrecy Scale
  ), each = 1,2),
  
  sd_pre = rep(c(
    0.8, 0.8,
    1, 1,
    4.1, 4.2,
    4.3, 8.1,
    7.4, 7.2,
    3.6, 4.5, 
    1.2, 1.6
  ), each = 1,2),
  
  m_post = c(
    # 3 Weeks
  #  3.2, 3.2, 
  #  3.6, 3,
  #  16.1, 15.9, 
  #  15.3, 16.9, 
  #  35.2, 39.1,
  #  13.8, 13.7,
  #  3.8, 3.7
    
  # 6 weeks
    3.4, 3.3, 
    3.5, 3.2, 
    17.8, 16.1, 
    14.6, 19.1, 
    31.5, 40.1, 
    12.6, 14.2, 
    3.5, 3.9,
    
    # 12 weeks 
    3.2, 3.2,
    3.3, 3.1,
    17.3, 15.7,
    15.4, 18.3,
    32, 37.5,
    12.3, 14.4,
    3.7, 4.4
    
  ),
  
 
sd_post = c(
  # 3 weeks
  # 0.7, 0.9, 
  # 1, 0.9, 
  # 4.2, 3.6, 
  # 6.2, 8, 
  # 8.8, 10.2, 
  # 3.9, 4.6, 
  # 1, 1.7
  
  # 6 weeks
  0.8, 0.9,
  1, 1, 
  3.4, 4.4,
  7.9, 8.7,
  8.5, 10.1,
  3.6, 5, 
  1.3, 1.7,
  
  # 12 weeks
  1, 0.9, 
  0.6, 0.9,
  3.5, 2.6,
  7, 9.6,
  7.6, 9.8,
  4.2, 3,
  0.9, 1.4
 )
); Rüsch2019



# Turning data into wide format
params <- tibble(
  filter_val1 = rep(c(paste0(c(6, 12), "w")), 1)
)

wide_Rüsch2019_func <- 
  function(filter_val1){
    
    Rüsch2019 |> 
      filter(timing == filter_val1) |> 
      mutate(group = case_match(group, "Intervention" ~ "t", "Control" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }


Rüsch2019_est <- 
  pmap(params, wide_Rüsch2019_func) |>
  list_rbind() |>
  # Based on the degrees of freedom value reported in Druss et al. 2018 table 2 and 3
  mutate(
    analysis_plan = rep(
      c(
        "Positive And Negative Syndromes Scale (PANSS)", # PANSS
        "Positive And Negative Syndromes Scale (PANSS)", # PANSS
        "Positive And Negative Syndromes Scale (PANSS)", # PANSS
        "Hamilton Depression Rating Scale (HAM-D)", # HAM-D
        "Health-related quality of life, with the Quality of Well-Being (QWB) Scale" # QWB
      )
