# Dataextraction from Wojtalik et al. (2022)
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


# Extracting data from online supplement in order to get specific measures instead 
# of a composite score. This is taken from the table on page 5-6. 






# Data from table 2 and table (p. 505)  containing marginal means with standard error
# It also contains standard error. To calculate pre-post test we will use the standard 
# deviation from table 1 (p. 504)

wojtalik2019 <- tibble( 
  group = as.factor(rep(c("Cognitive enhancement therapy (CET)", # Intervention
                "Enriched supportive therapy (EST)"), # Control
              each = 1,12)),
  
  analysis = rep(c("ITT", "Completers"), each = 12),
  
  
  timing = rep(c("9m", 
                 "18m"), 
               each = 6, 2),
  
  outcome = as.factor(rep(c("Cognition composite",
                  "Social adjustment composite",
                  "Symptoms composite"), 
                each = 2,4)),
  
  N = c(rep(c(58, 44), each = 1,6),
        rep(c(26, 23),each = 1,6)),
  
  me_pre = c(rep(c(
    # ITT
    34.4, 38,  # Baseline Cognition composite 
    49.5, 51.0,# Baseline Social adjustment composite
    40.1, 50.4), # Baseline Symptoms composite
    each = 1,2),
    
    # Completers
    rep(c(
    34.6, 39.4, # Baseline Cognition composite
    47.1, 50.4, # Baseline Social adjustment composite
    49.4, 50.2 # Baseline Symptoms composite
  ), each = 1,2)
  ),
  
  # Taken from table 1 (p. 504)
  sd_pre = c(rep(c(
    # ITT
    11.3, 11.8, # Baseline Cognition composite  
    9.6, 10.5,  # Baseline Social adjustment composite
    10.4, 10.0), each = 1,2),  # Baseline Symptoms composite
    
    # Completer (taken from online supplements)
    rep(c(12.52, 12.52,
    8.80, 8.80,
    9.18, 9.18
  ), each = 1,2)),
  
  se_pre = rep(c(
    # ITT
    1.5, 1.7,  # Baseline Cognition composite 
    1.3, 1.4,  # Baseline Social adjustment composite
    1.3, 1.5,   # Baseline Symptoms composite
    
    # Completer
    2.4, 2.6,
    1.8, 1.8,
    1.9, 2.0
    
  ), each = 1,2),
  
  me_post = c(
    # ITT
    38.4, 37.1, # 9 months Cognition composite
    54.1, 56.8, # 9 months Social adjustment composite
    53.5, 53.4, # 9 months Symptoms composite
    
    40.1, 39.8, # 18 months Cognition composite
    56.3, 55.5, # 18 months Social adjustment composite
    54.3, 53.5,  # 18 months Symptoms composite
    
    # Completer
    40.0, 37.5, # 9 months Cognition composite
    52.2, 54.9, # 9 months Social adjustment composite
    53.2, 52.8, # 9 months Symptoms composite
    
    41.3, 40.6, # 18 months Cognition composite
    54.5, 53.4, # 18 months Social adjustment composite
    54.0, 53.1  # 18 months Symptoms composite
    
  ),
  
  se_post = c(
    # ITT
    1.6, 1.9, # 9 months Cognition composite
    1.4, 1.7, # 9 months Social adjustment composite
    1.4, 1.7, # 9 months Symptoms composite
    
    1.8, 2.0, # 18 months Cognition composite
    2.0, 2.3, # 18 months Social adjustment composite
    1.8, 1.9,  # 18 months Symptoms composite
    
    # Completer
    2.4, 2.6, # 9 months Cognition composite
    1.9, 2.0, # 9 months Social adjustment composite
    1.8, 1.9, # 9 months Symptoms composite
    
    2.5, 2.6, # 18 months Cognition composite
    2.4, 2.5, # 18 months Social adjustment composite
    2.0, 2.1  # 18 months Symptoms composite
    
  )
  
)


# Making the wojtalik2019 tibble wide in order to estimate the effect sizes and
# further analysis. 

# Turning data into wide format
params <- tibble(
  filter_val1 = rep(unique(wojtalik2019$analysis), each = 2),
  filter_val2 = rep(c(paste0(c(9, 18), "m")), 2)
)


wide_wojtalik2019_func <- 
  function(filter_val1, filter_val2){
    
    wojtalik2019 |> 
      filter(analysis == filter_val1 & timing == filter_val2) |> 
      mutate(group = case_match(group, "Cognitive enhancement therapy (CET)" ~ "t", 
                                "Enriched supportive therapy (EST)" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }


wojtalik2019_est <- 
  pmap(params, wide_wojtalik2019_func) |>
  list_rbind() |> 
  
  mutate(
    
    # analyse_plan()
    
    study = "Wojtalik et al. 2019", 
    
    t_val = c(
      # ITT
      2.81, # 9 months Cognition composite
      -0.78, # 9 months Social adjustment composite
      0.20, # 9 months Symptoms composite
      
      2.04, # 18 months Cognition composite
      0.80, # 18 months Social adjustment composite
      0.43,  # 18 months Symptoms composite
      
      # Completer
      3.44, # 9 months Cognition composite
      0.32, # 9 months Social adjustment composite
      0.51, # 9 months Symptoms composite
      
      2.58, # 18 months Cognition composite
      1.60, # 18 months Social adjustment composite
      0.60  # 18 months Symptoms composite
      
    ),
    
    df = c(
      # ITT
      106,  # 9 months Cognition composite
      107,  # 9 months Social adjustment composite
      107,  # 9 months Symptoms composite
      
      106, # 18 months Cognition composite
      107, # 18 months Social adjustment composite
      107, # 18 months Symptoms composite
      
      # Completer
      88, # 9 months Cognition composite
      91, # 9 months Social adjustment composite
      91, # 9 months Symptoms composite
      
      88, # 18 months Cognition composite
      91, # 18 months Social adjustment composite
      91  # 18 months Symptoms composite
      
    ),
    
    p_values_t = c(
      # ITT
      0.003,  # 9 months Cognition composite
      0.219,  # 9 months Social adjustment composite
      0.421,  # 9 months Symptoms composite
      
      0.022, # 18 months Cognition composite
      0.214, # 18 months Social adjustment composite
      0.335,  # 18 months Symptoms composite
      
      # Completer
      0.001,  # 9 months Cognition composite
      0.376,  # 9 months Social adjustment composite
      0.305,  # 9 months Symptoms composite
      
      0.006, # 18 months Cognition composite
      0.057, # 18 months Social adjustment composite
      0.275  # 18 months Symptoms composite
      
    ),
    
    d = c(
      # ITT
      0.44,  # 9 months Cognition composite
      -0.14, # 9 months Social adjustment composite
      0.04,  # 9 months Symptoms composite
      
      0.35, # 18 months Cognition composite
      0.23, # 18 months Social adjustment composite
      0.11, # 18 months Symptoms composite
      
      # Completer
      0.59, # 9 months Cognition composite
      0.07, # 9 months Social adjustment composite
      0.12, # 9 months Symptoms composite
      
      0.45, # 18 months Cognition composite
      0.51, # 18 months Social adjustment composite
      0.18  # 18 months Symptoms composite
      
    )
  ) |> 
  rowwise() |> 
  mutate(
    study = "Wojtalik et al. 2020",
    N_total = N_t + N_c,
    df_ind = N_total,
    
    
    # m_post og sd_pool skal regnes efter
    
    m_post = if_else(analysis == "ITT",
                     
                     # ((T_post1-T_pre0) - (C_post1 - C_pre0)) / SD_basline 
                     
                                                                      # This is done to get the combined SD
                     (me_post_t - me_pre_t - me_post_c + me_pre_c) / sqrt((N_t * sd_pre_t^2 + N_c * sd_pre_c^2) / (N_t + N_c)),
                     
                     (me_post_t - me_pre_t - me_post_c + me_pre_c) / sd_pre_c
    ),
                      # E.14 (WWC Handbook 5, p. 168)
    sd_pool = if_else(analysis == "ITT",
                      #ITT                                  # To get the combined SD           
                      sqrt((N_total - 1) / (N_total - 2) * (N_t * sd_pre_t^2 + N_c * sd_pre_c^2) / (N_t + N_c) - (N_t * N_c) / (N_total * (N_total - 2)) * (me_pre_t - me_pre_c)^2),
                      # Completers
                      sqrt((N_total - 1) / (N_total - 2) * sd_pre_c^2 - (N_t * N_c) / (N_total * (N_total - 2)) * (me_pre_t - me_pre_c)^2)
      ),
    
    
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post
  )|> 
  ungroup ()







