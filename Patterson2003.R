# Dataextraction from Patterson et al. (2003)
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

# Data from table 3 (p.21)  containing means, standard deviation. It also contains
# Time X Interaction coefficients with p-values (estimated with ANOVA) 


patterson2003 <- tibble(
  group = as.factor(rep(c("FAST Intervention", 
                          "Treatment-as-Usual"), 
                        each = 1, 10)),
  
  timing = rep(c("3m", 
                 "6m"), 
               each = 10,1),
 
   outcome = as.factor(rep(c(
#    "UPSA", 
    "PANSS_Positive", 
    "PANSS_Negative", 
    "PANSS_General", 
    "HAM_D", 
    "QWB"), each = 2, 2)),
 
   N = 16,
  
  m_pre = rep(c(
#    31.9, 41.5,   # UPSA
    12.5, 13.0,   # PANSS Positive
    16.9, 13.0,   # PANSS Negative
    25.0, 21.9,   # PANSS General
    7.8, 7.7,     # Ham-D
    0.53, 0.55    # QWB
  ), each = 1,2),
  
  sd_pre = rep(c(
#    11.8, 8.3,    # UPSA
    5.6, 4,       # PANSS Positive
    6.6, 3.2,     # PANSS Negative
    6.2, 3.7,     # PANSS General
    6.1, 2.8,     # Ham-D
    0.08, 0.08    # QWB
  ), each = 1,2),
  
  
  m_post = c(
#    41.5, 40.3, # UPSA           3 months
    13.0, 10.4, # PANSS Positive 3 months
    13.0, 10.1, # PANSS Negative 3 months
    21.9, 22.3, # PANSS General  3 months
    7.7, 4.9,   # Ham-D          3 months
    0.55, 0.49, # QWB            3 months
    
#    42.7, 41.6,  # UPSA           6 months
    12.13, 13.1, # PANSS Positive 6 months 
    14.3, 12.4,  # PANSS Negative 6 months
    23.9, 23.9,  # PANSS General  6 months
    7.2, 7.9,    # Ham-D          6 months
    0.51, 0.49   # QWB            6 months
  
  ),

  sd_post = c(
#    9.5, 7.1,     # UPSA           3 months
    6.6, 6.3,     # PANSS Positive 3 months
    5.1, 4,       # PANSS Negative 3 months
    4.7, 4.9,     # PANSS General  3 months
    5.2, 5.3,     # Ham-D          3 months
    0.10, 0.07,   # QWB           3 months
    
    
   
#    9.7, 9.7,     # UPSA           6 months
    4.3, 5.9,     # PANSS Positive 6 months
    6, 4.4,       # PANSS Negative 6 months
    4.6, 4.4,     # PANSS General  6 months
    4.9, 5,       # Ham-D          6 months
    0.07, 0.07    # QWB            6 months
   )
)


# Making the patterson2003 tibble wide in order to estimate the effect sizes and
# further analysis. 

# Turning data into wide format
params <- tibble(
  filter_val1 = rep(c(paste0(c(3, 6), "m")), 1)
)

wide_patterson2003_func <- 
  function(filter_val1){
    
    patterson2003 |> 
      filter(timing == filter_val1) |> 
      mutate(group = case_match(group, "FAST Intervention" ~ "t", "Treatment-as-Usual" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }



patterson2003_est <- 
  pmap(params, wide_patterson2003_func) |>
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
      ), each = 1,2),
    
    study = "Patterson et al. 2003") |> 
  
  rowwise() |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    
    # For PANSS and HAM-D lower scores are beneficial why these is reverted
    m_post = if_else(outcome != "QWB", (m_post_t - m_post_c)*-1,
                     m_post_t - m_post_c),
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
  ) |> 
  ungroup()
