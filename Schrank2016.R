# Dataextraction Schrank et al. 2016
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

# Based on table 1, 2 and 3 in the same article 
Schrank2016 <- tibble(
  group = rep(c("Intervention", "Control"), each = 1, 8),
  
  outcome = rep(c(
    "WEMWBS", 
    "MANSA", 
    "PPI", 
    "BPRS",
    "SDHS",
    "IHS",
    "RSES", 
    "RES"
  ), each = 2,1),
  
  
  N = rep(c(
    41, 43
  ), each = 1, 8),
  
 m_pre = c(3.19, 3.00,
  4.05, 4.14,
  3.58, 3.44,
  30.70, 33.57,
  2.29, 2.48,
  4.02, 3.72, 
  2.24, 2.09,
  2.74, 2.71),
 
sd_pre = c(
  0.76, 0.89,
  0.85, 1.01,
  0.73, 0.80,
  8.81, 8.42,
  0.69, 0.76,
  0.79, 0.85,
  0.64, 0.66,
  0.32, 0.32
),

m_post = c(
  3.36,3.24,
  4.42, 4.21,
  3.72, 3.48,
  29.37, 33.23,
  2.13, 3.34,
  4.11, 4.04,
  2.37, 2.21,
  2.8,2.73
),

se_post = c(
  0.10, 0.10,
  0.10, 0.10,
  0.07, 0.07,
  0.96, 0.98,
  0.08, 0.09,
  0.1, 0.1,
  0.07, 0.07,
  0.04,0.04) 
) |> 
  mutate(sd_post = se_post * sqrt(N))




Schrank2016_wide <-
  Schrank2016 |> 
  mutate (group = case_match(
    group, "Intervention"  ~ "t", "Control" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )



Schrank2016_est <-           
  Schrank2016_wide |>
  mutate(
    analysis_plan = rep(c(
      "Warwick-Edinburgh Mental Well-Being Scale (WEMWBS)",
      "Manchester Short Assessment (MANSA)",
      "Positive psychotherapy inventory (PPI)",
      "Brief Psychiatric Rating Scale (BPRS)",
      "The Short Depression-Happiness Scale (SDHS)",
      "The Integrative Hope Scale (IHS)",
      "Rosenberg Self-Esteem Scale (RSES)",
      "Rogers Empowerment Scale (RES)"
    )
  )
) |> 
  rowwise() |> 
  mutate(
    study = "Schrank et al. 2016",
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # The outcome BPRS is reverted because lower score is beneficial
    m_post = if_else(analysis_plan != "Brief Psychiatric Rating Scale (BPRS)", 
                     m_post_t - m_post_c,
                     (m_post_t - m_post_c) * -1),
    
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
    vary_id = outcome
    
  ) |> 
  ungroup()

  