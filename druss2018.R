# Dataextraction from Druss (2018)
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

# Data from table 2 (p. 532), containing means and SD for health related quality of life 
# (PCS and MCS),and table 3 (p. 533), containing means and SD for RAS

druss2018 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,6)),
  
  timing = rep(c("3m", "6m"), each = 6,1),
  
  outcome = as.factor(rep(c("RAS", # Recovery Assessment Scale
                  # Both of the below measures is related to health related quality of life
                  # as measured by the Short-Form Health Survey (SF-36) (see p. 531)
                        "PCS", # The physical component summary
                        "MCS"), # The mental component summary
                        each= 2,2)),
  
  N = rep(c(198, 202), each = 1,6),
 
 
 m_pre = rep(c(
   3.72, 3.66,   # RAS
   32.73, 32.74, # PCS
   32.05, 32.04) # MCS
 , each = 1,2)
 ,
  
  sd_pre = rep(c(
    0.62, 0.57,    # RAS
    10.92, 11.29,  # PCS
    11.79, 11.36), # MCS
    each = 1,2) 
 ,
  
  m_post = c(
    3.89, 3.70,   # RAS 3 months
    34.49, 33.89, # PCS 3 months
    34.49, 34.25, # MCS 3 months
    
    3.87, 3.74,   # RAS 6 months
    35.42, 34.25, # PCS 6 months
    36.64, 34.54  # MCS 6 months
  ),
  
  sd_post = c(
    0.55, 0.68,   # RAS 3 months
    11.15, 10.41, # PCS 3 months
    11.15, 11.97, # MCS 3 months
    
    0.61, 0.59,   # RAS 6 months
    11.02, 11.52, # PCS 6 months
    12.28, 11.82  # MCS 6 months
  )
)

# Making the druss2018 tibble wide in order to estimate the effect sizes and
# further analysis. 

# Turning data into wide format
params <- tibble(
  filter_val1 = rep(c(paste0(c(3, 6), "m")), 2)
)

wide_druss2018_func <- 
  function(filter_val1){
    
    druss2018 |> 
      filter(timing == filter_val1) |> 
      mutate(group = case_match(group, "Intervention" ~ "t", "Control" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }

  
druss2018_est <- 
  pmap(params, wide_druss2018_func) |>
  list_rbind() |>
# Based on the degrees of freedom value reported in Druss et al. 2018 table 2 and 3
  mutate(
    p_val_f = rep(c(0.02, 0.046, 0.039), each = 1,4),
    df1 = 2,
    df2 = rep(c(704, 697, 697), each = 1,4), 
    F_val = rep(c(3.79, 3.09, 3.25), each = 1,4),
    analysis_plan = rep(
    c("Patient activation measureMorisky scale Recovery assessment scale (RAS)", # RAS
      "Short-Form Health Survey (SF-36)= Quality of life", # PCS & MCS
      "Short-Form Health Survey (SF-36)= Quality of life" # PCS & MCS
    ), each = 1,4),
    
    study = "Druss et al. 2018"
    
 ) |> 
  rowwise() |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    m_post =  (m_post_t - m_post_c),
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


