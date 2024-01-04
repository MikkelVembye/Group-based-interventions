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

# Data from table 2 (p. 1186) provides mean and standard deviation. Table also 
# indcludes p-values indicating "Test of significance between the two groups 
# using repeated-measures analysis with PROC MIXED " (Sajatovic et al. 2009: 1186)


sajatovic2009  <- tibble(
  group = rep(c("Life Goals Program", # Intervention
                "Treatment as usual"), # Control
              each = 1,9),
  
  outcome = rep(c("HAM-D", # Hamilton Depression Rating Scale
                   "YMRS", # Young Mania Rating Scale
                   "GAS"), # Global Assessment Scale
                 each = 2,3),
  
  timing = rep(c("3m", "6m", "12m"), each = 6),
  
  N = c(
        # 3 months
        63, 65,
        63, 65,
        61, 61,
        
        # 6 months
        51, 55,
        51, 55,
        46, 53,
        
        # 12 months 
        41, 39, 
        41, 39, 
        40, 39
        ),
  
  m_pre = rep(c(
    19.98, 17.08, # HAM-D
    7.3, 7.58,    # YMRS
    56.53, 58.22  # GAS
    
  ), each =  1,3),
  
  
  sd_pre = rep(c(
    11.45, 10.99,
    5.41, 5.44,
    12.43, 12.00
    
  ), each = 1, 3),
  
  
  m_post = c(
    # 3 months
    16.30, 16.35,
    6.14, 15.85,
    60.10, 59.05,
    
    # 6 months
    16.35, 15.96,
    6.78, 7.69,
    61.72, 62.19, 
    
    # 12 months
    16.02, 14.39,
    5.85, 7.15,
    63.70, 64.51
  ), 
  
  
  sd_post = c(
    # 3 months
    9.68, 10.52,
    4.85, 5.38,
    11.63, 12.44, 
    
    # 6 months
    10.18, 12.47,
    5.36, 6.26,
    12.76, 14.42,
    
    # 12 months
    11.73, 10.87,
    4.74, 5.60,
    12.66, 15.90
  )
  
)


# Making the sajatovic2009 tibble wide in order to estimate the effect sizes and
# further analysis. 

# Turning data into wide format

# Turning data into wide format
params <- tibble(
  filter_val1 = rep(c(paste0(c(3, 6, 12), "m")), 1)
)

wide_sajatovic2009_func <- 
  function(filter_val1){
    
    sajatovic2009 |> 
      filter(timing == filter_val1) |> 
      mutate(group = case_match(group, "Life Goals Program" ~ "t", "Treatment as usual" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }



sajatovic2009_est <- 
  pmap(params, wide_sajatovic2009_func) |>
  list_rbind() |> 
  mutate(
    # P-values obtained from table 2, calculated with repeated-measures analysis with 
    # PROC MIXED in SAS (p. 1186) - 
    p_val = rep(c(0.40, 0.22, 0.97), each = 1,3), # Should be checked

   analysis_plan = rep(
    c("Hamilton Depression Rating Scale (HAM-D)", # HAM-D
      "Young Mania Rating Scale (YMRS)", # YMRS
      " Functional impairments caused by illness were assessed with the Global Assessment Scale (GAS)" # GAS
  ), each = 1,3),

  study = "Sajatovic et al. 2009"

) |> 
  
  rowwise() |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # For YMRS and HAM-D lower scores are beneficial why these is reverted
    m_post = if_else(outcome != "GAS", (m_post_t - m_post_c)*-1,
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
  


