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
                        each = 1,12)),
  
  timing = rep(c("6w", 
                 "12w"), 
               each = 12,1),

  outcome = as.factor(rep(c("Job_search", 
                            "Help_seeking", 
                            "SISR",
                            "SSMIS_SF",
                            "CES-D", 
                            "BHS"  
                          #  "Secrecy"
                            ), 
                          each = 2,2 )
                      ),
  
  N = c(rep(c(
    18, 17), # 6 weeks 
     times = 6), 
        rep(c(
    
    20, 13), # 12 weeks
    times = 6)),
  
  N_start = rep(c(
    23, 19
  ), each = 1,12),
  
  m_pre = rep(c(
    3.1, 3.3, # Job-search self-efficacy (1–5)
    3.4, 3.0,  # Help-seeking intentions (1–7)
    15.3, 16.3, # Recovery (SISR: 4–24)
    17.3, 17.3,  # Self-stigma (SSMIS-SF: 5–45)
    39.2, 41.4, # Depressive symptoms (CES-D: 15–60)
    14.9, 14.1 # Hopelessness (BHS: 4–24)
#    4.0, 3.7   # Link’s 5-item Secrecy Scale
  ), each = 1,2),
  
  sd_pre = rep(c(
    0.8, 0.8,
    1, 1,
    4.1, 4.2,
    4.3, 8.1,
    7.4, 7.2,
    3.6, 4.5 
#    1.2, 1.6
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
#    3.5, 3.9,
    
    # 12 weeks 
    3.2, 3.2,
    3.3, 3.1,
    17.3, 15.7,
    15.4, 18.3,
    32, 37.5,
    12.3, 14.4
#    3.7, 4.4
    
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
#  1.3, 1.7,
  
  # 12 weeks
  1, 0.9, 
  0.6, 0.9,
  3.5, 2.6,
  7, 9.6,
  7.6, 9.8,
  4.2, 3
#  0.9, 1.4
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
  
  mutate(
    analysis_plan = rep(
      c(
        "6-item Job Search Self-Efficacy Scale",
        "item General Help-Seeking Questionnaire",
        "the 4-item part B of the Self-Identified Stage of Recovery Scale",
        "5-item self-concurrence subscale of the Self-Stigma of Mental Illness Scale-Short Form",
        "the 15-item German version of the Center for Epidemiologic Studies-Depression Scale",
        "short 4-item version of Beck’s Hopelessness Scale"
      #  "Link’s 5-item Secrecy Scale"
        
      ), each = 1,2),
    
    # ANCOVA estimates taken from table 1, p. 335
    F_val = c(
      # 6 weeks
      1.4, 0.2, 4.6, 10.8, 6.3, 3.4,
      # 12 weeks
      0.01, 0.04, 1.9, 1.5, 0.9, 2.4
    ),
    
    eta_sqrt = c(
      # 6 weeks
      0.04, 0.007, 0.13, 0.25, 0.17, 0.10,
      
      # 12 weeks
      0.01, 0.01, 0.06, 0.05, 0.03, 0.07
    ),
    
    p_val_f = c(
      # 6 weeks
      0.24, 0.64, 0.039, 0.08, 0.17, 0.08,
      
      # 12 weeks
      0.96, 0.85, 0.17, 0.23, 0.35, 0.14
      
    ),
      
      study = "Rüsch et al. 2019") |> 
      
      rowwise() |> 
      mutate(
        N_total = N_t + N_c,
        df_ind = N_total,
        

        m_post = case_when(
          outcome %in% c("Job_search", "Help_seeking", "SISR") ~ (m_post_t - m_post_c), # Higher is beneficial
          outcome %in% c("SSMIS_SF", "CES-D", "BHS") ~ (m_post_t - m_post_c) * -1), # Lower is beneficial
          
        sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
        
        d_post = m_post/sd_pool, 
        vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
        Wd_post = (1/N_t + 1/N_c),
        
        J = 1 - 3/(4*df_ind-1),
        
        g_post = J * d_post,
        vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
        Wg_post = Wd_post,
        
        vary_id = paste0(outcome, "/", timing)
      
      ) |> 
      ungroup()

      