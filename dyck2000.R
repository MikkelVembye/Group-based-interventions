#Data extraction from Dyck et al. (2000)
#Draft by Jakob

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

# Data from table 2 (p. 516) containing means and standard deviation

dyck2000 <- tibble(
  group = as.factor(c("Multiple-family group", 
                          "Standard care")),
  outcome = "MSANS", #The modified Scale for the Assessment of Negative Symptoms
  N = 21,
  
  m_pre = c(
    7.9, 8.7
  ),
  
  m_post = c(
#    7.4, 9.1, # 3 months
#    7.2, 8.9, # 6 months
#    7.2, 8.9, # 9 months
    7.2, 8.4 # 12 months, the real post-treatment measure because the treatment
             # ends after 12 months.
  ),
  
  
  sd_pre = c(
    3.1, 3.3 
  ),
  
  sd_post = c(
#    2.3, 3.2, # 3 months
#    2.1, 2.7, # 6 months
#    2.1, 3,   # 9 months
    2, 3.1    # 12 months, the real post-treatment measure because the treatment
              # ends after 12 months.
  )
)



# Making the dyck2000 tibble wide in order to estimate the effect sizes and
# further analysis. 
dyck2000_wide <-
  dyck2000 |> 
  mutate (group = case_match(
    group, "Multiple-family group"  ~ "t", "Standard care" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )

# Effect size calculating 
dyck2000_est <-           
  dyck2000_wide |>
  mutate(
    analysis_plan = "The modified Scale for the Assessment of Negative Symptoms" 
    )|> 
  rowwise() |> 
  mutate(
    study = "Dyck et al. 2000",
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    m_post =  m_post_t - m_post_c,
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/df_ind,
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/df_ind,
    Wg_post = Wd_post,
    
  ) |> 
  ungroup()


