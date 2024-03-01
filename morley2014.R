# Dataextraction from Morley (2014)
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

# Data from table 2 containing means and standard deviation on page 136

Morley2014 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,4)),
  
  outcome = as.factor(rep(c("BSS",
                            "HADS_depression",
                            "HADS_anxiety",
                            "SAS"), each = 2,1
  )),
  
  N = rep(c(
    122, 63
  ), each = 1,4),
  
  m_pre = c(
    9.5, 12.56, # Beck Scale for Suicide Ideation (range 0–38)
    9.3, 9.89,  # HAD Scale Depression (range 0–21)
    12.58, 12.35, # HAD Scale Anxiety (range 0–21)
    61.76, 65.09  # Self-Efficacy Scale (range 0–90)
  ),
  
  sd_pre = c(
    9.61, 8.55,
    6.66, 5.6, 
    6.66, 5.39,
    15.34, 12.55
  ),
  
  m_post = c(
    5.83, 6,
    6.4, 7.1,
    8.83, 9.71,
    65.31, 65.13
  ),
  
  sd_post = c(
    5.58, 6.61,
    5.43, 4.3,
    5.72, 4.3,
    12.49, 9.58
  ),
  
); Morley2014



# Turning dataset into wide format
Morley2014_wide <-
  Morley2014 |> 
  mutate (group = case_match(
    group, "Intervention"  ~ "t", 
    "Control" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )


# Effect size calculating 
Morley2014_est <-           
  Morley2014_wide |>
  mutate(
    analysis_plan =
      c("Suicide Ideation",
        "Hospital Anxiety and Depression Scale",
        "Hospital Anxiety and Depression Scale",
        "Self-Efficacy Scale"
      )
    
  ) |> 
  
  rowwise() |> 
  mutate(
    study = "Morley et al. 2014",
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # Reverting m_post for specific outcomes
    m_post = if_else(outcome %in% c("BSS", "HADS_depression", "HADs_anxiety"),
                     (m_post_t - m_post_c) * -1, 
                     m_post_t - m_post_c),
    
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
