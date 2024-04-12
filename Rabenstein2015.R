# Dataextraction Rabenstein et al. 2015
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

# Based on table 2, p. 6
# Based on table 1, 2 and 3 in the same article 
Rabenstein2015 <- tibble(
  group = rep(c("Intervention", "Control"), each = 1, 16),
  
  outcome = rep(c(
    "Somatization", 
    "OCD", 
    "Insecurity", 
    "Depression",
    "Anxiety",
    "Aggressiveness",
    "Phobic Anxiety",
    "Paranoid Thinking",
    "Psychoticism",
    "GSI", 
    "BDI", 
    "Global",
    "Physical",
    "Mental", 
    "Social Relations", 
    "Environment"), each = 2,1
  ),
  
  
  
  N = rep(c(
    153, 148
  ), each = 1, 16),
  
  
  N_start = rep(c(
    170
  )),
  
  m_pre = c(
    8.11, 8.08,
    9.85, 12.04,
    5.58, 7.23,
    9.22, 10.45,
    7.82, 9.91,
    4.08, 5.45,
    5.15, 6.43,
    5.83, 7.24,
    4.74, 5.76,
    1.12, 1.48,
    23.39, 25.04,
    40.71, 42.87,
    50.78, 50.35,
    38.52, 37.9,
    53.31, 47.49,
    61.68, 63
  ),
  
  sd_pre = c(
    5.51, 5.79,
    5.45, 5.30,
    4.04, 3.67,
    6.08, 5.28,
    5.09, 5.20,
    3.61, 3.23,
    4.92, 4.83,
    4.35, 4.09,
    3.72, 3.87,
    0.68, 0.66,
    10.92, 9.83,
    22.38, 21.37,
    19.56, 15.96,
    19.55, 16.43,
    24.22, 23.85,
    18.62, 16.04
  ),
  
  m_post = c(
    4.49, 7.15,
    7.02, 10.25,
    4.06, 6.63,
    5.68, 9.87,
    4.45, 8.59,
    2.74, 4.15,
    2.91, 5.59,
    4.82, 6.25,
    3.12, 4.93,
    0.81, 1.30,
    15.24, 22.08,
    53.21, 46.1,
    59.43, 52.07,
    48.45, 38.49,
    58.23, 50.27,
    65.83, 65.83
  ),
  
  sd_post = c(
    4.46, 5.64,
    5.16, 5.63,
    3.53, 4.06,
    5.18, 5.63,
    4.36, 5.40,
    2.45, 3.18,
    3.67, 4.62,
    4.26, 4.51,
    3.25, 3.91,
    0.60, 0.67,
    10.78, 9.95,
    22.83, 20.44,
    21.37, 15.32,
    19.94, 17.32,
    23.77, 22.13,
    18.08, 17.12
  )
)

# Making it a wide format:

Rabenstein2015_wide <-
  Rabenstein2015 |> 
  mutate (group = case_match(
    group, "Intervention"  ~ "t", "Control" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )


# Effect size calculation:

Rabenstein2015_est <-           
  Rabenstein2015_wide |>
  mutate(
    analysis_plan = c(
      rep(c("Brief Symptom Inventory"), each = 10,1),
      "Beck Depression Inventory (BDI)",
      rep(c("WHO Quality of Life-BREF"), each = 5,1)),
    
    study = "Rabenstein et al. 2015"
    
    # There are also some effect sizes in the raw text on p. 7
    
  ) |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # The outcome BSI and BDI are reverted because lower score is beneficial
    m_post = if_else(analysis_plan %in% c("Brief Symptom Inventory",
                                         "Beck Depression Inventory (BDI)"), 
                    (m_post_c - m_post_t), # Revert the difference if BSI or BDI
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

  
  