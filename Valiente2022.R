# Dataextraction from Valiente et al. (2022)
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

# Data from table 3 and table (p. 4)  containing means, standard deviation. It also contains
# difference Morris' d and p

valiente2022  <- tibble(
  group = rep(c("Multicomponent PPI", # Intervention
                "Treatment as usual"), # Control
              each = 1,16),
  
  outcome = rep(c(
                  # Table 3
                  "SPWB Autonomy",          #Scales of Psychological Well-Being (SPWB)
                  "SPWB Positive_relationship", 
                  "SPWB Self_acceptance",
                  "SPWB Enviormental mastery",
                  "SPWB Purpose_in_life",
                  "SPWB Personal_Growth",
                  "SWLS",                  # Satisfaction With Life Scale (SWLS)
                  
                  # Table 4
                  "Somatization",         # Symptom checklist-90-Revised (SCL-90-R)
                  "Obsessive–compulsive", 
                  "Interpersonal sensibility",
                  "Depression", 
                  "Anxiety", 
                  "Hostility", 
                  "Phobic anxiety",
                  "Paranoid ideation", 
                  "Psychoticism"
                  
                  ), 
                each = 2,1),
  
  N = rep(c(52,61),
          each = 1,16),
  
  
  m_pre = c(
    
    # Table 3
    19.5, 20.1,
    34.7, 33.9,
    29.9, 31.6,
    30.6, 31.4,
    34.4, 34.4,
    35.3, 35.7,
    17.4, 18.2,
   
    # Table 4
    1.00, 1.02,
    1.66, 1.62,
    1.42, 1.41,
    1.51, 1.47,
    1.12, 1.31,
    0.75, 0.63,
    0.89, 1.09,
    1.15, 1.15,
    1.14, 0.99
    
  ),
  
  sd_pre = c(
    # Table 3
    4.4,  4.2,
    8.0, 7.5,
    8.7, 7.8,
    7.8, 7.3,
    7.7, 5.9,
    7.4, 6.8,
    7.1, 6.8,
    
    # Table 4
    0.83, 0.81,
    0.87, 0.93,
    0.81, 0.95,
    0.87, 0.89,
    0.82, 0.97,
    0.83, 0.80,
    0.77, 1.01,
    0.86, 0.86,
    0.82, 0.83
    
  ),
        
 
  
  m_post = c(
    # Table 3
    19.9, 19.7,
    35.7, 34.4,
    31.7, 31.1,
    32.7, 30.8,
    34.9, 34.2,
    35.5, 35.5,
    19.0, 19.2,
    
    # Table 4
    0.90, 0.96,
    1.50, 1.48,
    1.28, 1.27,
    1.30, 1.40,
    1.07, 1.14,
    0.71, 0.61,
    0.91, 1.07,
    1.18, 1.21,
    0.99, 0.98
  ),
  
  sd_post = c(
    # Table 3
    3.4, 4.2, 
    7.1, 7.6, 
    7.5, 7.8, 
    7.5, 7.3, 
    6.9, 5.9,
    6.8, 6.8,
    6.6, 6.8,
    
    # Table 4
    0.82, 0.84,
    0.86, 0.96,
    0.79, 0.89,
    0.80, 0.94,
    0.84, 0.91,
    0.76, 0.79,
    0.80, 0.97,
    0.90, 0.87,
    0.85, 0.86
  )
    
)

# Making the tibble wide 

valiente2022_wide <-
  valiente2022 |> 
  mutate (group = case_match(
    group, "Multicomponent PPI"  ~ "t", "Treatment as usual" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )
  
# Effect sizes: Estimating G and D (pre-post test) (Not finished yest)
valiente2022_est <-           
  valiente2022_wide |>
  mutate(
    analysis_plan = rep(c(
                      "Scales of Psychological Well-Being (SPWB)",
                      "Satisfaction With Life Scale (SWLS)",
                      "Symptom checklist-90-Revised (SCL-90-R)"
                      ), c(6,1,9))
    
    
  )|> 
  rowwise() |> 
  mutate(
    study = "Valiente et al. 2022",
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # The outcome SCL-90-R reverted because lower score is beneficial
    m_post = if_else(analysis_plan != "Symptom checklist-90-Revised (SCL-90-R)", 
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
    
  ) |> 
  ungroup()

