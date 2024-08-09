

# Data from table 2 and table (p. 505)  containing marginal means with standard error
# It also contains standard error. To calculate pre-post test we will use the standard 
# deviation from table 1 (p. 504)

### **Wojtalik et al. [-@Wojtalik2022]**

# Loading the relevant package herein Tidyverse 
library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(tidyverse)
library(stringr)

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

ICC_01 <- 0.1

ppcor_imp <- 0.5

# Data from table 2 and table (p. 505)  containing marginal means with standard error
# It also contains standard error. To calculate pre-post test we will use the standard 
# deviation from table 1 (p. 504)

Wojtalik2019 <- tibble( 
  group = as.factor(rep(c("Cognitive enhancement therapy (CET)", # Intervention
                          "Enriched supportive therapy (EST)"), # Control
                        each = 1,4)),
  
  analysis = rep(c("ITT", "Completers"), each = 4),
  
  
  timing = 18,
  
  outcome = as.factor(rep(c(
    #      "Cognition composite",
    "Social adjustment composite",
    "Symptoms composite"), 
    each = 2,2)),
  
  N = c(rep(c(58, 44), each = 1,2),
        rep(c(26, 23),each = 1,2)),
  
  N_start = rep(c(
    58, 44
  ), each = 1,4),
  
  emm_pre = c(
    # ITT
    #    34.4, 38,  # Baseline Cognition composite 
    49.5, 51.0,# Baseline Social adjustment composite
    40.1, 50.4, # Baseline Symptoms composite
    
    
    # Completers
    
    #    34.6, 39.4, # Baseline Cognition composite
    47.1, 50.4, # Baseline Social adjustment composite
    49.4, 50.2 # Baseline Symptoms composite
    
  ),
  
  # Taken from table 1 (p. 504)
  sd_pre = rep(
    c(
      # ITT
      #    11.3, 11.8, # Baseline Cognition composite  
      9.6, 10.5,  # Baseline Social adjustment composite
      10.4, 10.0), 2
  
    # Baseline Symptoms composite
  
  # Completer (taken from online supplements)
  #rep(c(
  #  #    12.52, 12.52,
  #  8.80, 8.80,
  #  9.18, 9.18
  #), each = 1,1)
  ),
  
  se_pre = rep(c(
    # ITT
    #    1.5, 1.7,  # Baseline Cognition composite 
    1.3, 1.4,  # Baseline Social adjustment composite
    1.3, 1.5,   # Baseline Symptoms composite
    
    # Completer
    #    2.4, 2.6,
    1.8, 1.8,
    1.9, 2.0
    
  ), each = 1,1),
  
  emm_post = c(
    # ITT
    #    38.4, 37.1, # 9 months Cognition composite
    #    54.1, 56.8, # 9 months Social adjustment composite
    #    53.5, 53.4, # 9 months Symptoms composite
    
    #    40.1, 39.8, # 18 months Cognition composite
    56.3, 55.5, # 18 months Social adjustment composite
    54.3, 53.5,  # 18 months Symptoms composite
    
    # Completer
    #    40.0, 37.5, # 9 months Cognition composite
    #    52.2, 54.9, # 9 months Social adjustment composite
    #    53.2, 52.8, # 9 months Symptoms composite
    
    #    41.3, 40.6, # 18 months Cognition composite
    54.5, 53.4, # 18 months Social adjustment composite
    54.0, 53.1  # 18 months Symptoms composite
    
  ),
  
  se_post = c(
    # ITT
    #    1.6, 1.9, # 9 months Cognition composite
    #    1.4, 1.7, # 9 months Social adjustment composite
    #    1.4, 1.7, # 9 months Symptoms composite
    
    #    1.8, 2.0, # 18 months Cognition composite
    2.0, 2.3, # 18 months Social adjustment composite
    1.8, 1.9,  # 18 months Symptoms composite
    
    # Completer
    #    2.4, 2.6, # 9 months Cognition composite
    #    1.9, 2.0, # 9 months Social adjustment composite
    #    1.8, 1.9, # 9 months Symptoms composite
    
    #    2.5, 2.6, # 18 months Cognition composite
    2.4, 2.5, # 18 months Social adjustment composite
    2.0, 2.1  # 18 months Symptoms composite
    
  )
  
); Wojtalik2019 

# Making the tibble into wide format
Wojtalik2019_wide <- Wojtalik2019 |> 
  mutate(
    group = case_match(
      group, 
      "Cognitive enhancement therapy (CET)" ~ "t", 
      "Enriched supportive therapy (EST)" ~ "c"
    )
  ) |>
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  ) |> 
  mutate(
    t_val = c(0.8, 0.43, 1.60, 0.60),
    d_paper = c(0.23, 0.11, 0.51, 0.18)
  
  ); Wojtalik2019_wide

Wojtalik2019_est <- 
  Wojtalik2019_wide |> 
  mutate(
    analysis_plan = case_when(
      str_detect(outcome, "Social adjustment composite") ~ "Social functioning (degree of impairment)",
      str_detect(outcome, "Symptoms composite") ~ "Mental health outcomes"),
    
    study = "Wojtalik et al. 2019", 
    
    # ppcor imputed
    pp_cor_method = "Not computable - t values used"
    
  ) |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    m_diff = emm_post_t - emm_post_c,
    sd_pool = sqrt(((N_t-1)*sd_pre_t^2 + (N_c-1)*sd_pre_c^2)/(N_t + N_c - 2)),
      
    d_adj = m_diff/sd_pool,
    vd_adj = d_adj^2/t_val^2 + d_adj^2/(2*df_ind),
    Wd_adj = d_adj^2/t_val^2,
    
    J = 1 - 3/(4*df_ind-1),
    
    g_adj = J * d_adj,
    vd_adj = g_adj^2/t_val^2 + g_adj^2/(2*df_ind),
    Wg_adj = g_adj^2/t_val^2
    
    
  ) |> 
  rowwise() |> 
  mutate(
    avg_cl_size = 4, 
    avg_cl_type = "From study (p. 530)", 
    
    icc = ICC_01, 
    icc_type = "Imputed", 
    n_covariates = 1, 
    
    gamma_sqrt = gamma_1armcluster(N_total = N_total, Nc = N_c, avg_grp_size = avg_cl_size, ICC = icc), 
    
    df_adj = df_h_1armcluster(N_total = N_total, ICC = icc, N_grp = N_t, avg_grp_size = avg_cl_size), 
    
    omega = 1 - 3 / (4 * df_adj - 1), 
    
    gt_adj = omega * d_adj * gamma_sqrt, 
    VIVECampbell::vgt_smd_1armcluster(
      N_cl_grp = N_t, 
      N_ind_grp = N_c, 
      avg_grp_size = avg_cl_size, 
      ICC = icc, 
      g = gt_adj, 
      model = "emmeans", 
      cluster_adj = FALSE, 
      t_val = t_val, 
      q = n_covariates, 
      add_name_to_vars = "_adj", 
      vars = -c("var_term1_adj")
      )
  ) |> 
  ungroup() |> 
  mutate(vary_id = paste0(outcome, "/", analysis)) |> 
  ungroup()

Wojtalik2019_est
