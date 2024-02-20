# Data typing from Hagen et al. (2005)
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

# Data from table 2, p. 39 
Hagen2005 <- tibble(
  
  outcome = rep(c(
    
  "SCL-90",
  "BAI",
  "BDI",
  "IIP-64-C"
  #"SAS-A",
  #"SAS-S",
  #"YSQ"
  ), 
  each = 2,1
 ),
 
 group = rep(c(
     
   "waiting-list", "CBGT"
   ),
   each = 1,4
 ),
 
 N = c(
     17, 15, 
     17, 15, 
     15, 15, 
     17, 14 
     # 16, 14, 
     # 16, 15, 
     # 16, 14
   ),
 m_pre = c(
     1.19, 1.15,
     20.29, 22.60,
     18.20, 15.40,
     1.35, 1.22
    # 73.06, 62.93,
    #  73.87, 70.80, 
    #  11.50, 7.21
 ),
 
  sd_pre = c(
      0.61, 0.61,
      10.08, 14.77,
      8.55, 9.86,
      0.72, 0.63
     # 20.23, 15.66,
     # 20.26, 19.16,
     # 14.15, 8.62
  ) ,
 
 m_post = c(
   1.15, 0.88, 
   22.94, 18.13, 
   18.35, 11.00, 
   1.28, 1.18 
  # 60.12, 58.57, 
  # 69.12, 60.14, 
  # 10.17, 6.63
 ),
 
 sd_post = c(
   0.47, 0.59,  
   13.46, 12.00,
   9.00, 8.36,
   0.68, 0.76
  # 14.17, 18.67,
  # 13.99, 22.52,
  # 13.41, 11.16
 ),

); Hagen2005


# Making the Hagen2006 tibble wide in order to estimate the effect sizes and
# further analysis. 


Hagen2005_wide <-
  Hagen2005 |> 
  mutate (group = case_match(
    group, "CBGT"  ~ "t", 
    "waiting-list" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )

# Effect size calculating 
Hagen2005_est <-           
  Hagen2005_wide |>
  mutate(
    analysis_plan =
      c("The Symptom Checklist 90-Revised (SCL-90-R)",
        "Beck Anxiety Inventory (BAI) ",
        "Beck Depression Inventory (BDI)",
        "Inventory of Interpersonal Problems 64-Circumplex (IIP-64C)"
      )
  
  ) |> 
  
  rowwise() |> 
  mutate(
    study = "Hagen et al. 2005",
    
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # For all measures lower scores are beneficial why these are reverted
    m_post = (m_post_t - m_post_c)*-1, 
    
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
