# Dataextraction Rosenblum et al. 2014
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

# Extracting data from table 3 p. 85 

Rosenblum2014 <- tibble(
  
  group = as.factor(rep(c(
    "Double Trouble in Recovery (DTR)",
    "waiting list control group"), 
    each = 1,15
  )),
  
  sample = rep(c(
    "Total",
    "New York",
    "Michigan"
  ), each = 10,1),
  
  
  outcome = rep(c(
    "Days any alcohol past 30",
    "Days heavy alcohol past 30",
    "Days any drugs past 30",
    "Days any drugs or alcohol past 30",
    
    "Medication Adherence"
    
   # "RQOL (quality of life)c (total scale)" Only measured at follow-up 
   ), each = 2,3
 ),
 
 N = c(
   rep(c(91,70), each = 1,5), # Total 
   
   rep(c(35, 32), each = 1,5), # New York
   
   rep(c(56, 38), each = 1,5) # Michigan
   
 ),
 
 m_pre = c(
   4.5, 6.5,
   2.6, 2.1,
   6.9, 7.7,
   9.3, 11.9,
   1.5, 1.5,

   
   1.9, 3.9,
   0.3, 0.8,
   4.2, 7.1,
   4.7, 9.9,
   1.4, 1.4,

   
   6.1, 8.6, 
   4.0, 3.1,
   8.6, 8.2,
   12.2, 13.6,
   1.6, 1.6 

 ),
 
 sd_pre = c(
   8.2, 9.6,
   6.1, 5.1,
   9.3, 10.4,
   10.1, 11.2,
   0.5, 0.5,

   
   4.7, 7.2,
   1.0, 1.8,
   6.3, 10.2,
   6.1, 10.8,
   0.4, 0.4,
 
   
   9.5, 11.0,
   7.4, 6.5,
   10.5, 10.6, 
   11.0, 11.5,
   0.5, 0.5
  
 ), 
 
 m_post = c(
   3.1, 6.1,
   1.1, 2.1, 
   5.5, 8.0,
   6.8, 11.3,
   1.4, 1.4,
#   2.7, 2.0,
   
   1.6, 3.2,
   0.3, 1.0, 
   3.4, 8.0,
   4.5, 10.2,
   1.4, 1.3,
#   3.5, 3.2,
   
   4.0, 8.4,
   1.5, 3.0, 
   6.8, 8.0,
   8.3, 12.2,
   3.5, 1.6
#   2.1, 1.0
 ),

sd_post = c(
  6.4, 8.8,
  2.8, 6.0,
  9.2, 10.6,
  9.8, 11.1,
  0.4, 0.4,
  # 1.8, 1.8,
  
  4.4, 7.2,
  1.3, 4.5,
  7.1, 10.2, 
  7.8, 10.8, 
  0.3, 0.3,
  #  1.5, 1.7,
  
  7.3, 9.5, 
  3.4, 7.0,
  10.2, 11.1,
  10.6, 11.4, 
  0.5, 0.4
# 1.8, 1.3
),

q = c(
  rep(c(1), each = 1,10), # The baseline equivalent of the dependent variable was used as a covariate, p. 85
  rep(c(2), each = 1, 4), # The baseline variable PTSD diagnosis was added as a covariate to these analyses because it was correlated with study condition and the outcome variable, p. 85
  rep(c(1), each  = 1,2),
  rep(c(2), each = 1,2),
  rep(c(1), each = 1,12)
  )
); Rosenblum2014



# Turning data into wide format
params <- tibble(
  filter_val1 = rep(unique(Rosenblum2014$sample), each = 1)
)


wide_Rosenblum2014_func <- 
  function(filter_val1){
    
    Rosenblum2014 |> 
      filter(sample == filter_val1) |> 
      mutate(group = case_match(group, "Double Trouble in Recovery (DTR)" ~ "t", "waiting list control group" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }


Rosenblum2014_est <- 
  pmap(params, wide_Rosenblum2014_func) |>
  list_rbind() |> 
  mutate(
    
    p_val = c(0.03, 0.11, 0.07, 0.02, 0.45, 
              0.84, 0.47, 0.07, 0.17, 0.73,
              0.01, 0.10, 0.31, 0.06, 0.15),
    
    analyse_plan = rep(c(
      rep(c("substance and alcohol use (and adaptation of the Addiction Severity Index plus Saliva testing)"), each = 1,4),
      rep(c("medication adherence (Medication Adherence Rating Scale; MARS)"), each = 1,1)
    ), each = 1,3),
    
    study = "Rosenblum et al. 2014" 
    
) |> 
  rowwise() |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # all scores are reverted because lower scores is beneficial 
    m_post =  (m_post_t - m_post_c)*-1,
    
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
    
    vary_id = paste0(sample, "/", outcome)
    
  ) |> 
  ungroup() 
  
  
  
