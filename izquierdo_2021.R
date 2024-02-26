# Dataextraction from Izquierdo et al. (2021)
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

# Rows 1-10 is data from table 3, p. 10 and rows 11-22 is from table 4 p. 13. 

izquierdo2021 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,12)),
  timing = rep(c(
    "Post",
    "3m",
    "6m"
  ),
  each = 8,1),
  
  
  outcome = rep(c(
                  #"EE",
                  #"OE",
                  "CS", 
                  "ACI",
                  "SCI",
                  #"POSIT",
                  #"NEGAT",
                  #"AFFECT",
                  #"DISOR",
                  #"DISORG",
                  "Total_score"),
                each = 2,3), 
  
  N = rep(c(7)),
  
  
  m_pre = rep(c(
    # 6.57, 10.57, # EE
    # 6.86, 8.29,  # OE
    13.43, 18.86,# CS
    NA, NA,      # ACI - Basline data not reported
    NA, NA,      # SCI - Basline data not reported
    # 21.29, 15.14,# POSIT
    # 10.57, 6.86, # NEGAT
    # 35.86, 37.14,# AFFECT
    # 5.00, 3.86,  # DISOR
    # 4.86, 2.43,  # DISORG
    77.57, 65.43 # Total_score
    ), each = 1,3),
  
  sd_pre = rep(c(
    # 2.44, 3.36,
    # 2.19, 1.38,
    2.44, 4.34,
    NA, NA,
    NA, NA,
    # 11.09, 5.84,
    # 4.24, 2.54,
    # 5.64, 4.56,
    # 3.00, 3.18, 
    # 2.67, 1.90,
    18.30, 10.85), 
    each = 1,3),
  
  m_post = c(
    
    # post measurement
    # 17.86, 10.57,
    # 16.29, 8.14,
    34.14, 18.71,
    36.57, 11.14,
    4.57, 1.14,
    # 8.14, 15.71,
    # 5.43, 7.71,
    # 16.86, 37.00,
    # 2.43, 3.86,
    # 1.29, 1.57,
    34.14, 65.86,
    
    # 3 months measurement
    #  16.00, 9.71,
    #  16.29, 7.57,
    32.29, 17.29,
    37.57, 11.00,
    4.57, 1.00,
    #  7.14, 15.71,
    #  5.00, 8.00,
    #  13.29, 35.14,
    #  2.57, 3.71,
    #  1.00, 1.14,
    29.00, 63.71,
    
    
    # 6 months measurement
    # 16.71, 9.71,
    # 15.71, 6.86,
    32.43, 16.57,
   37.14, 11.57,
    4.29, 1.14,
    #  7.29, 15.29,
    #  5.00, 8.29,
    #  13.00, 36.14,
    #  2.29, 3.57,
    #  1.00, 1.29,
    28.57, 64.57
    
  ),
  
  sd_post = c(
  
  # post measurement
  #  1.86, 3.15,
  #  3.09, 1.35,
    4.81, 2.87,
    5.32, 3.19,
    0.54, 0.38,
  #  2.12, 6.40,
  #  1.27, 3.45,
  #  2.27, 3.74,
  #  0.53, 2.54,
  #  0.49, 0.79,
    3.13, 11.13,
  
  # 3 months measurement 
  #  3.21, 2.98,
  #  2.63, 2.07,
  5.28, 2.87,
  4.20, 3.21,
  0.53, 0.00,
  #  1.07, 6.97,
  #  0.82, 3.56,
  #  0.95, 4.70,
  #  0.79, 2.63,
  #  0.00, 0.38,
  1.83, 11.97,
  
  # 6 months measurement 
  # 16.71, 9.71,
  # 15.71, 6.86,
  5.19, 2.88,
  4.88, 2.94,
  0.76, 0.38,
  #  7.29, 15.29,
  #  5.00, 8.29,
  #  13.00, 36.14,
  #  2.29, 3.57,
  #  1.00, 1.29,
  1.81, 9.68
  )
  

); izquierdo2021


# Turning data into wide format
params <- tibble(
  filter_val1 = rep(c("Post", paste0(c(3, 6), "m")), 1)
  
)



wide_izquierdo2021_func <- 
  function(filter_val1){
    
    izquierdo2021 |> 
      filter(timing == filter_val1) |> 
      mutate(group = case_match(group, "Intervention" ~ "t", "Control" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }



izquierdo2021_est <- 
  pmap(params, wide_izquierdo2021_func) |>
  list_rbind()|>
  
  mutate(
    analysis_plan = rep(
      c("Coping with Stress Self-efficacy (CSSE)",
        "Perceived changes in successful daily functioning (Areas of Change Index, ACI)",
        "Satisfaction with changes in overall daily functioning (single-item self-report, SCI)",
        "The expanded brief psychiatric rating scale" # Total score
      ), each = 1,3),
    
    study = "Izquierdo et al. 2021"
    
  )





