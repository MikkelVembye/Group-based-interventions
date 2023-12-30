# Dataextraction from Lim et al. (2012)
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

# Data from table 2 (p.5)  containing means, standard deviation and ANOVA estimations
# with F-values and η2. Table 3 (p.6) has the same values but with ANCOVAS (i.e. indcludes covariates)


lim2020 <- tibble(
  group = as.factor(rep(c("Social Cognitive Skills Training",
                          "Treatment as Usual"), each = 1,6)
                    ),
  
  outcome = as.factor(rep(c(
                            "Social Functioning (QLS)",
                            "PANSS Negative",
                            "PANSS Excitement",
                            "PANSS Cognitive",
                            "PANSS Positive",
                            "PANSS Depressive"
                            ), each = 2,1 )
                      ),
  
  N = 21,
  
  m_pre = c(46.15, 61.74, # Social Functioning (QLS) total
           18.22, 16.57,  # PANSS Negative
           6.28, 6.91,    # PANSS Excitement
           13.94,13.57,   # PANSS Cognitive
           7.67, 9.62,    # PANSS Positive
           8.67, 11.29    # PANSS Depressive
           
    ), 
  
  sd_pre = c(12.01, 18.60, # Social Functioning (QLS) tota
             6.23, 3.84,   # PANSS Negative
             1.87, 2.02,   # PANSS Excitement
             4.67, 3.25,   # PANSS Cognitive
             2.00, 3.54,   # PANSS Positive
             2.47, 3.45    # PANSS Depressive
  ),
  
  m_post = c(54.76, 57.70, # Social Functioning (QLS) tota
             16.89, 18.14, # PANSS Negative
             6.89, 7.33,   # PANSS Excitement
             13.44, 15.19, # PANSS Cognitive
             8.72, 8.72,   # PANSS Positive
             8.94, 13.81   # PANSS Depressive
    ),
  
  sd_post = c(10.88, 15.92, # Social Functioning (QLS) tota
              4.51, 3.71,   # PANSS Negative
              2.05, 2.20,   # PANSS Excitement
              2.75, 2.98,   # PANSS Cognitive
              3.06, 2.67,   # PANSS Positive
              2.98, 4.21    # PANSS Depressive
    
  )
)

# Making the lim2020 tibble wide in order to estimate the effect sizes and
# further analysis. 


lim2020_wide <-
  lim2020 |> 
  mutate (group = case_match(
    group, "Social Cognitive Skills Training"  ~ "t", "Treatment as Usual" ~ "c")) |> 
  tidyr::pivot_wider(d
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  )

