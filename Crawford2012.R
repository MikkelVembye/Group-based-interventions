library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Crawford et al. (2012)
#Draft by Jasmin

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


Crawford2012 <- tibble(
  Outcome = rep(c("GLobal assessment of functioning", "Positive and negative syndrome scale", "Positive symptoms score", "Negative symptoms score",
                  "General symptoms score", "Social functioning", "Wellbeing", "Satisfaction with care", "Morisky score"), each = 3),
  Treatment = rep(c("Standard care", "Actvity groups", "Group art therapy"), dplyr::n_distinct(Outcome)),
  
  # Flere sample sizes t1 står for time1 og t2 for time2. Kunne også være post og fu istedet for t1 og t2 
  N_base = rep(c(137, 140, 140), dplyr::n_distinct(Outcome)),
  N_12month = rep(c(121, 121, 119), dplyr::n_distinct(Outcome)),
  N_24month = rep( c(117, 121, 117), dplyr::n_distinct(Outcome)),

  m_pre = c(
    44.9, 45.0, 44.8,
    72.6, 75.3, 74.3,
    17.3, 18.2, 18.,
    18.5, 18.7, 18.7,
    36.8, 37.6, 37.6,
    8.1,  9., 8.6,
    64.5, 59.1, 58.3,
    24.9, 23.8, 24.8,
    1.2, 1.2, 1.0),
  
  m_12month = c(
    45.7, 45.5, 44.9,
    71.2, 69.6, 72.7,
    16.7, 16.1, 17.3,
    18.2, 17.3, 18.4,
    36.3, 35.9, 37.,
    8.5, 8.1, 8.3,
    64.1, 63.6, 59.6,
    24.3, 25., 23.6,
    .7, .6, .7),
  
  
  m_24month = c(
    46.8, 46.4, 45.6,
    68.1, 66.9, 69.2, 
    16.1, 15.6, 16.8,
    17.2, 16.4, 16.9,
    34.9, 34.9, 35.3,
    8.1, 8.1, 8.2,
    68.1, 66.1, 65.1,
    24.2, 24.9, 23.1,
    .6, .5, .6),
  
  ## Standard deviation values##
  sd_pre = c(
    12.6, 12.7, 13.1,
    21.5, 22., 23.7,
    5.6, 6.8, 6.9,
    7.5, 7., 7.1,
    11.3, 12.5, 12.5,
    4.7, 4.8, 4.2,
    20.6, 19.5, 21.1,
    5.7, 6.2, 5.7,
    1.3, 1.3, 1.2),
  
  sd_12month = c(
    14.4, 14.1, 14.6,
    24.6, 23.2, 27.3,
    6.3, 5.9, 7.6, 
    7.7, 7.2, 8.,
    13., 12.7, 14.,
    4.9, 4.6, 8.3,
    23.7, 23.2, 20.8,
    6.4, 5.2, 6.5,
    1.1, .6, .7),
  
  
  sd_24month = c(
    12.8, 13.6, 13.1,
    20.7, 23.3, 21.8,
    5.5, 6.4, 6.5,
    7.3, 6.8, 7.1,
    11.3, 12.4, 11.4,
    4.8, 4.5, 4.8,
    18.8, 18.4, 18.6,
    5.3, 5., 5.9,
    .9, .9, .9)
)

Crawford2012
View(Crawford2012)