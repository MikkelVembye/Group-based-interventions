library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Himle et al. (2014)
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


Himle2014 <- tibble(
  Outcome = rep(c("LSAS -Total", "LSAS - Anxiety subscale", "LSAS -Avoidance subscale", "Work-related social anxiety- total", "Work-related social anxiety- anxiety Subscale", "Work-related social anxiety- Avoidance Subscale", "Brief fear of negative evaluation", "Mini social phobia inventory", "Beck anxiety inventory", " PHQ9 depression screen", "Sheehan disability scale", "Social phobia symptom severity", "Clinician global impressions - symptom severity", "Job search self-efficiacy scale", "Job search Activities", "Hrs. Worked per Week"), each = 2),
  Treatment = rep(c("Work-related cognitive behavioral therapy", "Vocational service as usual"), dplyr::n_distinct(Outcome)),
  N = rep(c(29, 29), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    84.31, 87.59, 
    43.45, 44.76, 
    40.86, 42.83, 
    18.72, 19.72,
    10.69, 10.55,
    8.03, 9.17,
    46.59, 45.,
    6.72, 7.52,
    17.69, 22.9,
    10.59, 12.17,
    5.93, 6.07,
    2.07, 2.1,
    4.59, 4.24,
    22.62, 19.03,
    30.38, 26.41,
    0., 0.),
  
  
  m_post = c(
    66.72, 90.62, 
    37.62, 45.28, 
    29.14, 45.24, 
    13.21, 19.76,
    8.07, 10.48,
    5.34, 9.10,
    39.01, 43.07,
    7.79, 9.66,
    11.34, 24.72,
    5.62, 10.41,
    3.63, 4.77,
    1.31, 2.17,
    3.76, 4.14,
    23.31, 18.48,
    34.05, 33.16,
    11.90, 4.14),
  
  
  m_followup = c(
    65.31, 92.79, 
    34.83, 47.10, 
    30.52, 45.83, 
    13.31, 19.31,
    7.66, 10.62,
    5.79, 8.66,
    35.62, 44.,
    7.48, 10.28,
    12.83, 22.45,
    5.83, 10.72,
    3.48, 5.61,
    1.28, 2.21,
    3.21, 4.21,
    24.31, 17.90,
    31.38, 25.33,
    16.10, 14.69),
   
  ## Standard deviation-values ##
  
  
  sd_pre = c(
    31.51, 26.33, 
    14.91, 12.36, 
    17.16, 14.68, 
    7.42, 6.53,
    3.54, 3.29,
    4.19, 3.72,
    12.83, 13.19,
    2.2, 2.87,
    12.83, 14.73,
    8.08, 5.62,
    2.6, 2.33,
    .65, .62,
    1.52, .99,
    6.24, 6.69,
    11.44, 10.8,
    0., 0.),
  
  sd_post = c(
    29.02, 36.67, 
    14.67, 18.26, 
    15.73, 18.61, 
    7.10, 7.83,
    3.63, 3.92,
    3.74, 4.36,
    13.57, 14.48,
    2.81, 2.58,
    9.54, 17.10,
    5.49, 6.92,
    2.1, 2.02,
    .81, .66,
    1.35, 1.27,
    5.37, 5.26,
    14.3, 8.39,
    19.81, 7.22),

  sd_followup = c(
    37.44, 36.26, 
    18.57, 17.99, 
    19.26, 18.35, 
    8.38, 7.72,
    4.49, 3.63,
    4.13, 4.21,
    13.57, 12.57,
    3.01, 1.28,
    13.4, 14.96,
    5.74, 6.68,
    2.27, 2.38,
    .84, .73,
    1.35, 1.37,
    5.7, 5.38,
    8.68, 6.48,
    10.49, 10.29)
  
)
  

Himle2014

View(Himle2014)