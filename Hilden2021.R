library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Hilden et al. (2021)
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


Hilden2021 <- tibble(
  Outcome = rep(c("BSL-23 total", "BSL-23 behavior", "Overall anxiety and impairment scale", "Patient health questionnaire 9 scale", "Alcohol use disorders identification test", "Sheehan disability work or studylife", "Sheehan disability social life", "Sheehan disability family life"), each = 2),
  Treatment = rep(c("Schema therapy", "TAU"), dplyr::n_distinct(Outcome)),
  N = rep(c(23, 12), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    39., 55.7, 
    3.2, 3.5, 
    11.3, 13.2, 
    14, 16.3,
    6.4, 8.4,
    6., 6.8,
    5.2, 6.7,
    5.2, 6.7),
  
  m_followup = c(
    32., 42.6, 
    1.5, 1.9, 
    10.3, 11.4, 
    5.7, 9.2,
    11.9, 14.3,
    5.2, 6.9,
    5.6, 5.9,
    4.9, 6.3),
  
  
  sd_pre = c(
    15.1, 14.9, 
    2.8, 3.5, 
    3.8, 2.6, 
    5.7, 4.1,
    4.8, 5.6,
    3.1, 2.3,
    2.8, 1.6,
    2.8, 1.6),
  
  sd_followup = c(
    16.4, 18.8, 
    1.7, 1.8, 
    3.9, 3.5, 
    5.5, 8.2,
    5.2, 5.9,
    3.3, 2.4,
    2.6, 2.7,
    2.9, 2.1)
)

Hilden2021

View(Hilden2021)
