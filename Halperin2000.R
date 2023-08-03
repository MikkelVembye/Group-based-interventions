library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(tidyverse)


# Data extraction from Halperin et al. (2000)
# Draft by Jasmin


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



Halperin2000 <- tibble(
  Outcome = rep(c("The Brief Social Phobia Scale", "The Social Interaction Anxiety Scale", "The Calgary Depression Scale for Schizophrenia", "Global Severity Index", "The quality of life, enjoyment and satisfaction", "The alcohol use disorders identification test"), each = 2),
  Treatment = rep(c("CBT group", "Waitlist control"), dplyr::n_distinct(Outcome)),
  N = rep(c(7, 9), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    47.29, 37.56, 
    45.14, 41.11, 
    10.71, 8.56, 
    71.86, 64.,
    52.22, 54.79,
    11.29, 6.67),
  
  m_post = c(
    38.14, 37., 
    37.43, 40.88, 
    4.57, 9.33, 
    64.86, 64.11,
    58.75, 54.5,
    8.43, 7.11),
  
  sd_pre = c(
    10.63, 13.58, 
    11.26, 12.61, 
    2.53, 3.5, 
    5.73, 6.12,
    11.85, 12.35,
    9.14, 8.83),
  
  sd_post = c(
    6.23, 13.18, 
    11.89, 11.39, 
    3.26, 2.7, 
    10.59, 5.75,
    10.65, 11.32,
    5.68, 9.24)

)

View(Halperin2000)
Halperin2000