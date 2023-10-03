library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Jacob et al. (2010)
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


Jacob2010 <- tibble(
  Outcome = rep(c("RSES", "SSES Achievement", "SSES Social", "SSES Appearance", "BDI"), each = 2),
  Treatment = rep(c("Intervention group", "Control group"), dplyr::n_distinct(Outcome)),
  N = rep(c(19, 24), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    13.3, 13.3, 
    18.7, 19.2, 
    13.8, 16.3,  
    11.7, 10.8,
    27.8, 30.),
  
  m_post = c(
    16.7, 11.6, 
    18.7, 20.7, 
    14.5, 16.,  
    11.6, 10.4,
    22.6, 30.9),
  
  
  m_6month = c(
    17.8, 13.2, 
    21.5, 19.1, 
    15.3, 16., 
    11.7, 11.5,
    22.6, 29.6),
  
  ## Standard deviation values##
  sd_pre = c(
    8.6, 9.9, 
    4., 6.6, 
    6.6, 7.4,  
    5.7, 5.4,
    11.8, 12.2),
  
  sd_post = c(
    10.3, 9.3, 
    6.3, 6.9, 
    7., 7.2,  
    5.4, 5.3,
    13.1, 12.3),
  
  
  sd_6month = c(
    11., 11.2, 
    6.4, 6.3, 
    8., 7.4, 
    5.2, 5.7,
    14.5, 13.3)
)

Jacob2010
View(Jacob2010)