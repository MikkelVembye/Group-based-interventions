library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Gatz et al. (2007)
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


Gatz2007 <- tibble(
  Outcome = rep(c("ASI alcohol", "ASI drug", "GSI", "PSS", "Coping skills"), each = 2),
  Treatment = rep(c("Intervention group", "Comparison group"), dplyr::n_distinct(Outcome)),
  N = rep(c(136, 177), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    .18, .27, 
    .19, .25, 
    1.07, 1.09, 
    20.43, 19.05,
    52.61, 54.26),
  
  m_12month = c(
    .06, .13, 
    .04, .08, 
    .8, .86, 
    13.78, 15.11,
    56.32, 52.96),
  
  sd_pre = c(
    .29, .35, 
    .15, .14, 
    .68, .69, 
    10.22, 11.84,
    17.83, 17.51),
  
  sd_12month = c(
    .17, .23, 
    .07, .11, 
    .72, .77, 
    11.1, 12.91,
    20.15, 20.09)
)

Gatz2007
View(Gatz2007)

