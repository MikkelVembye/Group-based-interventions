library(tibble)

library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Barbic et al. (2009)
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


Barbic2009 <- tibble(
  Outcome = rep(c("Hope Index", "Empowerment scale", "Quality of life", "Recovery assessment"), each = 2),
  Treatment = rep(c("Recovery workbook", "Control group"), dplyr::n_distinct(Outcome)),
  N = rep(c(16, 17), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    37.13, 36.00, 
    55.93, 63.88, 
    20.34, 20.70, 
    163.75, 156.41),
  
  m_post = c(
    38.93, 35.06, 
    14.93, 61.96, 
    21.60, 22.27, 
    168.81, 149.11),
  
  sd_pre = c(
    6.53, 5.36, 
    6.91, 6.91, 
    5.01, 4.13, 
    22.60, 14.22),
  
  sd_post = c(
    5.34, 6.21, 
    10.43, 7.33, 
    3.35, 4.91, 
    20.11, 22.09)
)

Barbic2009
View(Barbic2009)
