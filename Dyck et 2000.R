library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Dyck et al. (2000)
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


Dyck2000 <- tibble(
  Outcome = rep(c("The modified Scale for the Assessment of Negative Symptoms"), each = 2),
  Treatment = rep(c("Multiple-family group", "Standard care"), dplyr::n_distinct(Outcome)),
  N = rep(c(21, 21), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    7.9, 8.7),
  
  m_1to3months = c(
    7.4, 9.1),
  
  m_4to6months = c(
    7.2, 8.9),
  
  m_7to9months = c(
    7.2, 8.9),
  
  m_10to12months = c(
    7.2, 8.4),
  
  
  sd_pre = c(
    3.1, 3.3),
  
  sd_1to3months = c(
    2.3, 3.2),
  
  sd_4to6months = c(
    2.1, 2.7),
  
  sd_7to9months = c(
    2.1, 3.),
  
  sd_10to12months = c(
    2., 3.1)

)

Dyck2000
View(Dyck2000)