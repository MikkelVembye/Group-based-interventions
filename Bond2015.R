library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Bond et al. (2015)
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


Bond2015 <- tibble(
  Outcome = rep(c("N of hospitalizations", "Days hospitalized"), each = 2),
  Treatment = rep(c("IPS", "Work Choice"), dplyr::n_distinct(Outcome)),
  N = rep(c(41, 43), dplyr::n_distinct(Outcome)),
  
  m_post = c(
    1.20, .70,
    10.44, 4.93),
  
  sd_post = c(
    1.58, 1.04,
    23.07, 7.59)
  
)

  
Bond2015
View(Bond2015)