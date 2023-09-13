library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)


#Data extraction from Burnam et al. (1995)
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


Burnam1995 <- tibble(
  Outcome = rep(c("Days used alcohol", "Level alcohol use", "Days used drugs", "Severity drug use", "Depressiona and anxiety", "Psychotic symptoms", "Anger and hostility", "Mania", "Self esteem", "percent time on streets", "Percent time in independant housing"), each = 2),
  Treatment = rep(c("Treatment group", "Control group"), dplyr::n_distinct(Outcome)),
  N = rep(c(211, 65), dplyr::n_distinct(Outcome)),
  
  m_pre = c(
    11.4, 12.3, 
    1.5, 1.6, 
    9.3, 4.5, 
    4.1, 4.5,
    53, 53,
    25, 27,
    27, 27,
    34, 32,
    49, 47,
    51, 53,
    17, 14),
  
  m_3month = c(
    5.2, .6, 
    0.5, 0.2, 
    3.8, 1.4, 
    1.3, 0.7,
    7.9, 7.5,
    2.1, 3.9,
    -.1, -7.8,
    .9, .2,
    7.2, 4.4,
    22, 22,
    10, 11),
  
  m_6month = c(
    3.6, 3.8, 
    0.3, 0.5, 
    2.4, 3.8, 
    .8, 2.2,
    7.3, 13.4,
    4.2, 4.9,
    -1.3, 2.8,
    .2, 2.5,
    8.5, 7.1,
    27, 22,
    23, 14),
  
  m_9month = c(
    5.3, 5.4, 
    0.4, 0.6, 
    3.8, 3.9, 
    1.8, 2.5,
    10.8, 15.5,
    5.2, 7.,
    -1., 2.5,
    1.1, 6.2,
    6.2, 2.8,
    21, 26,
    17, 29)
)

Burnam1995
View(Burnam1995)
