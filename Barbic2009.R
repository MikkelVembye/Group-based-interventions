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

View(Barbic2009)
