library(tibble)

library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(tidyverse)


#Data extraction from Fallot et al. (2011)
#Draft by Jasmin


Fallot2011 <- tibble(
  Outcome = rep(c("PTSD symptom severity", "Alcohol problem severity", "Global symptom severity", "depression subscale", "Anxiety subscale", "Hostility subscale", "Personal safety scale", "Physical component score", "Mental component score", "Current exposure to interpersonal abuse", "Current exposure to other stressors"), each = 2),
  Treatment = rep(c("TREM group", "Comparison group"), dplyr::n_distinct(Outcome)),
  N = rep(c(153, 98), dplyr::n_distinct(Outcome)),
  
  Gem den her til Mikkel - hvis jeg læser rigtigt er det både intention to treat og per protocol analysis
  
  m_pre = c(
    25.4, 27.5, 29.8, 
    25.7, 27.6, 27.7, 
    16.6, 15.6, 15.6, 
    .28, .33, .32,
    .09, .10, .09,
    25.3, 29.4, 28.6,
    100.5, 110.7, 111.9),
  
  m_post = c(
    22.9, 24.3, 26.1,
    20.8, 23.00, 24.00,
    18.3, 19.4, 16.3,
    .22, .25, .30,  
    .07, .06, .08,
    19.0, 26.2, 25.8,
    95.6, 102.9, 109.1),
  
  m_3month = c(
    22.1, 23.7, 24.5,
    20.9, 22.5, 24.3,
    19.5, 21.1, 17.6,
    .20, .25, .28,
    .06, .06, .06,
    19.9, 25.00, 25.2,
    94.1, 100.9, 106.8),
  
  m_6month = c(
    22.1, 20.7, 24.3,
    19.4, 19.9, 23.7,
    20.5, 22.4, 16.4,
    .24, .19, .27,
    .05, .04, .07,
    18.5, 22.3, 24.1,
    92.7, 100, 107.3),
  
  ## standard deviation values ##
  sd_pre = c(
    9.7, 9.8, 9.4, 
    11.2, 10.0, 10.2, 
    12.1, 11.9, 12.3,
    .25, .29, .27,
    .11, .14, .11,
    13.2, 11.7, 10.9,
    26.9, 24.6, 25.3),
  
  sd_post = c(
    12.4, 11.9, 10.3, 
    12, 11.2, 10.7, 
    11.9, 11.7, 12.4,
    .24, .24, .28,
    .12, .11, .11,
    12.4, 12, 12.6,
    24.7, 26.4, 24.5),
  
  sd_3month = c(
    12.2, 11.5, 11.8,
    13.8, 11.5, 12.5,
    11.6, 10.2, 11.9,
    .23, .26, .28,
    .11, .12, .10,
    14.4, 14., 13.,
    27.2, 27.8, 26.1),
  
  sd_6month = c(
    11.5, 11., 11.4,
    11.9, 11.7, 12.5,
    11.3, 10.7, 12.7,
    .26, .22, .28,
    .10, .10, .11,
    12.5, 13.3, 14.,
    24.7, 25.2, 25.4)
  
)

View(Schafer2019)