# Dataextraction from Bækkelund et al. 2022 - Amalie 
# Loading the relevant package herein Tidyverse 
library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
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



Bækkelund2022 <- tibble(
  group = rep(c("Intervention", #Intervention group
                "Control"),     #Control group
              each = 1, 4),
  
  
  outcome = rep(c("GAF-S",     # Global Assesment of Function - Split version
                  "SCL-90 R"),  # Symptom Checklist 90 Revised
                each = 2, 2), 
  
  
  timing = rep(c("6m", "36m"), each = 4), 
  
  N = rep(c(
    29, 30, 
    29, 30), 
    each = 1, 2), 

  
  m_pre = rep(c(
    41.7, 40.9,
    1.8, 1.9),
    each = 1,2), 
  
  
  sd_pre = rep(c(
    5.7, 8.2,
    0.7, 0.9), 
    each = 1,2), 
  
  m_post = c(
    # 6 months
    43.9, 44.2,
    1.7, 1.9,
    
    # 36 months
    48.4, 45.7,
    1.7, 1.9),
  
  sd_post = c(
    # 6 months
    6.3, 8.7,
    0.8, 0.9,
    
    # 36 months
    6.3, 4.2,
    0.9, 1.0)
  
); Bækkelund2022

view(Bækkelund2022)  



# Making the tibble into wideformat in order to estimate effect sizes
Bækkelund2022_wide <-
  Bækkelund2022 |> 
  mutate(group = case_match(group, "Intervention" ~ "t", "Control" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  ) 

View(Bækkelund2022_wide)


# Effect size calculating 
Bækkelund2022_est <-           
  Bækkelund2022_wide |>
  mutate(
    pp_cor = 0.5,
    pp_cor_methods = "Imputed",
    
    diff_t = m_post_t - m_pre_t, 
    diff_c = m_post_c - m_pre_c,
    mean_diff = diff_t - diff_c,
    
    
    # SCL-90 R lower scores are beneficial why these is reverted
    m_post = if_else(outcome != "GAF-S", (m_post_t - m_post_c)*-1,
                     m_post_t - m_post_c),
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    N_total = N_t + N_c,
    df = N_total, # frihedsgrader svarer til den samlede stikprøvestørrelse
    
    d_post = m_post/sd_pool, # Cohens Post D - posttest baseret effektstørrelse
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df), # varians af effekstørrelsen
    Wd_post = (1/N_t + 1/N_c), # Det store variansled 
    
    d_DD = (mean_diff/sd_pool), #Cohens d
    vd_DD = (1/N_t + 1/N_c) * 2*(1-pp_cor) + d_DD^2/(2*df), # skaber falsk korrelation, hvorfor vi beregner det store variansled
    Wd_DD = (1/N_t + 1/N_c) * 2*(1-pp_cor), # det store variansled
    
    J = 1 - 3/(4*df-1), # her bruger vi J som er en korrigeringsfaktor som vi kan bruge til at beregne Hedges' g (fra d til g)
    
    g_post = J * (m_post/sd_pool),
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df),
    Wg_post = (1/N_t + 1/N_c),
    
    
    g_DD = J * (mean_diff/sd_pool), # her beregner vi Difference in difference effektstørrelse som er Mikkels foretrukne - baseline korrigeret effekstørrelse
    vg_DD = (1/N_t + 1/N_c) * 2*(1-pp_cor) + g_DD^2/(2*df), # variansen af den baseline adjusted effekstørrelse
    Wg_DD = Wd_DD,
    
    .by = outcome
    
  ); Bækkelund2022_est

view(Bækkelund2022_est)

## vgt_smd_1armcluster {VIVECampbell}	og så tryk f1 inde i kommandoen i console og ikke i parantesen. Så kommer der en guide

