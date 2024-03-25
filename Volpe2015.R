# Dataextraction from McCay et al. 2007 - Jakob 
# Loading the relevant package herein Tidyverse 
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

Volpe_2015 <- tibble(
  # 'outcome' kolonnen oprettes ved at gentage en vektor af psykologiske skalaer.
  # Funktionen rep() anvendes her til at gentage elementerne i vektoren.
  # Argumentet 'each' bestemmer, hvor mange gange hvert element skal gentages i træk.
  outcome = rep(c(
    "BPRS",   # Brief Psychiatric Rating Scale
    "PHQ-9",  # Patient Health Questionnaire
    "PSP",    # Personal and Social Performance Scale
    "DAS-II"  # Disability Assessment Schedule 2.0
  ), each = 2, 1), 
  
  # 'group' kolonnen specificerer, hvilken gruppe hver måling tilhører. 
  # Her bruges rep() funktionen igen til at skifte mellem "Reading group" og "Control group".
  # Argumentet 'each=1' sørger for, at hver gruppebetegnelse gentages én gang før skiftet til den næste.
  group = rep(c(
    "Reading group",
    "Control group"
  ), each = 1, 4), 
  
  # 'N' kolonnen angiver antallet af deltagere (21 for læsegruppen, 20 for kontrolgruppen) for hvert outcome.
  # Funktionen rep() med 'each=1' sikrer, at hvert tal (21 og 20) gentages én gang per outcome,
  # mens det ydre '4' indikerer, at hele mønstret (21, 20) gentages fire gange for de fire outcomes.
  N = rep(c(21,20), each = 1, 4), 
  
  m_pre = c(
    49.95, 51.2, # Brief Psychiatric Rating Scale
    9.74, 9.9,   # Patient Health Questionnaire
    44.09, 46.2, # Personal and Social Performance Scale
    2.64, 2.95   # Disability Assessment Schedule 2.0
  ),
  
  sd_pre =c(
    15.14, 17.75,
    4.58, 4.35,
    16.19, 18.23,
    0.93, 0.94
  ),
  
  m_post = c(
    36.21, 41.2,
    4.7, 5.66,
    49.95, 47.25,
    1.25, 2.12
  ), 
  
  sd_post = c(
    17.43, 15.69,
    2.78, 2.45,
    15.53, 18.35,
    0.5, 0.25
  )
)

# Ved brug af piping kan du bruge ctrl + shift + m få denne frem |> 

# Making the tibble into wideformat
Volpe2015_est <-
  Volpe_2015 |> 
  mutate(group = case_match(group, "Reading group" ~ "t", "Control group" ~ "c")) |> 
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  ) |> 
  mutate(
    
    analysis_plan = c(
      "The Brief Psychiatric Rating Scale (BPRS)",
      "Personal Health Questionnaire Depression Scale (PHQ-9)",
      "Personal and Social Performance Scale (PSP)",
      "The Disability Assessment Schedule (DAS-II)"
    ),
    
    N_total = N_t + N_c,
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    
    # For BPRS, PHQ-9, and DAs-II lower scores are beneficial why these is reverted
    m_post = if_else(outcome != "PSP", (m_post_t - m_post_c)*-1,
                     m_post_t - m_post_c),
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
    vary_id = outcome
    
  ) |> 
  ungroup()
