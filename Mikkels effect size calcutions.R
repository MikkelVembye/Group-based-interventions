library(tidyverse)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 310)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE)

# Til inspektion
# Fisher_test |> pivot_wider(names_from = treatment, values_from = N:v)

## Fisher 1996
# Find data in Table 4 (p. 1248)

Fisher1996 <- tibble::tibble(
  
  Setting = rep(c("Inpatient", "Outpatient"), each = 3, 2),
  
  Outcome = rep(c("Alcohol use", "Outcome 2"), each = 6),
  
  treatment = rep(rep(c("Disease", "Cognitive", "TAU"), 2), 2),
  
#  N = 6,
#  
#  m_pre = c(
#    .469, .441, .349, # Alcohol use - inpatient
#    .725, .598, .682  # Alcohol use - outpatient
#    
#  ),
#  
#  sd_pre = c(
#    .12, .13, .22,
#    .11, .16, .17
#    
#  ),
#  
#  m_post = c(
#    .070, .018, .141,
#    .521, .152, .492
#  ),
#  
#  sd_post = c(
#    .11, .05, .21,
#    .11, .15, .27
#    
#    
#  ),
#  
#  m_diff = c(
#    .399, .423, .208,
#    .204, .446, .190
#    
#  ),
#  
#  sd_diff = c(
#    .02, .09, .04,
#    .01, .02, .11
#    
#  ),
#  
#  # Taking from the Cochrane handbook (Higgins & Thomas, 2019, p. 166)
#  # r = (sd_pre^2 + sd_post^2-sd_diff^2)/(2 * sd_pre * sd_post ),
#  # 
#  # # Obtained from Wilson (2016). Converting sd_diff to raw sd
#  # sdg = sd_diff/sqrt(2*(1-r)),
#  # 
#  # z = 0.5 * log( (1+r)/(1-r) ),
#  # v = 1/(N-3)
#  
#  
); Fisher1996

# Effect size calculation

Fisher_test <- 
  Fisher1996 |> 
  dplyr::filter(treatment != "Disease")


treat_label_fisher <- unique(Fisher1996$treatment)[1:2]


fisher_functionen <- function(label){
  
    Fisher1996 |> 
    dplyr::filter(treatment != label) |> 
    group_by(Setting, Outcome) |> 
    summarise(
      study = "Fisher1996",
      treatment = treatment[1],
      es_method = "diff-in-diffs",
      
      M = sum(v*z)/sum(v),
      ppcor = (exp(2*M)-1)/(exp(2*M)+1),
      
      Nt = N[1],
      Nc = N[2],
      N_tot = Nt + Nc,
      
      df_ind = N_tot,
      
      s_pool = sqrt(sum((N - 1) * sd_post^2) / df_ind),
      
      diff_t = m_post[1] - m_pre[1],
      diff_c = m_post[2] - m_pre[2],
      
      d = (diff_t - diff_c) / s_pool,
      vd = sum(1/N) * (2*(1-ppcor)) + d^2/(2*df_ind),
      se = sqrt(vd)
      
      
    ) |> 
    ungroup() |> 
    relocate(study)
  
}

Fisher1996_est <- 
  map_dfr(treat_label_fisher, ~ fisher_functionen(label = .x))







