library(tidyverse)
library(VIVECampbell)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 310)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE)

# Find in Table 4 (p. 1248)

Fisher1996 <- tibble::tibble(
  
  Outcome = rep(c("Alcohol use", "Drug use"), each = 6),
  
  Setting = rep(c("Inpatient", "Outpatient"), each = 3, 2),
  
  treatment = rep(rep(c("Disease", "Cognitive", "TAU"), 2), 2),
  
  # N is imputed/guested for illustrative purposes only, because we only know the total sample 
  # of the treatment groups (N = 19). We here assume that the control group have similar size as
  # the treatment group. 
  N = 9, 
  
  m_pre = c(
    
    # All values entered below follow the same structure as presented here
    
    .469, .441, .349, # Alcohol use - inpatient
    .725, .598, .682, # Alcohol use - outpatient
    
    .107, .116, .117, # Drug use - inpatient 
    .219, .200, .322  # Drug use - outpatient
  ),
  
  sd_pre = c(
    .12, .13, .22,
    .11, .16, .17,
    
    .09, .10, .12,
    .11, .12, .05
    
  ),
  
  m_post = c(
    .070, .018, .141,
    .521, .152, .492,
    
    .001, .008, .087,
    .167, .044, .216
    
  ),
  
  sd_post = c(
    .11, .05, .21,
    .11, .15, .27,
    
    .01, .02, .12,
    .10, .05, .15
    
  ),
  
  m_diff = c(
    .399, .423, .208,
    .204, .446, .190,
    
    .106, .108, .030,
    .052, .196, .106
    
  ),
  
  sd_diff = c(
    .02, .09, .04,
    .01, .02, .11,
    
    .09, .09, .02, 
    .02, .08, .11
    
  ),
  
  # Here we test if the mean differences reported in the study are in line
  # with the differences between the reported pre and post means. It seems to
  # be the case that they make a reporting error for the cognitive outpatient 
  # group mean difference on drug use. Wrtie about the error can be on many levels
  mean_diff_test = m_pre - m_post,
  
  # From the Cochrane handbook (Higgins & Thomas, 2019, p. 166)
  r = (sd_pre^2 + sd_post^2-sd_diff^2)/(2 * sd_pre * sd_post ),
  
  # Obtained from Wilson (2016). Converting sd_diff to raw sd
  sdg = sd_diff/sqrt(2*(1-r)),
  
  z = 0.5 * log( (1+r)/(1-r) ),
  v = 1/(N-3)
  
  
); Fisher1996


treat_label_fisher <- unique(Fisher1996$treatment)[1:2]

fisher_function <- function(label){
  
  Fisher1996 |> 
  group_by(Outcome, Setting) |> 
  filter(treatment != label) |> 
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
    
    d_post = m_post[1] - m_post[2],
    
    # NOTE: remember scales
    #d_post_test = if_else(Outcome == "Alcohol use", m_post[1] - m_post[2], m_post[2] - m_post[1]),
    
    diff_t = m_post[1] - m_pre[1],
    diff_c = m_post[2] - m_pre[2],
    
    d_diff = (diff_t - diff_c) / s_pool,
    vd = sum(1/N) * (2*(1-ppcor)) + d_diff^2/(2*df_ind),
    se_d = sqrt(vd),
    Wd = sum(1/N) * (2*(1-ppcor)),
    
    J = 1 - 3/(4*df_ind-1),
    
    g = J * d_diff,
    vg = J^2 * vd, 
    se_g = sqrt(vg),
    Wg = J^2* Wd,
  
    .groups = "drop"
  ) |> 
  relocate(study)
  
}

Fisher1996_est <- 
  purrr::map_dfr(treat_label_fisher, ~ fisher_function(label = .x))

Fisher1996_est

