library(tidyverse)
#library(dplyr)
library(readxl)
library(writexl)
library(purrr)
library(VIVECampbell)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 310)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE)

ICC_01 <- 0.1
grp_size_imp <- 8

McCay2014_est <-
  tibble(
    
    study = "McCay",
    
    sd_used = "Unknown - d was calculated from ANOVA F-val",
    
    outcome = c("MES", "QLS", "MHS"),
    
    N_t = 29,
    N_c = 18,
    
    N_total = N_t + N_c,
    
    N_start_t = 41, # See text on page 3
    N_start_c = 26,
    
    df_ind = N_total,
    n_covariates = 1,
    
    m_pre_t = c(85.99, 61.90, 137.05),
    sd_pre_t = c(19.38, 17.08, 28.83),
    
    m_post_t = c(77.64, 72.76, 143.52),
    sd_post_t = c(20.28, 15.03, 26.17),
    
    tval_paired = c(4.12, -6.59, -2.13),
    
    ppcor = ((sd_pre_t^2*tval_paired ^2 + sd_post_t^2*tval_paired^2) - 
           (m_post_t-m_pre_t)^2 * N_t)/ (2 * sd_pre_t*sd_post_t*tval_paired^2),
    
    ppcor_method = "Calcualted from treatment group scores only",
    
    F_val = c(6.5, 11.67, 6.15),
    main_es_method = "Repeated ANOVA (group x time)",
    
    d_adj = sqrt((F_val * N_total)/(N_t*N_c)) * sqrt(1-ppcor^2),
    vd_adj = (1/N_t + 1/N_c) * (1-ppcor^2) + d_adj^2/(2*N_total),
    Wd_adj = (1/N_t + 1/N_c) * (1-ppcor^2),
    
    J = 1 - 3/(4*df_ind-1),
    g_adj = J * d_adj,
    vg_adj = (1/N_t + 1/N_c) * (1-ppcor^2) + g_adj^2/(2*N_total),
    Wg_adj = (1/N_t + 1/N_c) * (1-ppcor^2)
    
    
  ) |> 
  rowwise() |> 
  mutate(
    
    # Average cluster size in treatment group
    # "was 7.0±2.1 (range of three to 12)" (p. 57)
    avg_cl_size = grp_size_imp, 
    avg_cl_type = "Imputed",
    
    # Imputed icc value
    icc = ICC_01,
    icc_type = "Imputed",
    
    n_covariates = 1,
    
    # Find info about the function via ?VIVECampbell::gamma_1armcluster
    gamma_sqrt = VIVECampbell::gamma_1armcluster(
      N_total = N_total, Nc = N_c, avg_grp_size = avg_cl_size, ICC = icc
    ),
    
    # Calculated via Eq. 7 (Hedges & Citkowicz, 2015)
    df_adj = df_h_1armcluster(N_total = N_total, ICC = icc, N_grp = N_t, avg_grp_size = avg_cl_size),
    omega = 1 - 3/(4*df_adj-1),
    
    gt_adj = omega * d_adj * gamma_sqrt,
    VIVECampbell::vgt_smd_1armcluster(
      N_cl_grp = N_t,
      N_ind_grp = N_c,
      avg_grp_size = avg_cl_size,
      ICC = icc, 
      g = gt_adj, 
      model = "ANCOVA",
      cluster_adj = FALSE,
      R2 = ppcor,
      add_name_to_vars = "_adj",
      vars = -var_term1_adj
    ),
    
  ) |> 
  ungroup(); McCay2014_est 

