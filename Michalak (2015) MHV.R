# Loading the relevant package herein Tidyverse 
library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(tidyverse)
library(stringr)

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

ICC_01 <- 0.1

ppcor_imp <- 0.5


# With both pre- and post-test and beta-coefficients

Michalak_2015 <- tibble(
  outcome = rep(c(
    "HAM-D",   # Hamilton Depression Rating Scale
    "BDI"      # BDI
  ), each = 3), 
  
  group = rep(c(
    "MBCT", # mindfulness-based cognitive therapy
    "CBASP", # cognitive behavioral analysis system of psychotherapy
    "TAU" # Treatment as usual (control group)
  ), times = 2), 
  
  N = rep(c(36, 35, 35), times = 2), 
  N_start = N,
  
  m_pre = c(
    23.03, 24.71, 23.87,   # Hamilton Depression Rating Scale
    31.31, 30.26, 29.76    # BDI
  ),
  
  sd_pre = c(
    6.27, 6.69, 6.33,
    9.55, 7.52, 7.42
  ),
  
  m_post = c(
    17.86, 14.64, 21.16,     # Hamilton Depression Rating Scale
    22.69, 22.28, 28.34      # BDI
  ), 
  
  sd_post = c(
    10.37, 8.85, 8.16,
    12.11, 12.54, 10.27
  )
)


Michalak2015_wide_DD <- 
  map(c("MBCT", "CBASP"), \(x)
      Michalak_2015 |> 
      filter(group %in% c(x, "TAU")) |> 
      mutate(group = case_match(group, x ~ "t", "TAU" ~ "c")) |> 
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      ) |> 
        mutate(
          treatment = x,
          N_total = N_t + N_c
        ) |> 
        relocate(treatment)
  ) |> 
  list_rbind() |> 
  mutate(
  
    effect_size = "SMD",
    sd_used = "Posttest",
    main_es_method = "DiD",
    analysis_plan = "Mental health outcomes/Depression"
  
  )


Michalak2015_wide_from_paper <- 
  tibble(
    
    effect_size = "SMD",
    sd_used = "Unknown",
    main_es_method = "Covariate-adjusted es from multi-level regression",
    
    analysis_plan = rep(c(
      "Wellbeing and Quality of Life",
      "Social functioning (degree of impairment)"
    ), c(8,2)),
    
    
    outcome = paste0(
      rep(c("SF-36", "SASS"), c(8,2)), "/", 
      rep(c("Vitality", "Social func", "Role", "Mental health", "overall"), each = 2)
    ),
    
    treatment = rep(c("MBCT","CBASP"), each = 1,5),
    N_t = rep(c(36,35), each = 1,5), 
    N_c = 35, 
    N_total = N_t + N_c, 
    
    N_start_t = N_t,
    N_start_c = N_c,
    
    beta = c(1.12, 1.32, 
             0.27, 0.74,
             0.52, 0.46,
             0.63, 1.25,
             2.43, 1.68),
    se_b = c(0.90, 0.70,
             0.43, 0.44,
             0.26, 0.24,
             0.95, 0.89,
             1.01, 0.93),
    t_val = beta/se_b,
    
    es_paper = c(0.31,0.36, 
                 0.12, 0.34,
                 0.48, 0.42,
                 0.14, 0.28,
                 0.57, 0.40),
    
    es_paper_type = "Covariate-adjusted ES (d) from a multi-level model",
    
    sd_total = beta/es_paper
    
  )


Michalak_2015_dat <- 
  bind_rows(Michalak2015_wide_DD, Michalak2015_wide_from_paper)

ppcor <- ppcor_imp <- 0.5

Michalak_2015_est <- 
  Michalak_2015_dat |> 
  mutate(
    study = "Michalak et al. 2015",
    vary_id = paste0(outcome, "/", treatment),
    
    ppcor = ppcor_imp,
    ppcor_method = "Imputed",
    
    df_ind = N_total,
    m_post = if_else(is.na(es_paper), (m_post_t - m_post_c)*-1, NA_real_),
    sd_pool = if_else(
      is.na(es_paper),
      sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),
      sqrt( ((N_total-1)/(N_total-2))*sd_total^2 - ((N_t*N_c)/(N_total * (N_total-2)))*beta^2 )
    ),
    
    d_post = m_post/sd_pool,
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = if_else(is.na(es_paper), (1/N_t + 1/N_c), NA_real_),
    
    J =  1 - 3/(4*df_ind-1),
    
    g_post =  J * (m_post/sd_pool),
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
    # DiD (DD) effect sizes for HAM-D og BDI
    
    
    
    #cluster bias adjustment
    
    # For HAM-D lower scores is beneficial, hence these are reverted
    m_diff_t = if_else(
      str_detect(outcome, "HAM|BDI"),
      (m_post_t - m_pre_t) * -1,
      m_post_t - m_pre_t
    ),
    
    m_diff_c = if_else(
      str_detect(outcome, "HAM|BDI"),
      (m_post_c - m_pre_c) * -1,
      m_post_c - m_pre_c
    ), 
    
    d_DD = (m_diff_t - m_diff_c) / sd_pool,
    vd_DD = 2 * (1 - ppcor) * (1 / N_t + 1 / N_c) + d_DD^2 / (2 * df_ind),
    Wd_DD = if_else(is.na(es_paper), 2 * (1 - ppcor) * (1 / N_t + 1 / N_c), NA_real_),
    
    g_DD = J * d_DD, 
    vg_DD = 2 * (1 - ppcor) * (1 / N_t + 1 / N_c) + g_DD^2 / (2 * df_ind),
    Wg_DD = Wd_DD,
    
    # Reg adjusted effect sizes for SF-36 and SASS outcomes
    d_reg = beta/sd_pool,
    vd_reg = d_reg^2 * (1/t_val^2 + 1/(2*df_ind)),
    Wd_reg = d_reg^2 * 1/t_val^2,
    
    g_reg = J * d_reg,
    vg_reg = g_reg^2 * (1/t_val^2 + 1/(2*df_ind)),
    Wg_reg = Wd_reg
    
  ) |> 
  rowwise() |> 
  mutate(
    # Average cluster size in treatment group
    avg_cl_size = 6, 
    avg_cl_type = "From study (p. 177)",
    
    # Imputed icc value
    icc = ICC_01,
    icc_type = "Imputed",
    
    n_covariates = 1,# Baseline scores only
    
    # Find info about the function via ?VIVECampbell::gamma_1armcluster
    gamma_sqrt = VIVECampbell::gamma_1armcluster(
      N_total = N_total, Nc = N_c, avg_grp_size = avg_cl_size, ICC = icc
    ),
    
    # Calculated via Eq. 7 (Hedges & Citkowicz, 2015)
    df_adj = df_h_1armcluster(N_total = N_total, ICC = icc, N_grp = N_t, avg_grp_size = avg_cl_size),
    omega = 1 - 3 / (4 * df_adj - 1),
    
    # Cluster-adjusting g_post
    gt_post = if_else(is.na(es_paper), omega * d_post * gamma_sqrt, NA_real_),
    VIVECampbell::vgt_smd_1armcluster(
      N_cl_grp = N_t, 
      N_ind_grp = N_c, 
      avg_grp_size = avg_cl_size, 
      ICC = icc, 
      g = gt_post, 
      model = "posttest",
      cluster_adj = FALSE,
      add_name_to_vars = "_post",
      vars = c("vgt_post", "Wgt_post")
    ),
    
    gt_DD = omega * d_DD * gamma_sqrt,
    VIVECampbell::vgt_smd_1armcluster(
      N_cl_grp = N_t, 
      N_ind_grp = N_c, 
      avg_grp_size = avg_cl_size, 
      ICC = icc, 
      g = gt_DD, 
      model = "DiD",
      cluster_adj = FALSE,
      prepost_cor = ppcor,
      q = n_covariates,
      add_name_to_vars = "_DD",
      vars = -c("var_term1_DD")
    ),
    
    gt_reg = omega * d_reg * gamma_sqrt,
    VIVECampbell::vgt_smd_1armcluster(
      N_cl_grp = N_t, 
      N_ind_grp = N_c, 
      avg_grp_size = avg_cl_size, 
      ICC = icc, 
      g = gt_reg, 
      model = "reg_coef",
      cluster_adj = TRUE,
      t_val = t_val,
      q = n_covariates,
      add_name_to_vars = "_reg",
      vars = -c("var_term1_reg")
    ),
    
    across(c(Wgt_post, Wgt_DD:adj_value_DD), ~ if_else(!str_detect(outcome, "HAM|BDI"), NA, .x)),
    across(c(Wgt_reg:adj_value_reg), ~ if_else(str_detect(outcome, "HAM|BDI"), NA, .x))
    
  ) |> 
  ungroup(); Michalak_2015_est
