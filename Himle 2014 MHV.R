### **Himle et al. [-@Himle2014]**

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

# From table 2, p. 174
Himle2014 <- 
  tibble(
    group = as.factor(rep(c("Intervention group",
                            "Control group"),
                          each = 1, 12)), # 18 rækker af hver gruppe fordi der er 9 outcomes of to opfølgningsperioder
    
    timing = rep(c("Post","3m"), each = 12,1), #den respektive opfølgning skal gentages 18 gange
    
    
    outcome = rep(c(
      "LSAS -Total", 
      #"Work-related social anxiety- total", 
      "Brief fear of negative evaluation", 
      "Mini social phobia inventory", 
      "Beck anxiety inventory (BAI)", 
      "PHQ9 depression screen", 
      "Sheehan disability scale"
      #  "Social phobia symptom severity", 
      #  "Clinician global impressions - symptom severity" 
    ), each = 2, 2), # Outcomes gentages to gange for hver opfølgningsperiode
    
    N = 29, #i hver gruppe er der 29 observationer
    # som skal gentages 18 gange for hver opfølgning
    
    N_start = 29,
    
    m_pre = rep(c(
      84.31, 87.59, 
      #  18.72, 19.72,
      46.59, 45.,
      6.72, 7.52,
      17.69, 22.9,
      10.59, 12.17,
      5.93, 6.07
      #  2.07, 2.1,
      # 4.59, 4.24)
    ),
    each = 1,2),
    
    sd_pre = rep(c(
      31.51, 26.33, 
      #   7.42, 6.53,
      12.83, 13.19,
      2.2, 2.87,
      12.83, 14.73,
      8.08, 5.62,
      2.6, 2.33
      #  .65, .62,
      #  1.52, .99)
    ),
    each = 1,2),
    
    m_post = c(
      
      #Postvalues
      66.72, 90.62, 
      #  13.21, 19.76,
      39.01, 43.07,
      7.79, 9.66,
      11.34, 24.72,
      5.62, 10.41,
      3.63, 4.77,
      #  1.31, 2.17,
      #  3.76, 4.14,
      
      #3m follow-up
      65.31, 92.79, 
      # 13.31, 19.31,
      35.62, 44.,
      7.48, 10.28,
      12.83, 22.45,
      5.83, 10.72,
      3.48, 5.61
      #  1.28, 2.21,
      #  3.21, 4.21
    ),
    
    ## Standard deviation-values ##
    sd_post = c(
      # Post SD values
      29.02, 36.67, 
      # 7.10, 7.83,
      13.57, 14.48,
      2.81, 2.58,
      9.54, 17.10,
      5.49, 6.92,
      2.1, 2.02,
      #  .81, .66,
      #  1.35, 1.27,
      
      37.44, 36.26, 
      #  8.38, 7.72,
      13.57, 12.57,
      3.01, 1.28,
      13.4, 14.96,
      5.74, 6.68,
      2.27, 2.38
      #    .84, .73,
      #    1.35, 1.37
    )
  )

Himle2014_wide <- 
  Himle2014|> 
  mutate(group = case_match(group, "Intervention group" ~ "t", "Control group" ~ "c")) |>
  tidyr::pivot_wider(
    names_from = group,
    names_glue = "{.value}_{group}",
    values_from = N:last_col()
  ) |> 
  relocate(outcome)

# Multi level effect estimates can be obtained from table 3, p. 3
Himle2014_est <- 
  Himle2014_wide |> 
  mutate(
    analysis_plan = rep(c(
      "the Liebowitz Social Anxiety Scale",
      "The Brief Fear of Negative Evaluation scale",
      "The Mini Social Phobia Inventory",
      "Beck Anxiety Inventory",
      "Patient Health Questionnaire (PHQ-9)",
      "Shehan disability scale"
    ), each = 2),  # assuming there are two rows per each analysis_plan, adjust if necessary
    
    analysis_plan = case_when(
      str_detect(outcome, "LSAS|Mini|Brief") ~ "Mental health outcomes",
      str_detect(outcome, "Beck") ~ "Mental health outcomes/Anxiety",
      str_detect(outcome, "Sheehan") ~ "Social functioning (degree of impairment)",
      str_detect(outcome, "PHQ9") ~ "Physical health",
      TRUE ~ NA_character_
    ),
    
    study = "Himle et al. 2014"
    
  ) |> 
  mutate(
    
    # ppcor is imputed
    ppcor = ppcor_imp,
    ppcor_calc_method = "Imputed",
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # MHV: ER VI HELT SIKRE PÅ DETTE?
    # Reverting all outcomes 
    m_post = (m_post_t - m_post_c) * -1,
    
    sd_pool = sqrt(((N_t - 1) * sd_post_t^2 + (N_c - 1) * sd_post_c^2) / (N_t + N_c - 2)),  
    
    d_post = m_post / sd_pool, 
    vd_post = (1 / N_t + 1 / N_c) + d_post^2 / (2 * df_ind),
    Wd_post = (1 / N_t + 1 / N_c),
    
    J = 1 - 3 / (4 * df_ind - 1),
    
    g_post = J * d_post,
    vg_post = (1 / N_t + 1 / N_c) + g_post^2 / (2 * df_ind),
    Wg_post = Wd_post,
    
    # Reverting all outcomes
    m_diff_t = (m_post_t - m_pre_t) * -1,
    m_diff_c = (m_post_c - m_pre_c) * -1,
    
    d_DD = (m_diff_t - m_diff_c) / sd_pool,
    vd_DD = 2 * (1 - ppcor) * (1 / N_t + 1 / N_c) + d_DD^2 / (2 * df_ind),
    Wd_DD = 2 * (1 - ppcor) * (1 / N_t + 1 / N_c),
    
    g_DD = J * d_DD, 
    vg_DD = 2 * (1 - ppcor) * (1 / N_t + 1 / N_c) + g_DD^2 / (2 * df_ind),
    Wg_DD = Wd_DD
    
  ) |> 
  rowwise() |> 
  mutate(
    
    # Average cluster size in treatment group
    avg_cl_size = 6, 
    avg_cl_type = "From study (p. 170)",
    
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
    omega = 1 - 3 / (4 * df_adj - 1),
    
    # Cluster-adjusting g_post
    gt_post = omega * d_post * gamma_sqrt,
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
    )
  ) |> 
  ungroup() |> 
  mutate(
    vary_id = paste0(outcome, "/", timing)
  )

Himle2014_est
