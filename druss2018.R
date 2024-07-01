library(VIVECampbell)
library(VIVECampbell)


ICC_005 <- 0.05
ICC_01 <- 0.1
ICC_02 <- 0.2
# Data from table 2 (p. 532), containing means and SD for health related quality of life 
# (PCS and MCS),and table 3 (p. 533), containing means and SD for RAS

Druss2018 <- tibble(
  group = as.factor(rep(c("Intervention", 
                          "Control"), 
                        each = 1,6)),
  
  timing = rep(c("3m", "6m"), each = 6,1),
  
  outcome = as.factor(rep(c("RAS", # Recovery Assessment Scale
                            # Both of the below measures is related to health related quality of life
                            # as measured by the Short-Form Health Survey (SF-36) (see p. 531)
                            "PCS", # The physical component summary
                            "MCS"), # The mental component summary
                          each= 2,2)),
  
  N = rep(c(198, 202), each = 1,6),
  
  
  m_pre = rep(c(
    3.72, 3.66,   # RAS
    32.73, 32.74, # PCS
    32.05, 32.04) # MCS
    , each = 1,2)
  ,
  
  sd_pre = rep(c(
    0.62, 0.57,    # RAS
    10.92, 11.29,  # PCS
    11.79, 11.36), # MCS
    each = 1,2) 
  ,
  
  m_post = c(
    3.89, 3.70,   # RAS 3 months
    34.49, 33.89, # PCS 3 months
    34.49, 34.25, # MCS 3 months
    
    3.87, 3.74,   # RAS 6 months
    35.42, 34.25, # PCS 6 months
    36.64, 34.54  # MCS 6 months
  ),
  
  sd_post = c(
    0.55, 0.68,   # RAS 3 months
    11.15, 10.41, # PCS 3 months
    11.15, 11.97, # MCS 3 months
    
    0.61, 0.59,   # RAS 6 months
    11.02, 11.52, # PCS 6 months
    12.28, 11.82  # MCS 6 months
  )
)

# Making the druss2018 tibble wide in order to estimate the effect sizes and
# further analysis. 

# Turning data into wide format
params <- tibble(
  filter_val1 = rep(c(paste0(c(3, 6), "m")), 1)
)

wide_druss2018_func <- 
  function(filter_val1){
    
    Druss2018 |> 
      filter(timing == filter_val1) |> 
      mutate(group = case_match(group, "Intervention" ~ "t", "Control" ~ "c")) |>
      tidyr::pivot_wider(
        names_from = group,
        names_glue = "{.value}_{group}",
        values_from = N:last_col()
      )   
    
  }


Druss2018_est <- 
  pmap(params, wide_druss2018_func) |>
  list_rbind() |>
  # Based on the degrees of freedom value reported in Druss et al. 2018 table 2 and 3
  mutate(
    p_val_f = rep(c(0.02, 0.046, 0.039), each = 1,2),
    df1 = 2,
    df2 = rep(c(704, 697, 697), each = 1,2), 
    F_val = rep(c(3.79, 3.09, 3.25), each = 1,2),
    analysis_plan = rep(
      c("Patient activation measureMorisky scale Recovery assessment scale (RAS)", # RAS
        "Short-Form Health Survey (SF-36)= Quality of life", # PCS & MCS
        "Short-Form Health Survey (SF-36)= Quality of life" # PCS & MCS
      ), each = 1,2),
    
    study = "Druss et al. 2018"
    
  ) |> 
  rowwise() |> 
  mutate(
    N_total = N_t + N_c,
    df_ind = N_total,
    
    m_post =  (m_post_t - m_post_c),
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c),
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
    
    
    
    m_diff_t = m_post_t - m_pre_t,
    m_diff_c = m_post_c - m_pre_c,
    
    d_DD = (m_diff_t - m_diff_c)/sd_pool,
    vd_DD = d_DD^2/F_val + d_DD^2/(2*df_ind),
    Wd_DD = d_DD^2/F_val,
    
    g_DD = J * d_DD, 
    vg_DD = g_DD^2/F_val + g_DD^2/(2*df_ind),
    Wg_DD = g_DD^2/F_val,
    
    
  ) |>  
  mutate( # Attempt to make cluster corrected estimates
    
    # Average cluster size in treatment group
    # Harp intevention had six to ten partcipants 
    
    avg_cl_size = 8, 
    avg_cl_type = "From study (p. 530)",
    
    # Imputed icc value
    icc = ICC_01,
    icc_type = "Imputed",
    
    n_covariates = 1,
    
    # Find info about the function via ?VIVECampbell::gamma_1armcluster
    gamma_sqrt = VIVECampbell::gamma_1armcluster(
      N_total = N_total, Nc = N_c, avg_grp_size = avg_cl_size, ICC = icc
    ),
    gamma_sqrt_icc005 = gamma_1armcluster(
      N_total = N_total, Nc = N_c, avg_grp_size = avg_cl_size, ICC = ICC_005
    ),
    gamma_sqrt_icc02 = gamma_1armcluster(
      N_total = N_total, Nc = N_c, avg_grp_size = avg_cl_size, ICC = ICC_02
    ),
    
    # Calculated via Eq. 7 (Hedges & Citkowicz, 2015)
    df_adj = df_h_1armcluster(N_total = N_total, ICC = icc, N_grp = N_t, avg_grp_size = grp_size_imp),
    omega = 1 - 3/(4*df_adj-1),
    
    df_adj_icc005 = df_h_1armcluster(N_total = N_total, ICC = ICC_005, N_grp = N_t, avg_grp_size = grp_size_imp),
    omega_icc005 = 1 - 3/(4*df_adj_icc005-1),
    
    df_adj_icc02 = df_h_1armcluster(N_total = N_total, ICC = ICC_02, N_grp = N_t, avg_grp_size = grp_size_imp),
    omega_icc02 = 1 - 3/(4*df_adj_icc02-1),
    
    
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
      vars = vgt_post:Wgt_post
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
      F_val = F_val,
      q = n_covariates,
      add_name_to_vars = "_DD"
    ),
    
    gt_DD_icc005 = omega_icc005 * d_DD * gamma_sqrt_icc005,
    vgt_smd_1armcluster(
      N_cl_grp = N_t, 
      N_ind_grp = N_c, 
      avg_grp_size = avg_cl_size, 
      ICC = ICC_005, 
      g = gt_DD_icc005, 
      model = "DiD",
      cluster_adj = FALSE,
      F_val = F_val,
      q = n_covariates,
      add_name_to_vars = "_DD_icc005",
      vars = vgt_DD_icc005:Wgt_DD_icc005
    ),
    
    gt_DD_icc02 = omega_icc02 * d_DD * gamma_sqrt_icc02,
    vgt_smd_1armcluster(
      N_cl_grp = N_t, 
      N_ind_grp = N_c, 
      avg_grp_size = avg_cl_size, 
      ICC = ICC_02, 
      g = gt_DD_icc02, 
      model = "DiD",
      cluster_adj = FALSE,
      F_val = F_val,
      q = n_covariates,
      add_name_to_vars = "_DD_icc02",
      vars = vgt_DD_icc02:Wgt_DD_icc02
    )
    
  ) |> 
  ungroup()

