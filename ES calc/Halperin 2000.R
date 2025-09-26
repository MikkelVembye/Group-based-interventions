library(dplyr)
library(tibble)

Halperin2000 <- 
  tibble(
  outcome = c("BSPS", "SIAS", "CDSS", "GSI", "Q-LES-Q", "AUDIT"),
  
  N_t = 7,
  N_c = 9,
  
  m_pre_t = c(47.29, 45.14, 10.71, 71.86, 52.22, 11.29),
  m_post_t = c(38.14, 37.43, 4.57, 64.86, 58.75, 8.43),
  
  sd_pre_t = c(10.63, 11.26, 2.43, 5.73, 11.85, 9.14),
  sd_post_t = c(6.23, 11.89, 3.26, 10.56, 10.65, 5.68),
  
  m_pre_c = c(37.56, 41.11, 8.56, 64.00, 54.79, 6.67),
  m_post_c = c(37.00, 40.88, 9.33, 64.71, 54.50, 7.11),
  
  sd_pre_c = c(13.58, 12.61, 3.50, 6.12, 12.35, 8.83),
  sd_post_c = c(13.63, 11.39, 2.70, 7.55, 11.32, 9.24),
  
  F_val = c(6.54, 6.95, 39.75, 9.26, 17.50, 1.43)
) |> 
  mutate(
    
    N_total = N_t + N_c,
    df_ind = N_total,
    
    # Calculate d post
    m_post = if_else(outcome == "Q_LES_Q", m_post_t - m_post_c, (m_post_t - m_post_c) * -1),
    sd_pool = sqrt(((N_t-1)*sd_post_t^2 + (N_c-1)*sd_post_c^2)/(N_t + N_c - 2)),  
    
    d_post = m_post/sd_pool, 
    vd_post = (1/N_t + 1/N_c) + d_post^2/(2*df_ind),
    Wd_post = (1/N_t + 1/N_c), 
    
    J = 1 - 3/(4*df_ind-1),
    
    g_post = J * d_post,
    vg_post = (1/N_t + 1/N_c) + g_post^2/(2*df_ind),
    Wg_post = Wd_post,
    
    # Account for the fact that some outcomes are on different scales
    m_diff_t = if_else(outcome == "Q_LES_Q", m_post_t - m_pre_t, (m_post_t - m_pre_t) * -1),
    m_diff_c = if_else(outcome == "Q_LES_Q", m_post_c - m_pre_c, (m_post_c - m_pre_c) * -1),
    
    d_DD = (m_diff_t - m_diff_c)/sd_pool,
    vd_DD = d_DD^2/F_val + d_DD^2/(2*df_ind),
    Wd_DD = d_DD^2/F_val,
    
    g_DD = J * d_DD, 
    vg_DD = g_DD^2/F_val + g_DD^2/(2*df_ind),
    Wg_DD = g_DD^2/F_val
    
  )
