library(dplyr)
library(stringr)


dat <- 
  readRDS("GAF_dat.rds") |> 
  filter(str_detect(outcome, "GAF|Global")) |> 
  relocate(study) |> 
  select(
    study, outcome, treatment, timing, N_t, N_c, N_total, sd_pre_t, sd_pre_c, 
    sd_post_t, sd_post_c, sd_pool_post = sd_pool, m_post_t, m_post_c,
    m_pre_t, m_pre_c,
    gt_DD, vgt_DD, vary_id
  ) |> 
  mutate(
    cluster = if_else(is.na(treatment), "One treatment", treatment),
    cluster_id = paste(study, "/", cluster)
  )

dat_restricted <- 
  dat |> 
  group_by(study) |> 
  filter(row_number() == 1) |> 
  ungroup()

N <- sum(dat_restricted$N_total)
N_0 <- sum(dat_restricted$N_c)
J <- n_distinct(dat_restricted$study)

#Equation 10
sigma2_WA <- 
  dat_restricted |> 
  summarise(
    sigma2_WA = sum((N_total-2)*sd_pool_post^2)/(N-2*J)
  ) |> 
  pull(sigma2_WA)

# Equation 11
SSB0 <- 
  dat_restricted |> 
  summarise(
    m_post_mean_c = mean(m_post_c),
    SSB0 = sum(N_c*(m_post_c - m_post_mean_c)^2)
  ) |> 
  pull(SSB0)

MSB0 <- SSB0/(J-1)

n0_star <- N_0 - (sum(dat_restricted$N_c^2/N_0)/J-1)

#Equation 12
sigma2_B <- (MSB0 - sigma2_WA)/n0_star

# Equation 13
sigma2_TA <- ((n0_star-1)/n0_star) * sigma2_WA + (MSB0/n0_star)
sd_population_GAF <- sqrt(((n0_star-1)/n0_star) * sigma2_WA) + sqrt(MSB0/n0_star)
sd_population_GAF_alt = sqrt(sigma2_WA) + sqrt(sigma2_B)
