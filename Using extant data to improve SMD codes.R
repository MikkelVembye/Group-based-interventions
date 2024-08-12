library(dplyr)
library(ggplot2)
library(stringr)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 310)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE) # Avoids summarize info from tidyverse

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

ggplot(data.frame(x = c(0, 100)), aes(x)) + 
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), linetype = "dashed", color = col)
  }, 
  # enter means, standard deviations and colors here
  mean = dat_restricted$m_post_c, 
  sd = dat_restricted$sd_post_c,
  col = "black"
  ) + theme_bw()



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
sd_population_GAF <- sqrt(sigma2_WA) + sqrt(sigma2_B)

# dplyr version (pre-test sd and means)

GAF_res <- 
  dat_restricted |>
  mutate(
    sd_pool_pre = sqrt(((N_t-1)*sd_pre_t^2 + (N_c-1)*sd_pre_c^2)/(N_t + N_c - 2)),  
  ) |> 
  summarise(
    N = sum(N_total),
    N_0 = sum(N_c),
    J = n_distinct(study),
    sigma2_WA = sum((N_total-2)*sd_pool_post^2)/(N-2*J),
    m_post_mean_c = mean(m_post_c),
    SSB0 = sum(N_c*(m_post_c - m_post_mean_c)^2),
    MSB0 = SSB0/(J-1),
    n0_star = N_0 - (sum(N_c^2/N_0)/J-1),
    
    #Equation 12
    sigma2_B = (MSB0 - sigma2_WA)/n0_star,
    sigma2_TA = ((n0_star-1)/n0_star) * sigma2_WA + (MSB0/n0_star),
    sd_population_GAF = sqrt(sigma2_TA),
    n_star = (N_0 - sum(N_c^2/N_0))/(J-1),
    c1 = (n_star-1)*sigma2_WA/(n_star*(N-2*J)),
    c2 = (sigma2_WA + n_star * sigma2_B)/(n_star*(J-1)),
    v1 = N_0-2*J,
    v2 = J-1,
    # Eq. 29
    df_pop = (c1*v1 + c2*v2)^2/(c1^2*v1 + c2^2+v2),
    WaboveT = sigma2_WA/sigma2_TA
    
  ); GAF_res

saveRDS(GAF_res, "ES calc/GAF_res.rds")




