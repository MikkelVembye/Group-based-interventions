
library(tidyverse)
library(metafor)
library(clubSandwich)

reintegration_dat <- readRDS("reintegration_dat.rds")

# Playing with vcalc 

ES_dat <-
  reintegration_dat |> 
  filter(str_detect(study, "Crawford")) |> 
  select(
    study, outcome = outcome_measure, trt_id, trt_name, N_t, m_post_t, sd_post_t, 
    control, ctr_id, N_c, m_post_c, sd_post_c, gt_pop, vgt_pop, vary_id) |> 
  escalc(
    data = _,
    measure = "SMD",
    yi = gt_pop, vi = vgt_pop
  ) |> 
  mutate(esid = 1:n())

ES_dat$time <- rep(c(1,2), each = 2, 3) 



V_vcalc <- 
  metafor::vcalc(
    data = ES_dat,
    vi = vi, 
    cluster = study,
    type = outcome, 
    time1 = time,
    grp1 = trt_name,
    w1 = N_t, 
    grp2 = control,
    w2 = N_c,
    rho = 0.8,
    phi = 0.8,
    sparse = FALSE
  )

V_vcalc_list <- blsplit(V_vcalc, ES_dat$study) 
V_vcalc_list

blsplit(V_vcalc, ES_dat$study) |> 
  lapply(cov2cor)


V_mat <- 
  vcalc(
    vi = vgt_post, 
    cluster = study, 
    obs = esid, 
    rho = 0.8, 
    data = reintegration_dat
  )
  
V_mat_list <- blsplit(V_mat, reintegration_dat$study) 

V_mat_list$`Crawford et al. 2012` <- V_vcalc

V_mat <- bldiag(V_mat_list)

blsplit(V_mat, reintegration_dat$study) |> 
  lapply(cov2cor)
