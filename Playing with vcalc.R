
library(tidyverse)
library(metafor)
library(clubSandwich)

reintegration_dat <- readRDS("reintegration_dat.rds")

# Playing with vcalc 

ES_dat_crawford <-
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

ES_dat_crawford$time <- rep(c(1,2), each = 2, 3) 


V_mat_crawford <- 
  metafor::vcalc(
    data = ES_dat_crawford,
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

V_mat_crawford_list <- blsplit(V_mat_crawford, ES_dat_crawford$study) 
V_mat_crawford_list

blsplit(V_mat_crawford, ES_dat_crawford$study) |> 
  lapply(cov2cor)

ES_dat_michalak <-
  reintegration_dat |> 
  filter(str_detect(study, "Micha")) |> 
  select(
    study, outcome = outcome, trt_id, trt_name, N_t, m_post_t, sd_post_t, 
    control, ctr_id, N_c, m_post_c, sd_post_c, gt_pop, vgt_pop, vary_id) |> 
  escalc(
    data = _,
    measure = "SMD",
    yi = gt_pop, vi = vgt_pop
  ) |> 
  mutate(esid = 1:n())

V_mat_michalak <- 
  metafor::vcalc(
    data = ES_dat_michalak,
    vi = vi, 
    cluster = study,
    obs = outcome, 
    grp1 = trt_name,
    w1 = N_t, 
    grp2 = control,
    w2 = N_c,
    rho = 0.8,
    sparse = FALSE
  )

blsplit(V_mat_michalak, ES_dat_michalak$study) |> 
  lapply(cov2cor)


ES_dat_schafer <-
  reintegration_dat |> 
  filter(str_detect(study, "Schaf")) |> 
  select(
    study, outcome = outcome, trt_id, trt_name, N_t, m_post_t, sd_post_t, 
    control, ctr_id, N_c, m_post_c, sd_post_c, gt_pop, vgt_pop, vary_id) |> 
  escalc(
    data = _,
    measure = "SMD",
    yi = gt_pop, vi = vgt_pop
  ) |> 
  mutate(esid = 1:n())

ES_dat_schafer$time <- rep(c(2,3,1), each = 2, 3)

V_mat_schafer <- 
  metafor::vcalc(
    data = ES_dat_schafer,
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

blsplit(V_mat_schafer, ES_dat_schafer$study) |> 
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

V_mat_list$`Crawford et al. 2012` <- V_mat_crawford
V_mat_list$`Michalak et al. 2015` <- V_mat_michalak
V_mat_list$`Schafer et al. 2019` <- V_mat_schafer

V_mat <- bldiag(V_mat_list)

blsplit(V_mat, reintegration_dat$study) |> 
  lapply(cov2cor)
