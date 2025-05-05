#-------------------------------------------
#      This script cleans the sim data
#-------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)

#-------------------------------------------
#      create data for graphing
#-------------------------------------------

# simulation results excluding WILS

res <- readRDS("simulations/results/SRres_rep2000.rds") # this is not complete

res_add1 <- readRDS("simulations/results/SRres_additional_2_rep2000.rds")
res_add2 <- readRDS("simulations/results/SRres_additional_k10_rep2000.rds")
res_add3 <- readRDS("simulations/results/SRres_additional_k100_rep2000.rds")

res_no_wils <- 
  rbind(res, res_add1, res_add2, res_add3) %>% 
  filter(!(method %in% c("uni_wils", "mv_wils"))) %>% 
  mutate(k_stop = "NA")

## check each method should have 2304 conditions
res_no_wils %>% 
  group_by(method) %>% 
  summarise(n = n()) %>% 
  filter(n != 2304) 
  

# WILS results

res_wils0 <- readRDS("simulations/WILS/sim-res-WILS-incomplete.rds")

res_wils0 %>% 
  group_by(method, k_stop) %>% # 2304 for each
  summarise(n = n()) # still missing 265 conditions for generally large k and small weights or wt = 1

res_wils1 <- readRDS("simulations/WILS/sim-res-WILS-additional1.rds")
res_wils2 <- readRDS("simulations/WILS/sim-res-WILS-additional2.rds")
res_wils2 <- 
  res_wils2 %>%
  mutate(
    mu = param,
    cor_sd = 0.05,
    id_start = 0,
    paste_ids = TRUE,
    omega = tau / sqrt(2),
    k_multiplier = "NA",
    iterations = 2000,
    seed = "NA",
  ) %>% 
  dplyr::select(-param)
res_wils <- rbind(res_wils0, res_wils1, res_wils2)

res_wils %>% 
  group_by(method, k_stop) %>% # 2304 for each
  summarise(n = n())

res_wils %>% 
  group_by(method, k_stop, k, mu, cor_mu, weights) %>% 
  summarise(n = n()) %>% 
  filter(n != 4)

# combine results
res <- rbind(res_no_wils, res_wils)

res_dat <- 
  res %>%   
  mutate(
    method = ifelse(method == "TF_R0_left", "TF",
                    ifelse(!method %in% c("p-uniform", "p-unistar"),
                           toupper(method), method)),
    method = ifelse(method == "MV_PET_PEESE", "MV_PET-PEESE",
                    ifelse(method == "UNI_PET_PEESE", "UNI_PET-PEESE",
                           ifelse(method == "UNI_RE_ISW", "UNI_RE-ISW",
                                  ifelse(method == "p-unistar", "p-uniform*",
                                         method)))),
    weight_length = purrr::map_dbl(weights, function(x) length(x)),
    sel_mechanism = if_else(weight_length == 1, "One-step selection", "Two-step selection"),
    # it is the first weight for the 4PSM, lambda2 = 0.5 * lambda1
    wts = purrr::map_dbl(weights, function(x) x[1]),
    wts = as.factor(wts),
    # method = factor(method),
    # k = factor(k),
    # tau = factor(tau)
    # wts = ifelse(sel_mechanism == "3PSM", wts, paste0("(",wts, ", ", wts/2 ,")"))
  )


# split one-step and two-step sim results
res_one_step <- res_dat |> filter(sel_mechanism == "One-step selection")
res_two_step <- res_dat |> filter(sel_mechanism == "Two-step selection") 

# check whether the results are complete
res_one_step |>
  group_by(method) |> 
  summarise(n = n()) |> 
  arrange(n) |>
  print(n = 22)

# check all method other than WILS: looking good
res_one_step %>% 
  filter(k_stop == "NA") %>% 
  group_by(mu, tau, k, cor_mu, method) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(n != 6)

res_two_step %>%  
  filter(k_stop == "NA") %>%
  group_by(mu, tau, k, cor_mu, method) %>% # should have 6 weights
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(n != 6) %>% 
  arrange(n)

#-------------------------------------------
#      prepare data for graphing
#-------------------------------------------

dat_one_step <- 
  res_one_step |> 
  filter(cor_mu == 0.4, cor_sd == 0.05)
unique(dat_one_step$method)

dat_sel_one_step <- 
  dat_one_step |> 
  filter(method %in% c("3PSM", "4PSM", "p-uniform", "p-uniform*"))

dat_uni_one_step <- 
  dat_one_step |>
  filter(str_detect(method, "^UNI")|method %in% 
           c("p-uniform*", "3PSM", "4PSM", "p-uniform","TF")) |> 
  mutate(method = if_else(method %in% c("p-uniform", "p-uniform*", "3PSM","4PSM","TF"),
                          method, sub("^[^_]*_", "", method)))
unique(dat_uni_one_step$method)

dat_uni_pp_one_step <- 
  dat_uni_one_step |> 
  filter(method %in% toupper(c("pet", "peese", "pet-peese")))
unique(dat_uni_pp_one_step$method)

# RQ1: include 3PSM, 4PSM p-uniform*, RE-ISW, EK PET-PEESE, WAAP and WILS
main_dat_uni_one_step <- 
  dat_uni_one_step |> 
  filter(method %in% c("3PSM", "4PSM", "p-uniform*", "RE-ISW", 
                       "EK", "PET-PEESE", "WAAP", "WILS"))

# RQ2: include CHE (RE), CHE-ISCW (UNI_RE-ISW), EK, PET-PEESE, WAAP, WILS
main_dat_reg_one_step <- 
  dat_one_step |> 
  filter(! method %in% c("3PSM", "4PSM", "TF", "p-uniform", "p-uniform*", "UNI_FE")) |>
  mutate(
    method = ifelse(method == "CHE", "MV_RE",
                    ifelse(method == "CHE-ISCW", "MV_RE-ISW",
                           method)),
   dependence = str_extract(method, "[^_]+"),
   method = sub("^[^_]*_", "", method)
 )
unique(main_dat_reg_one_step$method)

dat_mv_pp_one_step <- 
  main_dat_reg_one_step |> 
  filter(method %in% c("PET", "PEESE", "PET-PEESE"),
         dependence == "MV")

# RQ3: only include 3PSM, CHE-ISCW, adapted PET-PEESE, EK
main_dat_comp <- 
  res_dat |> 
  filter(
    cor_mu == 0.4, cor_sd == 0.05,
    method %in% c("3PSM", "CHE-ISCW", "MV_PET-PEESE", "MV_EK", "4PSM"))

# save the data
save(res_dat, res_one_step, 
     dat_one_step, dat_sel_one_step, dat_uni_one_step, 
     dat_uni_pp_one_step, main_dat_uni_one_step, 
     main_dat_reg_one_step, dat_mv_pp_one_step, main_dat_comp,
     file = "simulations/sim-res.Rdata")
