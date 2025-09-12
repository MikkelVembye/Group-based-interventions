library(metaselection)
library(dplyr)

reint_ma_dat <- 
  reint_ma_dat |> 
  mutate(
    Wse_pop = sqrt(Wgt_pop)
  )

library(future)

# Overall reintegration
plan(multisession, workers = 4L)

mod_3PSM_mod_overall <- 
  metaselection::selection_model(
    data = reint_ma_dat, 
    yi = gt_pop,
    sei = Wse_pop,
    cluster = study,
    selection_type = "step",
    steps = 0.025,
    estimator = "CML",
    vcov_type = "robust",
    CI_type = "percentile",
    bootstrap = "two-stage",
    R = 99
  )
plan(sequential)
mod_3PSM_mod_overall

plan(multisession, workers = 4L)

mod_4PSM_mod_overall <- 
  metaselection::selection_model(
    data = reint_ma_dat, 
    yi = gt_pop,
    sei = Wse_pop,
    cluster = study,
    selection_type = "step",
    steps = c(0.025, 0.500),
    estimator = "CML",
    vcov_type = "robust",
    CI_type = "percentile",
    bootstrap = "two-stage",
    R = 99
  )
plan(sequential)
mod_4PSM_mod_overall


# Across outcomes
plan(multisession, workers = 4L)

mod_3PSM_mod <- 
  metaselection::selection_model(
    data = reint_ma_dat, 
    yi = gt_pop,
    sei = Wse_pop,
    cluster = study,
    selection_type = "step",
    steps = 0.025,
    estimator = "CML",
    vcov_type = "robust",
    CI_type = "percentile",
    bootstrap = "two-stage",
    mean_mods = ~ outcome_type + prereg_c - 1,
    R = 1999
  )
plan(sequential)
mod_3PSM_mod

plan(multisession, workers = 4L)

mod_4PSM_mod <- 
  metaselection::selection_model(
    data = reint_ma_dat, 
    yi = gt_pop,
    sei = Wse_pop,
    cluster = study,
    selection_type = "step",
    steps = c(0.025, 0.500),
    estimator = "CML",
    vcov_type = "robust",
    CI_type = "percentile",
    bootstrap = "two-stage",
    mean_mods = ~ outcome_type + prereg_c - 1,
    R = 1999
  )
plan(sequential)
mod_4PSM_mod