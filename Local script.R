# Difference between local and cloud data. 

RNGkind("L'Ecuyer-CMRG") # My naive try to steer seed

library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidyr)
library(rempsyc)
library(purrr)
library(metafor)
library(clubSandwich)
library(knitr)
library(kableExtra)
library(forcats)
library(future)
library(furrr)
library(gt)
library(fastDummies)
library(rlang)
library(wildmeta)
library(tictoc)

reintegration_dat <- readRDS("reintegation_dat.rds")

# Read: reint_ma_dat = reintegrational meta-analysis data
reint_ma_dat <- 
  reintegration_dat |> 
  select(
    # Various types of effect size estimates 
    study, gt_pop, vgt_pop, Wgt_pop, gt, vgt, Wgt, g, vg, d, vd, gt_post, vgt_post,
    
    # Categorical moderators and control variables 
    analysis_plan, schizophrenia, CBT_intervention = CBT_int, prereg_chr, conventional, 
    test_type, analysis_strategy, QES_design, control, D1:D7, overall_rob, 
    
    # Continuous moderators and control variables
    age_mean, male_pct, total_number_of_sessions, sessions_per_week, duration_in_weeks,
    time_after_end_intervention_weeks, time_from_baseline_weeks
    
  ) |> 
  mutate(
    esid = 1:n(),
    
    # Outcome variables
    outcome_type = case_match(
      analysis_plan, 
      c("Employment", "Physical health", "Psychiatric hospitalization") ~ "Other",
      .default = analysis_plan
    ),
    
    outcome_type = fct_relevel(outcome_type, sort),
    outcome_type = fct_relevel(outcome_type, "Other", after = Inf),
    
    prereg_c = conventional - mean(conventional),
    
    schizophrenia_in_sample = if_else(schizophrenia == "Schizophrenia", "Yes", "No"),
    schizophrenia_in_sample = factor(schizophrenia_in_sample, levels = c("Yes", "No")),
    
    schizo_in_sample = if_else(schizophrenia == "Schizophrenia", 1, 0),
    schizo_c = schizo_in_sample - mean(schizo_in_sample),
    
    cbt = if_else(CBT_intervention == "CBT", 1, 0),
    cbt_c = cbt - mean(cbt),
    
    test_type = if_else(test_type != "Clinician-rated measure", "Self reported/raw events", test_type),
    
    clin_measure = if_else(test_type == "Clinician-rated measure", 1, 0),
    clinical_c = clin_measure - mean(clin_measure),
    
    tot = if_else(analysis_strategy == "TOT", 1, 0),
    tot_c = tot - mean(tot),
    
    qes = if_else(QES_design == "QES", 1, 0),
    qes_c = qes - mean(qes),
    
    indi_treat_ctr = if_else(str_detect(control, "Ind"), "Individual treatment", "TAU and Waitlist"),
    
    crt_grp = if_else(str_detect(control, "Ind"), 1, 0),
    crt_grp_c = crt_grp - mean(crt_grp),
    
    risk_of_bias = if_else(overall_rob == "Serious/High", "Serious/High", "Low/Some concerns/Moderate"),
    
    sessions_per_week = if_else(is.na(sessions_per_week), mean(sessions_per_week, na.rm = TRUE), sessions_per_week),
    male_pct = if_else(is.na(male_pct), mean(male_pct, na.rm = TRUE), male_pct),
    
    age_c = age_mean - 40,
    male_c = male_pct/100 - 0.5,
    sessions_c = sessions_per_week - 1,
    duration_c = duration_in_weeks - 12,
    fu_time_c = time_after_end_intervention_weeks - 1,
    
    study = stringi::stri_trans_general(study, "Latin-ASCII"),
    study = fct_relevel(study, sort)
    
  ) |> 
  arrange(study) |> 
  mutate(
    study = as.character(study)
  )

reint_ma_dat <- 
  fastDummies::dummy_cols(reint_ma_dat, select_columns = "outcome_type", omit_colname_prefix = TRUE) |> 
  rename(
    alcohol = `Alcohol and drug abuse/misuse`, 
    hope = `Hope, empowerment & self-efficacy`, 
    lonely = Loneliness,
    self_est = `Self-esteem`,
    social_fun = `outcome_type_Social functioning (degree of impairment)`,
    wellbeing = `Wellbeing and quality of life`,
    other = Other
  ) |> 
  mutate(
    across(where(is.double), as.double),
    across(where(is.integer), as.integer)
  )

# It used to later recontract the call in the rma.obj
attr(reint_ma_dat , "data_name") <- "reint_ma_dat"

saveRDS(reint_ma_dat, "reint_ma_dat.rds")

# Fitting meta-analysis model 
rho <- 0.8

V_mat <- 
  vcalc(
    vi = vgt_pop, 
    cluster = study, 
    obs = esid, 
    data = reint_ma_dat, 
    rho = rho
  )

outcome_obj <- 
  rma.mv(
    yi = gt_pop ~ outcome_type - 1,
    V = V_mat, 
    random = list(~ outcome_type | study, ~ outcome_type | esid),
    struct = c("DIAG", "DIAG"),
    data = reint_ma_dat,
    sparse=TRUE
  )

saveRDS(outcome_obj, file = "outcome_obj.rds")

# Cluster wild bootstrapping
plan(sequential)
plan()


plan(multisession)
wildmeta::Wald_test_cwb(
  full_model = outcome_obj,
  constraints = wildmeta::constrain_equal(1:7),
  R = 19,
  seed = 28082025L
)
plan(sequential)
plan()

