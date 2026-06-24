library(metafor)
library(metaselection)

dat <- readRDS("Data/prereg_dat_to_james.rds")
dat$notprereg_I <- as.integer(dat$prereg_chr == "Not preregistered")

funnel(x = dat$gt_pop, sei = dat$Wse_pop)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Quick-and-dirty first fit with cluster-robust standard errors ----
# (Not to be trusted for inference)

selmod_CRVE <- 
  selection_model(
    data = dat,                   # dataset
    yi = gt_pop,                  # effect size estimate
    sei = Wse_pop,                # standard error of ES estimate
    cluster = study,              # cluster ID variable
    selection_type = "step",      # step function model
    steps = c(.025),              # threshold value(s) for step function model
    sel_mods = ~ 0 + notprereg_I, # Predictors of selection parameter(s)
    # Dropping the intercept means that selection will only apply to effects with notprereg_I == 1
    priors = NULL,                # Set to NULL to use composite maximum likelihood
    estimator = "CML",            # CML estimator is recommended (and default)
    vcov_type = "robust",         # CRVE standard errors
    CI_type = "large-sample"      # large-sample CIs
  )

summary(selmod_CRVE)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Better standard errors with bootstrapping ----

# set up parallel processing
library(future)
plan(multisession, workers = 10L)

# set up progress bar
progressr::handlers(global = TRUE)

# Bootstrap selection model
selmod_boot <- 
  selection_model(
    data = dat,                   # dataset
    yi = gt_pop,                  # effect size estimate
    sei = Wse_pop,                # standard error of ES estimate
    cluster = study,              # cluster ID variable
    selection_type = "step",      # step function model
    steps = c(0.025, 0.500),       # threshold value(s) for step function model
    sel_mods = ~ 0 + notprereg_I, # Predictors of selection parameter(s)
    # Dropping the intercept means that selection will only apply to effects with notprereg_I == 1
    priors = NULL,                # Set to NULL to use composite maximum likelihood
    estimator = "CML",            # CML estimator is recommended (and default)
    vcov_type = "robust",         # CRVE standard errors
    bootstrap = "two-stage",      # Two-stage clustered bootstrap (recommended)
    R = 1999,                     # Use ~2k bootstrap replications
    CI_type = c("large-sample", "percentile") # Percentile bootstrap confidence intervals (recommended)
  )

summary(selmod_boot)

# Turn off parallel processing
plan(sequential)
