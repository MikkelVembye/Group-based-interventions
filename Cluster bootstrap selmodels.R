# Cluster bootstrap selmodel and punifor

library(metadat)   # for the example dataset
library(tidyverse) # for tidying
library(janitor)   # for tidying variable names
library(metafor)   # for meta-analysis
library(boot)      # for bootstrapping
library(tictoc)    # for keeping time

lehmann_dat <- 
  dat.lehmann2018 %>%
  clean_names() %>%
  mutate(study = str_split_fixed(short_title, pattern = "-", n = 2)[, 1]) %>%
  arrange(study) %>%
  select(study, presentation = stimuli_presentation, yi, vi, everything())

# Estimate random effects model
RE_mod <- rma.uni(lehmann_dat$yi, vi = lehmann_dat$vi, method = "ML")

# Calculate cluster-robust standard errors
RE_robust <- robust(RE_mod, cluster = lehmann_dat$study, clubSandwich = TRUE)
RE_robust

# Selection model
RE_sel <- selmodel(RE_mod, type = "stepfun", steps = .025)
RE_sel

#-------------------------------------------------------------------------------
# Fit sel model 
fit_selmodel <- function(
    dat,   # dataset with one row per cluster
    index, # vector of indexes used to create the bootstrap sample
    ...    # any further arguments
) { 
  
  # take subset of data
  boot_dat <- dat[index,]
  
  # fit selection model
  
  # compile parameter estimates into a vector
  
}

#-------------------------------------------------------------------------------
lehmann_nested <- nest_by(lehmann_dat, study, .key = "data")

glimpse(lehmann_nested)

# Recover the full dataset
full_dat <-
  lehmann_nested %>%
  unnest(data)

full_dat %>% select(study, yi, vi) %>% glimpse()


#-------------------------------------------------------------------------------
fit_selmodel <- function(
    dat,   # dataset with one row per cluster
    index, # vector of indexes used to create the bootstrap sample
    ...    # any further arguments
) { 
  
  # take subset of data
  boot_dat_cluster <- dat[index, ]
  
  # expand to one row per effect size
  boot_dat <- tidyr::unnest(boot_dat_cluster, data)
  
  # fit selection model
  
  # compile parameter estimates into a vector
  
}

#-------------------------------------------------------------------------------

run_sel_model <- function(g, vg, steps = .025) {
  
  # initial random effects model
  RE_mod <- metafor::rma.uni(
    yi = g, vi = vg, method = "ML"
  )
  
  # fit selection model
  res <- metafor::selmodel(
    RE_mod, type = "stepfun", steps = steps,
    skiphes = TRUE, # turn off SE calculation
    skiphet = TRUE # turn off heterogeneity test
  )
  
  # compile parameter estimates into a vector
  c(beta = res$beta[,1], tau = sqrt(res$tau2), delta = res$delta[-1])
  
}

run_sel_model <- purrr::possibly(run_sel_model, otherwise = rep(NA_real_, 3))

fit_selmodel <- function(dat, 
                         index = 1:nrow(dat), 
                         steps = 0.025) {
  
  # take subset of data
  boot_dat_cluster <- dat[index, ]
  
  # expand to one row per effect size
  boot_dat <- tidyr::unnest(boot_dat_cluster, data)
  
  # fit selection model, return vector
  run_sel_model(g = boot_dat$yi, vg = boot_dat$vi, steps = steps)
  
}

lehmann_nested <- nest_by(lehmann_dat, study, .key = "data")

fit_selmodel(lehmann_nested)
