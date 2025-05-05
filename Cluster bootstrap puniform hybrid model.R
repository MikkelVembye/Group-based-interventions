# Cluster bootstrap puniform hybrid model

library(puniform)
library(metadat)   # for the example dataset
library(tidyverse) # for tidying
library(janitor)   # for tidying variable names
library(metafor)   # for meta-analysis
library(boot)      # for bootstrapping
library(tictoc) 


dat <- 
  metadat::dat.lehmann2018 |> 
  clean_names()  |> 
  mutate(
    study = str_split_fixed(short_title, pattern = "-", n = 2)[, 1],
    conventional = ifelse(preregistered == "Not Pre-Registered", 1, 0)
    ) |> 
  arrange(study)  |> 
  select(study, presentation = stimuli_presentation, conventional, yi, vi, everything())


#dat$conventional <- ifelse(dat$Preregistered == "Not Pre-Registered", 1, 0)

### They do the analyses for males and females. I only do these now for females,
# because there are more preregistered studies for females
red_romance_femalep <- dat[dat$gender == "Females", ]

### Prepare data for the analysis
#yi <- red_romance_femalep$yi
#vi <- red_romance_femalep$vi
#conventional <- red_romance_femalep$conventional

### Random-effects model that allows for better comparison, because Wald test
# for fixed effect and maximum likelihood estimator for tau^2
res_ml_red <- rma(yi = red_romance_femalep$yi, vi = red_romance_femalep$vi, method = "ML")
res_tau2_red <- confint(res_ml_red)

#optimizers <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")
#mod <- "Non-converged"
#i <- 1L
#
#
#while (!inherits(mod, "hybridoutput") & i <= 4L) {
#
#mod <- 
#  hybrid(
#    yi = red_romance_femalep$yi, 
#    vi = red_romance_femalep$vi, 
#    conventional = red_romance_femalep$conventional, 
#    side = "right",
#    control = list(optimizer = optimizers[i])
#  )
#
#i <- i + 1L
#
#}
#
#c(beta = mod$est, tau = sqrt(mod$tau2))
#
hybrid_red <- 
  hybrid(
    yi = red_romance_femalep$yi, 
    vi = red_romance_femalep$vi, 
    conventional = red_romance_femalep$conventional, 
    side = "right",
    control = list(optimizer = "Nelder-Mead")
  )

c(beta = hybrid_red$est, tau = sqrt(hybrid_red$tau2))



fit_hybrid_model <- 
  function(
    dat, 
    index = 1:nrow(dat) 
  ) {
  
  # take subset of data
  boot_dat_cluster <- dat[index, ]
  
  # expand to one row per effect size
  boot_dat <- tidyr::unnest(boot_dat_cluster, data)
  
  run_hybrid_model <- function(
    g, vg, conventional
  ) {
    
    optimizers <- c("Nelder-Mead", "BFGS", "L-BFGS-B", "CG", "SANN", "Brent")
    mod <- "Non-converged"
    i <- 1L
    
    while (!inherits(mod, "hybridoutput") & i <= 6L) {
      
      mod <- 
        hybrid(
          yi = g, 
          vi = vg, 
          conventional = conventional, 
          side = "right",
          control = list(optimizer = optimizers[i])
        ) |> 
        suppressWarnings()
      
      i <- i + 1L
      
    }
    
    c(beta = mod$est, tau = sqrt(mod$tau2))
    
  }
  
  run_hybrid_model <- purrr::possibly(run_hybrid_model, otherwise = rep(NA_real_, 2))
  
  
  # fit selection model, return vector
  run_hybrid_model(g = boot_dat$yi, vg = boot_dat$vi, boot_dat$conventional)
  
}

red_romance_femalep_nested <- nest_by(red_romance_femalep, study, .key = "data")
#red_romance_femalep_nested |> glimpse()

fit_hybrid_model(red_romance_femalep_nested)

# Generate bootstrap
set.seed(05052025)

ncpus <- parallel::detectCores() - 1

tic()

# Make work in parallell
boots <- boot(
  data = red_romance_femalep_nested,    # nested dataset
  statistic = fit_hybrid_model,         # function for fitting selection model
  R = 99,                             # number of bootstraps
  parallel = "multicore", ncpus = ncpus
)

time_seq <- toc()

est <- boots$t0

boot_SE <- apply(boots$t, 2, sd, na.rm = TRUE)  

model_SE <- with(hybrid_red, c(se[1], se[2] / (2 * sqrt(tau2))))

res <- tibble(
  Parameter = names(est),
  Est = est,
  `SE(bootstrap)` = boot_SE,
  `SE(model)` = model_SE,
  `SE(bootstrap) / SE(model)` = boot_SE / model_SE
)

res

# Est
ci_est <- boot.ci(boots, type = "perc", index = 1) # For overall average ES
ci_est$percent[4:5] 
# Tau
ci_tau <- boot.ci(boots, type = "perc", index = 2) # For heterogeneity
ci_tau$percent[4:5]

#-------------------------------------------------------------------------------






