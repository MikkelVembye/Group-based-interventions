# Cluster bootstrap puniform hybrid model

library(puniform)
library(metadat)   # for the example dataset
library(tidyverse) # for tidying
library(janitor)   # for tidying variable names
library(metafor)   # for meta-analysis
library(boot)      # for bootstrapping
library(tictoc) 


#-------------------------------------------------------------------------------
# Make it work on the reintegration data
reint_dat <- readRDS("reint_ma_dat.rds")

hybrid_reint <- 
  hybrid(
    yi = reint_dat$gt_pop, 
    vi = reint_dat$Wgt_pop, 
    conventional = reint_dat$conventional, 
    side = "right",
    control = list(optimizer = "Nelder-Mead")
  )

c(beta = hybrid_reint$est, tau = sqrt(hybrid_reint$tau2))

fit_hybrid_model2 <- 
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
    run_hybrid_model(g = boot_dat$gt_pop, vg = boot_dat$Wgt_pop, boot_dat$conventional)
    
}

reint_dat_nested <- nest_by(reint_dat, study, .key = "data")
#red_romance_femalep_nested |> glimpse()

fit_hybrid_model2(reint_dat_nested)

# Generate bootstrap
set.seed(05052025)
R <- 1999
ncpus <- parallel::detectCores() - 1

tic()

# Make work in parallell
boots_overall <- boot(
  data = reint_dat_nested,    # nested dataset
  statistic = fit_hybrid_model2,         # function for fitting selection model
  R = R,                             # number of bootstraps
  parallel = "multicore", ncpus = ncpus
)

time_seq <- toc()

est <- boots_overall$t0

boot_SE <- apply(boots_overall$t, 2, sd, na.rm = TRUE)  
boot_pval_overall <- sum(abs(boots_overall$t[,1]-1) > abs(boots_overall$t0[1]-1))/(1+boots_overall$R)

model_SE <- with(hybrid_reint, c(se[1], se[2] / (2 * sqrt(tau2))))

ci_est <- boot.ci(boots_overall, type = "perc", index = 1) # For overall average ES
ci_est$percent[4:5] 
# Tau
ci_tau <- boot.ci(boots_overall, type = "perc", index = 2) # For heterogeneity
ci_tau$percent[4:5]


hyema_overall <- 
  tibble(
    Parameter = names(est),
    Est = est,
    SE_bootstrap = boot_SE,
    CIL_bootstrap = c(ci_est$percent[4], ci_tau$percent[4]),
    CIU_bootstrap = c(ci_est$percent[5], ci_tau$percent[5]),
    p_val_bootstrap = boot_pval_overall,
    SE_model = model_SE,
    SE_bootstrap_vs_SE_model = boot_SE / model_SE,
    R = R
  )

hyema_overall

saveRDS(boots_overall, "GitHub repos/Group-based-interventions/Boostrap results/boots_overall_reint.rds")
saveRDS(hyema_overall, file = "GitHub repos/Group-based-interventions/Boostrap results/hyema_overall_reint.rds")

# Make work with moderator

# Test run function
run_hybrid_model <- function(
    g, vg, conventional, moderator
) {
  
  optimizers <- c("Nelder-Mead", "BFGS", "L-BFGS-B", "CG", "SANN", "Brent")
  mod <- "Non-converged"
  i <- 1L
  
  mod <- moderator
  
  while (!inherits(mod, "hybridoutput") & i <= 6L) {
    
    mod <- 
      hybrid(
        yi = g, 
        vi = vg, 
        conventional = conventional, 
        side = "right",
        mods = ~ mod -1,
          control = list(optimizer = optimizers[i])
      ) |> 
      suppressWarnings()
    
    i <- i + 1L
    
  }
  
  c(betaone = mod$est[1], betatwo = mod$est[2], tau = sqrt(mod$tau2))
  
}


fit_hybrid_reintegration <- 
  function(
    dat, 
    index = 1:nrow(dat) 
  ) {
    
    # take subset of data
    boot_dat_cluster <- dat[index, ]
    
    # expand to one row per effect size
    boot_dat <- tidyr::unnest(boot_dat_cluster, data)
    
    run_hybrid_model <- function(
      g, vg, conventional, moderator
    ) {
      
      optimizers <- c("Nelder-Mead", "BFGS", "L-BFGS-B", "CG", "SANN", "Brent")
      mod <- "Non-converged"
      i <- 1L
      
      subgroup <- moderator
      
      while (!inherits(mod, "hybridoutput") & i <= 6L) {
        
        mod <- 
          puniform::hybrid(
            yi = g, 
            vi = vg, 
            conventional = conventional, 
            side = "right",
            mods = ~ subgroup -1,
            control = list(optimizer = optimizers[i])
          ) |> 
          suppressWarnings()
        
        i <- i + 1L
        
      }
      
      X <- stats::model.matrix(~ -1 + subgroup)
      n <- nrow(X)
      p <- ncol(X)
      
      beta_hat <- mod$est
      
      # Obtain residuals
      residuals <-  g - X %*% beta_hat
      
      # Calculate vcov
      wi <- 1/(vg + mod$tau2)
      W <- diag(wi, nrow = n, ncol = n)
      
      sigma_sq_hat <- sum(wi*residuals^2) / (n - p)
      
      vcov <- sigma_sq_hat * solve(t(X) %*% W %*% X)
      
      # Obtain q, Q, and the F value
      q <- length(beta_hat)
      
      C_mat <- diag(1L, nrow = q)[1:2,,drop=FALSE]
      
      inverse_vcov <- chol2inv(chol(C_mat %*% vcov %*% t(C_mat)))
      
      C_beta <- (C_mat %*% beta_hat - matrix(c(0,0), ncol = 1L))
      
      Q <- as.numeric(t(C_beta) %*% inverse_vcov %*% C_beta)
      
      #Naive-F
      F_naive <- Q/q
      c(
        F_val = F_naive, 
        Alchohol = beta_hat[1], 
        Hope = beta_hat[2], 
        Lone = beta_hat[3],
        Selfest = beta_hat[4],
        Social = beta_hat[5], 
        Wellbeing = beta_hat[6],
        Other = beta_hat[7],
        tau = sqrt(mod$tau2)
      )
      
      #c(betaone = mod$est[1], betatwo = mod$est[2], tau = sqrt(mod$tau2))
      
    }
    
    run_hybrid_model <- purrr::possibly(run_hybrid_model, otherwise = rep(NA_real_, 9))
    
    
    # fit selection model, return vector
    run_hybrid_model(
      g = boot_dat$gt_pop, 
      vg = boot_dat$Wgt_pop, 
      conventional =  boot_dat$conventional,
      moderator = boot_dat$outcome_type
      )
    
}

reint_dat_nested <- nest_by(reint_dat, study, .key = "data")

tic()
fit_hybrid_reintegration(reint_dat_nested)
toc()

# Generate bootstrap
set.seed(12092025)

ncpus <- parallel::detectCores() - 1

tic()

R <- 1999

# Make work in parallell
boots_outcome <- 
  boot(
    data = reint_dat_nested,               # nested dataset
    statistic = fit_hybrid_reintegration,  # function for fitting selection model
    R = R,                                # number of bootstraps
    parallel = "multicore", 
    ncpus = ncpus
  )


time_seq <- toc()

F_boot_pval_outcome <- (1/R)*sum(boots_outcome$t[,1] > boots_outcome$t0[1], na.rm = TRUE)
F_boot_pval_outcome

est_outcome <- boots_outcome$t0

boot_SE_outcome <- apply(boots_outcome$t[,2:8], 2, sd, na.rm = TRUE)  

boot_pval_outcome <- 
  map(
    2:8, ~ {
    sum(abs(boots_outcome$t[,.x]-1) > abs(boots_outcome$t0[.x]-1), na.rm = TRUE)/(1+boots_outcome$R)  
  }) |> 
  list_c()

#
#model_SE <- with(hybrid_red, c(se[1], se[2] / (2 * sqrt(tau2))))
#
#res <- tibble(
#  Parameter = names(est),
#  Est = est,
#  `SE(bootstrap)` = boot_SE,
#  `SE(model)` = model_SE,
#  `SE(bootstrap) / SE(model)` = boot_SE / model_SE
#)
#
#res

# Est
cis_l_outcome <- map(2:8, ~ {
  obj <- boot.ci(boots_outcome, type = "perc", index = .x) |> suppressWarnings()
  obj$percent[4]
}) |> list_c()

cis_u_outcome <- map(2:8, ~ {
  obj <- boot.ci(boots_outcome, type = "perc", index = .x) |> suppressWarnings()
  obj$percent[5]
}) |> list_c()

#ci_alcohol <- boot.ci(boots, type = "perc", index = 2) 
#ci_alcohol$percent[4:5] 
## Tau
#ci_hope <- boot.ci(boots, type = "perc", index = 3) 
#ci_hope$percent[4:5]
#
#ci_social <- boot.ci(boots, type = "perc", index = 4) 
#ci_social$percent[4:5] 
## Tau
#ci_well <- boot.ci(boots, type = "perc", index = 5) 
#ci_well$percent[4:5]

hyema_overcome <- tibble(
  Parameter = names(est_outcome[2:8]),
  Est = est_outcome[2:8],
  SE_bootstrap = boot_SE_outcome,
  CIL_bootstrap = cis_l_outcome,
  CIU_bootstrap = cis_u_outcome, 
  p_val_bootstrap = boot_pval_outcome,
  R = R
)

hyema_overcome

F_boot_pval_outcome

saveRDS(boots_outcome, file = "GitHub repos/Group-based-interventions/Boostrap results/boots_outcome_reint.rds")
saveRDS(hyema_overcome, file = "GitHub repos/Group-based-interventions/Boostrap results/hyema_overcome.rds")
saveRDS(F_boot_pval_outcome, "GitHub repos/Group-based-interventions/Boostrap results/F_boot_pval_outcome.rds")


