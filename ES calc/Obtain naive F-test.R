################################################################################
##### ILLUSTRATION OF EXTENDED HYBRID METHOD WITH MODERATOR VARIABLE.      #####
##### THE ANALYSIS IS ILLUSTRATED BY USING THE META-ANALYSIS ON THE        #####
##### RED-ROMANCE HYPOTHESIS                                               #####
##### Author: Robbie C.M. van Aert, 19-09-2023                             #####
################################################################################

rm(list = ls())

################
### PACKAGES ###
################

#install.packages(c("puniform", "metafor"))
library(puniform)
library(metadat)   # for the example dataset
library(tidyverse) # for tidying
library(janitor)   # for tidying variable names
library(metafor)   # for meta-analysis
library(boot)      # for bootstrapping
library(tictoc) 


################################################################################
################################################################################
################################################################################

### Data from Lehmann et al. (2018). Red-romance hypothesis.
dat <- metadat::dat.lehmann2018
dat$conventional <- ifelse(dat$Preregistered == "Not Pre-Registered", 1, 0)

### They do the analyses for males and females. I only do these now for females,
# because there are more preregistered studies for females
red_romance_femalep <- dat[dat$Gender == "Females", ]

### Prepare data for the analysis
yi <- red_romance_femalep$yi
vi <- red_romance_femalep$vi
conventional <- red_romance_femalep$conventional

reintergation_dat <- readRDS("ES calc/reintergation_dat.rds") 
reintergation_dat

################################################################################

### Moderator analysis for the meta-analysis by Lehmann et al. Color_Match is 
# used as moderator. See the help file of dat.lehmann2018 or Lehmann et al. (2018)
# for more information about this moderator.

### Prepare data for the analysis
Color_Match <- red_romance_femalep$Color_Match

### Same results as on page 15 of Lehmann et al.
res_mod <- rma(yi = yi, vi = vi, mods = ~ Color_Match - 1, method = "ML", test = "knha")
res_mod

### Random-effects model that allows for better comparison, because Wald test
# for fixed effect and maximum likelihood estimator for tau^2
res_mod_ml <- rma(yi = yi, vi = vi, method = "ML", mods = ~ Color_Match)
res_mod_ml

### Apply hybrid method
hyb_mod_obj <- hybrid(yi = yi, vi = vi, conventional = conventional, side = "right", mods = ~ Color_Match - 1)
hyb_mod_obj 

# Obtaining naive F test

# Estimate this myself
vcov <- vcov(res_mod)

beta <- res_mod$b
q <- length(beta)

C_mat <- diag(1L, nrow = q)[1:2,,drop=FALSE]

inverse_vcov <- chol2inv(chol(C_mat %*% vcov %*% t(C_mat)))
inverse_vcov

C_beta <- (C_mat %*% res_mod$b - matrix(c(0,0), ncol = 1L))
C_beta

Q <- as.numeric(t(C_beta) %*% inverse_vcov %*% C_beta)
F_naive <- Q/q
F_naive

c(handcrafted = F_naive, metafor = res_mod$QM)

# GPT example
#set.seed(123)
#x1 <- rnorm(50)
#x2 <- rnorm(50)
#y <- 2 + 1.5*x1 - 3*x2 + rnorm(50, sd=2)

# Data frame

# Fixed effect 
res_mod_fix <- rma(yi = yi, vi = vi, mods = ~ Color_Match - 1, method="EE")
res_mod_fix

X <- res_mod$X |> as.matrix()

beta_hat <- coef(res_mod, type = "beta")
beta_hat

y_pred_manual <- X %*% beta_hat

residuals_manual <- yi - X %*% beta_hat

all.equal(as.numeric(residuals_manual), as.numeric(resid(res_mod)))
#
#n <- nrow(X)
#p <- ncol(X)
#
## Should be weighted
#sigma_sq <- sum(residuals_manual^2) / (n - p)
#
#vcov_manual <- as.numeric(sigma_sq) * solve(t(X) %*% X)
#vcov <- stats::vcov(res_mod)

#--------------------
# Play rma object
res_mod <- rma(yi = yi, vi = vi, mods = ~ Color_Match - 1, method = "ML", test = "knha")


X <- model.matrix(~ -1 + Color_Match, red_romance_femalep)
n <- nrow(X)
p <- ncol(X)

# Obtain estimate
beta_hat <- coef(res_mod)

# Obtain residual
residuals <- resid(res_mod)
# Residual handmade
residuals_manual <-  yi - X %*% beta_hat
all.equal(as.numeric(residuals_manual), as.numeric(residuals))

# Calculate vcov
wi <- 1/(vi + res_mod$tau2)
W <- diag(wi, nrow = n, ncol = n)

sigma_sq_hat <- sum(wi*residuals^2) / (n - p)

vcov_manual <- sigma_sq_hat * solve(t(X) %*% W %*% X)
vcov_builtin <- stats::vcov(res_mod)
all.equal(vcov_manual, vcov_builtin, check.attributes = FALSE)

# Obtain Q, q, the naive F test
q <- length(beta)

C_mat <- diag(1L, nrow = q)[1:2,,drop=FALSE]

inverse_vcov <- chol2inv(chol(C_mat %*% vcov_manual %*% t(C_mat)))
inverse_vcov

C_beta <- (C_mat %*% beta_hat - matrix(c(0,0), ncol = 1L))
C_beta

Q <- as.numeric(t(C_beta) %*% inverse_vcov %*% C_beta)
F_naive <- Q/q
F_naive

c(handcrafted = F_naive, metafor = res_mod$QM)

# Obtain F val for hybrid model

hyb_mod <- hybrid(yi = yi, vi = vi, conventional = conventional, side = "right", mods = ~ Color_Match - 1)
hyb_mod 

X <- model.matrix(~ -1 + Color_Match, red_romance_femalep)
n <- nrow(X)
p <- ncol(X)

beta_hat <- hyb_mod$est

# Obtain residual
#residuals <- resid(res_mod)
# Residual handmade
residuals_manual <-  yi - X %*% beta_hat
#all.equal(as.numeric(residuals_manual), as.numeric(residuals))

# Calculate vcov
wi <- 1/(vi + hyb_mod$tau2)
W <- diag(wi, nrow = n, ncol = n)

sigma_sq_hat <- sum(wi*residuals_manual^2) / (n - p)

vcov_manual <- sigma_sq_hat * solve(t(X) %*% W %*% X)
#vcov_builtin <- stats::vcov(res_mod)
#all.equal(vcov_manual, vcov_builtin, check.attributes = FALSE)

# Obtain Q, q, the naive F test
q <- length(beta_hat)

C_mat <- diag(1L, nrow = q)[1:2,,drop=FALSE]

inverse_vcov <- chol2inv(chol(C_mat %*% vcov_manual %*% t(C_mat)))
inverse_vcov

C_beta <- (C_mat %*% beta_hat - matrix(c(0,0), ncol = 1L))
C_beta

Q <- as.numeric(t(C_beta) %*% inverse_vcov %*% C_beta)
F_naive <- Q/q
F_naive

F_hybrid <- function(
    data, 
    reint_outcomes = TRUE,
    p_val = FALSE
  ){
  
  
  dat <- 
    data |> 
    select(author_year, gt, Wgt, Wse, conventional, analysis_plan) |> 
    arrange(analysis_plan) |> 
    mutate(
      outcome = factor(analysis_plan, levels = unique(analysis_plan))
    )
  
  hyb_mod <- hybrid(yi = dat$gt, vi = dat$Wgt, conventional = dat$conventional, side = "right", mods = ~ dat$outcome - 1)
  
  X <- model.matrix(~ -1 + outcome, dat)
  n <- nrow(X)
  p <- ncol(X)
  
  beta_hat <- hyb_mod$est
  
  # Obtain residuals
  residuals <-  dat$gt - X %*% beta_hat
  
  # Calculate vcov
  wi <- 1/(dat$Wgt + hyb_mod$tau2)
  W <- diag(wi, nrow = n, ncol = n)
 
  sigma_sq_hat <- sum(wi*residuals^2) / (n - p)
  
  vcov <- sigma_sq_hat * solve(t(X) %*% W %*% X)

  # Obtain q, Q, and the F value
  q <- length(beta_hat)
  
  C_mat <- diag(1L, nrow = q)[1:2,,drop=FALSE]
  
  inverse_vcov <- base::chol2inv(chol(C_mat %*% vcov %*% t(C_mat)))
  
  C_beta <- (C_mat %*% beta_hat - matrix(c(0,0), ncol = 1L))

  Q <- as.numeric(t(C_beta) %*% inverse_vcov %*% C_beta)
  
  F_naive <- Q/q
  
  # Naive-F
  pval <- pf(F_naive, df1 = q, df2 = n - 1, lower.tail = FALSE)
  
  if (reint_outcomes){
    res <- c(F_val = F_naive, Alcohol = beta_hat[1], Hope = beta_hat[2], Social = beta_hat[3], Wellbeing = beta_hat[4])
  } else {
    res <- c(F_val = F_naive, Anxiety = beta_hat[1], Depression = beta_hat[2], General = beta_hat[3], Symptoms = beta_hat[4])
  }
  
  if (p_val) res <- c(F_value = F_naive, F_pval = pval)
  
  res

}

F_hybrid(reintergation_dat)
#F_hybrid(reintergation_dat, p_val = TRUE)
#
#F_hybrid(mental_health_dat, reint_outcomes = FALSE)
#F_hybrid(mental_health_dat, reint_outcomes = FALSE, p_val = TRUE)

fit_hybrid_subgroup <- 
  function(
    dat, 
    reintegration = TRUE,
    index = 1:nrow(dat) 
  ) {
    
    # take subset of data
    boot_dat_cluster <- dat[index, ]
    
    # expand to one row per effect size
    boot_dat <- tidyr::unnest(boot_dat_cluster, data)
    
    F_hybrid <- function(
      data, 
      reint_outcomes = TRUE
    ){
      
      optimizers <- c("Nelder-Mead", "BFGS", "L-BFGS-B", "CG", "SANN", "Brent")
      hyb_mod <- "Non-converged"
      i <- 1L
      
      while (!inherits(hyb_mod, "hybridoutput") & i <= 6L) {
        hyb_mod <- 
          puniform::hybrid(
            yi = data$gt, 
            vi = data$Wgt, 
            conventional = data$conventional, 
            side = "right",
            control = list(optimizer = optimizers[i])#,
            #mods = ~ data$analysis_plan - 1
          ) |> 
          suppressWarnings()
        
        i <- i + 1L
      }
      
 #     X <- stats::model.matrix(~ -1 + analysis_plan, data)
 #     n <- nrow(X)
 #     p <- ncol(X)
      
      beta_hat <- hyb_mod$est
      
      c(beta1 = beta_hat[1])
#      # Obtain residuals
#      residuals <-  data$gt - X %*% beta_hat
#      
#      # Calculate vcov
#      wi <- 1/(data$Wgt + hyb_mod$tau2)
#      W <- diag(wi, nrow = n, ncol = n)
#      
#      sigma_sq_hat <- sum(wi*residuals^2) / (n - p)
#      
#      vcov <- sigma_sq_hat * solve(t(X) %*% W %*% X)
#      
#      # Obtain q, Q, and the F value
#      q <- length(beta_hat)
#      
#      C_mat <- diag(1L, nrow = q)[1:2,,drop=FALSE]
#      
#      inverse_vcov <- chol2inv(chol(C_mat %*% vcov %*% t(C_mat)))
#      
#      C_beta <- (C_mat %*% beta_hat - matrix(c(0,0), ncol = 1L))
#      
#      Q <- as.numeric(t(C_beta) %*% inverse_vcov %*% C_beta)
#      
#      #Naive-F
#      F_naive <- Q/q
#      c(F_val = F_naive)
      
      #if (reint_outcomes){
      #  res <- c(F_val = F_naive, Alcohol = beta_hat[1], Hope = beta_hat[2], Social = beta_hat[3], Wellbeing = beta_hat[4])
      #} else {
      #  res <- c(F_val = F_naive, Anxiety = beta_hat[1], Depression = beta_hat[2], General = beta_hat[3], Symptoms = beta_hat[4])
      #}
      #
      #res
      
    }
    
    F_hybrid <- purrr::possibly(F_hybrid, otherwise = rep(NA_real_, 2))
    
    
    # fit selection model, return vector
    F_hybrid(data = boot_dat, reint_outcomes = reintegration)
    
  }

reintergation_dat_nested <- nest_by(reintergation_dat, author_year, .key = "data")
fit_hybrid_subgroup(reintergation_dat_nested)
#  reintergation_dat |> 
#  arrange(analysis_plan) |> 
#  mutate(analysis_plan = factor(analysis_plan, levels = unique(analysis_plan))) |> 
#  nest_by(author_year, .key = "data")

#index <- 1:nrow(reintergation_dat_nested) 
#boot_dat_cluster <- reintergation_dat_nested[index, ]
#
## expand to one row per effect size
#boot_dat <- tidyr::unnest(boot_dat_cluster, data)
#F_hybrid(boot_dat)



# Generate bootstrap
set.seed(22052025)

ncpus <- parallel::detectCores() - 1

tic()

boots_F <- boot(
  data = reintergation_dat_nested,    # nested dataset
  statistic = fit_hybrid_subgroup,    # function for fitting selection model
  R = 10,                             # number of bootstraps
  parallel = "multicore", ncpus = ncpus
)

time_seq <- toc()




