.estimate_meta <- function(data,
                          rho = .8,
                          rand = NULL,
                          moderator = ~ 1,  # change to sd or var for pet peese
                          V_mat = NULL,
                          W = FALSE, # true for correlated fixed effects model
                          method,
                          modified = TRUE,
                          returnMod = FALSE) {
  require(metafor)
  
  
  if (modified == TRUE & is.null(V_mat)) {
    V_mat <- vcalc(Wgt, cluster = author_year, obs = esid, data = data, rho = rho)
  } else if (modified == FALSE & is.null(V_mat)) {
    V_mat <- vcalc(vgt, cluster = author_year, obs = esid, data = data, rho = rho)
  }
  
  if(W == TRUE) W <- solve(V_mat) else {W <- NULL}
  
  optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
  mod <- "Non-converged"
  i <- 1L
  
  while (!inherits(mod, "rma.mv") & i <= 4L) {
    if (!is.null(W)) {
      mod <- tryCatch(
        rma.mv(
          yi = gt,
          V = V_mat,
          W = W, 
          random = rand,
          mods = moderator,
          data = data,
          sparse = TRUE,
          control = list(optimizer=optimizers[i])
        ),
        error = function(e) "Non-converged"
      )
    } else {
      mod <- tryCatch(
        rma.mv(
          yi = gt,
          V = V_mat,
          random = rand,
          mods = moderator,
          data = data,
          sparse = TRUE,
          control = list(optimizer=optimizers[i])
        ),
        error = function(e) "Non-converged"
      )
    }
    i <- i + 1L
  }
  
  if (inherits(mod, "rma.mv")) { # rma.mv for CHE, PET, PEESE, WAAP, WILS
    mod <- robust(mod, cluster = author_year, clubSandwich = TRUE) 
    
    if (returnMod == TRUE) {
      res <- mod
    } else {
      res <- data.frame(
        param = "beta.intrcpt",
        est = as.numeric(mod$beta[1]),
        se = mod$se[1],
        lo = mod$ci.lb[1],
        hi = mod$ci.ub[1],
        p_val = mod$pval[1],
        R_conv = NA,
        method = method
      )
    }
  } else {
    res <- NULL
  }
  
  return(res)
  
}

.estimate_meta(data = reintergation_dat, rand = ~ 1 | author_year / esid, W = TRUE, method = "CHE-ISCW")


.fit_reg_mv <- function(data, rho = 0.8, modified = TRUE, k_stop = 5) {
  
  require(metafor)
  require(dplyr)
  
  if (modified == TRUE) {
    V_mat <- vcalc(Wgt, cluster = author_year, obs = esid, data = data, rho = rho)
  } else {
    V_mat <- vcalc(vgt, cluster = author_year, obs = esid, data = data, rho = rho)
  }
  
  # W <- solve(V_mat)
  
  # CHE
  if (sum(table(data$author_year) > 1L) >= 3) {
    res_CHE <- .estimate_meta(data = data,
                             rand = ~ 1 | author_year / esid,
                             V_mat = V_mat,
                             modified = modified,
                             method = "CHE")
  } else {
    res_CHE <- .estimate_meta(data = data,
                             rand = ~ 1 | author_year,
                             V_mat = V_mat,
                             modified = modified,
                             method = "CHE")
  }
  
  # CHE-ISCW
  res_CHE_ISCW <- .estimate_meta(data = data, 
                                rand = ~ 1 | author_year / esid,
                                V_mat = V_mat, 
                                W = TRUE,
                                method = "CHE-ISCW", modified = modified)
  if (is.null(res_CHE_ISCW)) {
    res_CHE_ISCW <- .estimate_meta(data = data, 
                                  rand = ~ 1 | author_year,
                                  V_mat = V_mat, 
                                  W = TRUE,
                                  method = "CHE-ISCW", modified = modified)
  }
  
  # multivariate PET-PEESE
  pet <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ Wse, 
                       rand = ~ 1 | author_year / esid, 
                       modified = modified, method = "mv_pet")
  if (is.null(pet)) { # make sure pet and peese converge
    pet <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ Wse, 
                         rand = ~ 1 | author_year, 
                         modified = modified, method = "mv_pet")
  }
  
  peese <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ Wgt, 
                         rand = ~ 1 | author_year / esid, 
                         modified = modified, method = "mv_peese")
  if (is.null(peese)) {
    peese <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ Wgt, 
                           rand = ~ 1 | author_year, 
                           modified = modified, method = "mv_peese")
  }
  
  pet_peese <- if (pet$p_val < 0.1 & pet$est > 0) peese else pet # .1 for one sided, .05 for two sided
  pet_peese$method <- "mv_pet_peese"
  res_pet_peese <- bind_rows(pet, peese, pet_peese)
  
  # multivariate EK
  mod_CHE_ISCW <- .estimate_meta(data = data, 
                                rand = ~ 1 | author_year / esid,
                                V_mat = V_mat, 
                                W = TRUE,
                                method = "CHE_ISCW", modified = modified,
                                returnMod = TRUE)
  if (!is.null(mod_CHE_ISCW)) {
    tausq_hat <- mod_CHE_ISCW$sigma2[1] # updated on 6/13
    omegasq_hat <- mod_CHE_ISCW$sigma2[2]
    tau_hat <- sqrt(tausq_hat)
    omega_hat <- sqrt(omegasq_hat)
  } else {
    mod_CHE_ISCW <- .estimate_meta(data = data, 
                                  rand = ~ 1 | author_year,
                                  V_mat = V_mat, 
                                  W = TRUE,
                                  method = "CHE_ISCW", modified = modified,
                                  returnMod = TRUE)
    tausq_hat <- mod_CHE_ISCW$sigma2
    omegasq_hat <- 0
    tau_hat <- sqrt(tausq_hat)
    omega_hat <- sqrt(omegasq_hat)
  }
  beta0_hat <- pet_peese$est
  a_est <- (beta0_hat^2 - 1.96^2 * (tausq_hat + omegasq_hat)) / ((1.96 + 1.96) * beta0_hat)
  kink <- if (beta0_hat <= 1.96 * sqrt(tau_hat^2 + omega_hat^2)) 0 else a_est
  data$kr_mod <- (data$Wse - kink) * (data$Wse > kink)
  
  EK <- .estimate_meta(data = data, V_mat = V_mat, 
                      W = TRUE, moderator = ~ I(kr_mod),
                      rand = ~ 1 | author_year / esid,
                      modified = modified, method = "mv_ek")
  
  # multivariate waap is multivariate version of WLS for ES with adequate power
  ## use CHE_ISCW as initial, determine power based on that
  waap_cutoff <- as.numeric(abs(res_CHE_ISCW$est / 2.8))
  data$waapInd <- data$Wse <= waap_cutoff
  waap_data <- subset(data, waapInd == TRUE)
  waap_j <- length(unique(waap_data$author_year))
  if (waap_j >= 2) { # if it does not converge, it does not converge.
    # should not specify V_mat bc it should be a subset of the original V_mat
    res_waap <- .estimate_meta(data = waap_data, 
                              W = TRUE,
                              rand = ~ 1 | author_year / esid,
                              method = "mv_waap",
                              modified = modified)
  } else {
    res_waap <- res_CHE_ISCW
    res_waap$method <- "mv_waap"
  }
  
  # multivariate WILS
  # res_wils <- estimate_mv_wils(data = data, modified = modified, k_stop = k_stop)
  
  return(rbind(res_CHE, res_CHE_ISCW, res_pet_peese, EK, res_waap))
  
}

.fit_reg_mv(data = reintergation_dat)

.fit_punistar <- function(data, otherwise = NULL) {
  
  # possibly is not working.
  # safe_punistar <- purrr::possibly(puni_star, otherwise = otherwise)
  # fit <- safe_punistar(ni = data$n, tobs = data$t_i, side = "right", plot = FALSE)
  
  fit <- tryCatch(puniform::puni_star(tobs = data$t_i, n1i = data$N_t, n2i = data$N_c, side = "right"), 
                  error = function(e) NULL)
  
  if (is.null(fit)) {
    NULL
  } else {
    data.frame(param = "beta.intrcpt",
               est = fit$est,
               se = NA,
               lo = fit$ci.lb,
               hi = fit$ci.ub,
               p_val = fit$pval.0, 
               R_conv = NA,
               method = "p-unistar") # pval for testing publication bias
  }
  
}