.estimate_meta <- function(
    data,
    g = gt,
    Va = Wgt, 
    var_g = vgt,
    studyid = author_year,
    rho = .8,
    rand = ~ 1 | studyid /esid,
    method,
    moderator = ~ 1,
    modified = TRUE,
    returnMod = FALSE
  ) {
  
  
  dat <- 
    data |> 
    dplyr::mutate(
      esid = 1:n()
    )
  
  g_class <- substitute(g) 
  Va_class <- substitute(Va) 
  var_g_class <- substitute(var_g)
  studyid_class <- substitute(studyid) 
  
  if (!is.character(g_class)) {
    dat$g <- data |> dplyr::pull({{ g }})
  } else {
    dat$g <- data[[g]]
  }
  
  
  if (!is.character(Va_class)) {
    dat$Va <- data |> dplyr::pull({{ Va }})
  } else {
    dat$Va <- data[[Va]]
  }
  
  if (!is.character(var_g_class)) {
    dat$var_g <- data |> dplyr::pull({{ var_g }})
  } else {
    dat$var_g <- data[[var_g]]
  }
  
  if (!is.character(studyid_class)) {
    dat$studyid <- data |> dplyr::pull({{ studyid }})
  } else {
    dat$studyid <- data[[studyid]]
  }
  
  
  if (modified == TRUE) {
    V_mat <- vcalc(Va, cluster = studyid, obs = esid, data = dat, rho = rho)
    W <- solve(V_mat)
  } else {
    V_mat <- vcalc(var_g, cluster = studyid, obs = esid, data = dat, rho = rho)
    W <- NULL
  }
  
  optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
  mod <- "Non-converged"
  i <- 1L
  
  while (!inherits(mod, "rma.mv") & i <= 4L) {
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
    i <- i + 1L
  }
  
  if (inherits(mod, "rma.mv")) { # rma.mv for CHE, PET, PEESE, WAAP, WILS
    mod <- robust(mod, cluster = studyid, clubSandwich = TRUE) 
    
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

#test_dat <- reintergation_dat |> filter(!str_detect(author_year, "Cano")) |> mutate(Wgt = (1/N_t + 1/N_c), Wse = sqrt(Va))
#.estimate_meta(data = reintergation_dat, method = "CHE-ISCW")


.fit_reg_mv <- 
  function(
    data, 
    g = gt,
    Va = Va, 
    var_g = vgt,
    studyid = author_year,
    rho = 0.8, 
    modified = TRUE, 
    k_stop = 5
  ) {
  
  data <- 
    data |> 
    dplyr::mutate(
      esid = 1:n()
    )
  
  g_class <- substitute(g) 
  Va_class <- substitute(Va) 
  var_g_class <- substitute(var_g)
  studyid_class <- substitute(studyid) 
  
  if (!is.character(g_class)) {
    data$g <- data |> dplyr::pull({{ g }})
  } else {
    data$g <- data[[g]]
  }
  
  
  if (!is.character(Va_class)) {
    data$Va <- data |> dplyr::pull({{ Va }})
    
  } else {
    data$Va <- data[[Va]]
  }
  
  data$sda <- sqrt(data$Va)
  
  if (!is.character(var_g_class)) {
    data$var_g <- data |> dplyr::pull({{ var_g }})
  } else {
    data$var_g <- data[[var_g]]
  }
  
  if (!is.character(studyid_class)) {
    data$studyid <- data |> dplyr::pull({{ studyid }})
  } else {
    data$studyid <- data[[studyid]]
  }
  
  
  require(metafor)
  require(dplyr)
  
  if (modified == TRUE) {
    V_mat <- vcalc(Va, cluster = studyid, obs = esid, data = data, rho = rho)
  } else {
    V_mat <- vcalc(var_g, cluster = studyid, obs = esid, data = data, rho = rho)
  }
  
  
  # CHE
  if (sum(table(data$studyid) > 1L) >= 3) {
    res_CHE <- .estimate_meta(data = data,
                             rand = ~ 1 | studyid / esid,
                             V_mat = V_mat,
                             modified = modified,
                             method = "CHE")
  } else {
    res_CHE <- .estimate_meta(data = data,
                             rand = ~ 1 | studyid,
                             V_mat = V_mat,
                             modified = modified,
                             method = "CHE")
  }
  
  # CHE-ISCW
  res_CHE_ISCW <- .estimate_meta(data = data, 
                                rand = ~ 1 | studyid / esid,
                                V_mat = V_mat, 
                                W = TRUE,
                                method = "CHE-ISCW", modified = modified)
  if (is.null(res_CHE_ISCW)) {
    res_CHE_ISCW <- .estimate_meta(data = data, 
                                  rand = ~ 1 | studyid,
                                  V_mat = V_mat, 
                                  W = TRUE,
                                  method = "CHE-ISCW", modified = modified)
  }
  
  # multivariate PET-PEESE
  pet <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ sda, 
                       rand = ~ 1 | studyid / esid, 
                       modified = modified, method = "mv_pet")
  if (is.null(pet)) { # make sure pet and peese converge
    pet <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ sda, 
                         rand = ~ 1 | studyid, 
                         modified = modified, method = "mv_pet")
  }
  
  peese <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ Va, 
                         rand = ~ 1 | studyid / esid, 
                         modified = modified, method = "mv_peese")
  if (is.null(peese)) {
    peese <- .estimate_meta(data = data, V_mat = V_mat, W = TRUE, moderator = ~ Va, 
                           rand = ~ 1 | studyid, 
                           modified = modified, method = "mv_peese")
  }
  
  pet_peese <- if (pet$p_val < 0.1 & pet$est > 0) peese else pet # .1 for one sided, .05 for two sided
  pet_peese$method <- "mv_pet_peese"
  res_pet_peese <- bind_rows(pet, peese, pet_peese)
  
  # multivariate EK
  mod_CHE_ISCW <- .estimate_meta(data = data, 
                                rand = ~ 1 | studyid / esid,
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
                                  rand = ~ 1 | studyid,
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
  data$kr_mod <- (data$sda - kink) * (data$sda > kink)
  
  EK <- .estimate_meta(data = data, V_mat = V_mat, 
                      W = TRUE, moderator = ~ I(kr_mod),
                      rand = ~ 1 | studyid / esid,
                      modified = modified, method = "mv_ek")
  
  # multivariate waap is multivariate version of WLS for ES with adequate power
  ## use CHE_ISCW as initial, determine power based on that
  waap_cutoff <- as.numeric(abs(res_CHE_ISCW$est / 2.8))
  data$waapInd <- data$sda <= waap_cutoff
  waap_data <- subset(data, waapInd == TRUE)
  waap_j <- length(unique(waap_data$studyid))
  if (waap_j >= 2) { # if it does not converge, it does not converge.
    # should not specify V_mat bc it should be a subset of the original V_mat
    res_waap <- .estimate_meta(data = waap_data, 
                              W = TRUE,
                              rand = ~ 1 | studyid / esid,
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

#dat <- reintergation_dat |> mutate(inv_wgt = 1/Wgt)
#
#.fit_reg_mv(
#  data = dat, 
#  g = gt,
#  Va = inv_wgt, 
#  modified = TRUE, 
#  rho = 0.8
#)

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

#.fit_punistar(data = reintergation_dat)

.fp_plot <- 
  function(
    data = reintergation_dat, 
    yi = gt,
    vi = Wgt,
    studyid = author_year,
    outcome_name = NULL,
    rho = 0.7,
    alpha_line = 0.5,
    polygon_fill = c("grey", "grey10", "lightcyan"),
    mean_line = "dashed",
    reg_test = TRUE,
    reg_line = "longdash",
    reg_color = "blue",
    breaks_y = seq(-3, 3, 0.5)
  ){
    
    dat <- 
      data |> 
      dplyr::mutate(
        esid = 1:n()
      )
    
    yi_class <- substitute(yi)
    vi_class <- substitute(vi)
    studyid_class <- substitute(studyid)
    
    
    if (!is.character(yi_class)) {
      dat$yi <- data |> dplyr::pull({{ yi }})
    } else {
      dat$yi <- data[[yi]]
    }
    
    
    if (!is.character(vi_class)) {
      dat$vi <- data |> dplyr::pull({{ vi }})
    } else {
      dat$vi <- data[[vi]]
    }
    
    dat$sei <- sqrt(dat$vi)
    
    if (!is.character(studyid_class)) {
      dat$studyid <- data |> dplyr::pull({{ studyid }})
    } else {
      dat$studyid <- data[[studyid]]
    }
    
    ### mixed-effects meta-regression version of the Egger test
    
    if (reg_test){
      RE_egg <- rma(yi = yi, vi = vi, data = dat)
      egg_res <- regtest(RE_egg)
      
      geom_reg_line <- geom_abline(slope = egg_res$fit$beta[2], intercept = egg_res$fit$beta[1], linetype = reg_line, color = reg_color) 
      
    } else {
      geom_reg_line <- NULL
    }
    
    # CHE
    V_mat <- vcalc(vi = vi, cluster = studyid, obs = esid, data = dat, rho = rho) 
    
    optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
    overall_res <- "Non-converged"
    i <- 1L
    
    while (!inherits(overall_res, "rma.mv") & i <= 4L) {
      
      overall_res <- tryCatch( 
        suppressWarnings(
          rma.mv(
            yi, 
            V = V_mat,
            random = ~ 1 | studyid / esid,
            data = dat
          ) |> 
            robust(cluster = studyid, clubSandwich = TRUE) 
        ),
        error = function(e) "Non-converged"
      )
    } 
    
    overall_mean <- as.numeric(overall_res$b)
      
    y_lim_exp1 <- max(dat$sei) + 0.02 
    
    funnel_exp1 <-  tribble(
      ~ x90, ~ x95, ~ x99, ~ y,
      0,     0,     0,     0,
      qnorm(0.05) * y_lim_exp1, qnorm(0.025) * y_lim_exp1, qnorm(0.005) * y_lim_exp1, y_lim_exp1,
      qnorm(0.95) * y_lim_exp1, qnorm(0.975) * y_lim_exp1, qnorm(0.995) * y_lim_exp1, y_lim_exp1,
      0,     0,     0,     0
    ) 

    y_lim_exp1
    
    if (is.null(outcome_name)){
      
      facet_grid_w_name <- NULL
      
    } else {
      
     outcome_name_class <- substitute(outcome_name)
      
     outcome_name <- if (!is.character(outcome_name_class)) deparse(outcome_name_class) else outcome_name
     
     dat <- 
       dat |> 
       mutate(
         outcome_facet_name = outcome_name
       )
      
      facet_grid_w_name <- facet_grid(~outcome_facet_name, scales = "free")
      
      
    }
    
    dat |> 
      ggplot() + 
      geom_polygon(data = funnel_exp1, aes(x = y, y = x99), fill = polygon_fill[1], alpha = 0.5) + 
      geom_polygon(data = funnel_exp1, aes(x = y, y = x95), fill = polygon_fill[2], alpha = 0.5) + 
      geom_polygon(data = funnel_exp1, aes(x = y, y = x90), fill = polygon_fill[3], alpha = 0.7) + 
      geom_abline(slope = qnorm(0.975), intercept = overall_mean, linetype = mean_line, alpha = alpha_line) + 
      geom_hline(yintercept = overall_mean, linetype = mean_line, alpha = alpha_line) +  
      geom_abline(slope = qnorm(0.025), intercept = overall_mean, linetype = mean_line, alpha = alpha_line) + 
      geom_reg_line + 
      geom_point(aes(sei, yi), alpha = 1, size = 1.2) +
      scale_color_brewer(type = "qual", palette = 2) + 
      coord_flip() +
      facet_grid_w_name +
      scale_x_reverse(limits = c(y_lim_exp1, 0.0), expand = c(0,0)) + 
      scale_y_continuous(breaks = breaks_y) + 
      theme_bw() + 
      labs(x = "Standard error (adjusted)", 
           y = "Standardized mean difference (Hedges' g)", 
           color = "", shape = "") +
      theme(legend.position = "bottom")
}

#.fp_plot(outcome_name = "Reintegrational")

# misc
#W_mat <- function(study, vi = vgt, data = reintergation_dat){
#  
#  data$vi <- data |> dplyr::pull({{vi}})
# 
#  V_mat <- metafor::vcalc(vi = vi, cluster = author_year, obs = es_id, data = data, rho = 0.7)
#  
#  W <- solve(V_mat)
#  
#  dat <- data |> 
#    mutate(id = 1:n()) |> 
#    filter(author_year == !!study) 
#  
#  start <- min(dat$id)
#  end <- max(dat$id)
#  
#  W[start:end, start:end]
#  
#}
#
#map(.x = studies, .f = W_mat)
#map(.x = studies, .f = W_mat, vi = Wgt)
