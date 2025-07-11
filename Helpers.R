# Function designed to perform a robust variance estimation meta-analysis. 
# For overall average effect modeling
.CHE_RVE <- function(data, studyid = study, rho = 0.8, study_out, pred_int = 80){
  
  if (missing(study_out)) {
    dat <- data
  } else{
    dat <- data |> 
      filter(Author != study_out)
  }
  
  studyid_class <- substitute(studyid)
  
  if (!is.character(studyid_class)) {
    dat$studyid <- data |> dplyr::pull({{ studyid }})
  } else {
    dat$studyid <- data[[studyid]]
  }
  
  V_mat <- vcalc(vi = vgt, cluster = studyid, obs = esid, data = dat, rho = rho) 
  
  optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
  overall_res <- "Non-converged"
  i <- 1L
  
  while (!inherits(overall_res, "rma.mv") & i <= 4L) {
    
    overall_res <- tryCatch( 
      suppressWarnings(
        rma.mv(
          gt, 
          V = V_mat,
          random = ~ 1 | studyid / esid,
          data = dat
        ) |> 
          robust(cluster = studyid, clubSandwich = TRUE) 
      ),
      error = function(e) "Non-converged"
    )
  } 
  # I2 calcution
  # See https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate?s[]=i2
  W <- solve(V_mat)
  X <- model.matrix(overall_res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  I2_tot <- round(100 * sum(overall_res$sigma2)/(sum(overall_res$sigma2) + 
                                                   (overall_res$k - overall_res$p)/sum(diag(P))), 2)
  
  pred <- predict(overall_res, level = pred_int)
  
  pi_lb_name <- paste0("pi_lb_", pred_int)
  pi_ub_name <- paste0("pi_ub_", pred_int)
  
  res <- 
    tibble(
      rho = rho, 
      studies = overall_res$n,
      effects = overall_res$k,
      avg_effect = as.numeric(overall_res$b),
      se = overall_res$se,
      LL = overall_res$ci.lb,
      UL = overall_res$ci.ub,
      !!pi_lb_name := pred$pi.lb,  
      !!pi_ub_name := pred$pi.ub,  
      pval = overall_res$pval,
      df_satt = overall_res$dfs,
      tau = sqrt(overall_res$sigma2[1]), 
      omega = sqrt(overall_res$sigma2[2]), 
      sd_total = sqrt(sum(overall_res$sigma2)),
      QE = overall_res$QE,
      I2 = I2_tot,
      tau2 = overall_res$sigma2[1],
      omega2 = overall_res$sigma2[2]
    )
  
  if (!missing(study_out)) res <- res |> mutate(omitted_study = study_out)
  
  res
  
}



.SCEp <- function(
    mod, 
    control_vars,
    data,
    yi = gt,
    vi = vgt,
    studyid = study,
    rho = 0.8
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
    dat$var <- data |> dplyr::pull({{ vi }})
  } else {
    dat$var <- data[[vi]]
  }
  
  if (!is.character(studyid_class)) {
    dat$studyid <- data |> dplyr::pull({{ studyid }})
  } else {
    dat$studyid <- data[[studyid]]
  }
  
  
  # Handling moderator
  mod_string <- mod_formula <- if (is.name(substitute(mod)))  deparse(substitute(mod)) else as.character(mod)
  
  dat <- if (mod_string == "timing") dat else dat |> dplyr:: filter(!stringr::str_detect(timing, "During"))
  
  if (!missing(control_vars)){
    
    control_string <- if (is.name(substitute(control_vars))) {
      deparse(substitute(control_vars))
    } else {
      as.character(control_vars)
    }
    
    if (str_detect(control_string, ";")) control_string <- str_split_1(control_string, pattern = ";")
    if (mod_string %in% control_string) control_string <- control_string[!control_string %in% mod_string]
    c_string <- if (length(control_string) > 1)  paste(control_string, collapse = ";") else control_string
    
    mod_formula <- c(mod_string, control_string)
    
  }
  
  reg <- reformulate(mod_formula, response = "yi", intercept = FALSE)
  
  # Setup for rma.mv formulas
  mod_name <- as.name(as.character(mod_string))
  
  outer_form <- substitute(~ moderator | studyid, list(moderator = mod_name)) |> as.formula()
  inner_form <- substitute(~ moderator | esid, list(moderator = mod_name)) |> as.formula()
  
  # Variance-Covariance Matrix
  V_mat <- vcalc(vi = var, cluster = studyid, obs = esid, data = dat, rho = rho, sparse = TRUE)
  
  optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
  raw_res <- "Non-converged"
  i <- 1L
  
  while (!inherits(raw_res, "rma.mv") & i <= 4L) {
    
    raw_res <- tryCatch(
      suppressWarnings(
        rma.mv(
          reg,
          V = V_mat,
          random = list(outer_form, inner_form), 
          struct = c("DIAG", "DIAG"),
          data = dat,
          sparse = TRUE,
          control = list(optimizer=optimizers[i])
        )
      ),
      error = function(e) "Non-converged"
    )
    i <- i + 1L
    
  }
  
  robu_res <- raw_res |> metafor::robust(cluster = studyid, clubSandwich = TRUE)
  
  moderators <- robu_res$g.levels.f[[1]]
  
  #seq_con <- 1:length(moderators)
  seq_con <- if (mod_string == "intervention") c(1, 3:7) else 1:length(moderators)
  seq_con <- if (mod_string == "sample") 2:3 else seq_con
  
  wald_htz_res <- 
    clubSandwich::Wald_test(
      raw_res,
      constraints = clubSandwich::constrain_equal(c(seq_con)),
      vcov = "CR2"
    )
  
  mod_string_table <- stringr::str_replace_all(mod_string, pattern = "_", replacement = " ")
  mod_string_table <- sub("^(\\w)(\\w*)", "\\U\\1\\L\\2", mod_string_table, perl = TRUE)
  
  controlled <- if (missing(control_vars)) "No" else "Yes"
  
  
  res <- 
    tibble(
      Characteric = mod_string,
      Moderator = c(mod_string_table, moderators),
      studies = c(robu_res$n, robu_res$g.levels.k),
      effects = c(robu_res$k, robu_res$h.levels.k),
      avg_effect = c(NA_real_, as.numeric(robu_res$b[1:length(moderators)])),
      LL = c(NA_real_, robu_res$ci.lb[1:length(moderators)]),
      UL = c(NA_real_, robu_res$ci.ub[1:length(moderators)]),
      df_satt = c(NA_real_, robu_res$dfs[1:length(moderators)]),
      SD_total = c(NA_real_, sqrt(robu_res$tau2 + robu_res$gamma2)),
      F_t = c(wald_htz_res$Fstat, robu_res$zval[1:length(moderators)]),
      pval = c(wald_htz_res$p_val, robu_res$pval[1:length(moderators)]),
      df_num = c(wald_htz_res$df_num, rep(NA_real_, length(moderators))),
      df_denom = c(wald_htz_res$df_denom, rep(NA_real_, length(moderators))),
      #boot_reps = R,
      rho = rho,
      wald_compared = c(paste(seq_con, collapse = ","), rep(NA_real_, length(moderators))),
      controls = controlled,
      optimizer = raw_res$control$optimizer,
      tau2 = c(NA_real_, robu_res$tau2),
      omega2 = c(NA_real_, robu_res$gamma2)
    )
  
  if (!missing(control_vars)) res <- res |> mutate(control_vars = c_string)
  
  attr(res, "robu_res") <- robu_res
  
  res
  
}

forest_plot_de <- 
  function(outcome_group, data, sce_data, che_data, rho = 0.8){
    
    if (!missing(che_data) && missing(outcome_group) && missing(sce_data)) {
      
      che_res <- che_data
      dat <- data |> mutate(analysis_plan = "Overall average effect")
      outcome_group <- "Overall average effect"
      tabel_label <- "Summary (CHE-RVE)"
      
    } else {
      
      che_res <- 
        sce_data |> 
        filter(Moderator == outcome_group)
     
      dat <- data |> filter(analysis_plan == outcome_group)
     
      tabel_label <- "Summary (SCE+)"  
    }
    
    tau2 <- che_res$tau2
    omega2 <- che_res$omega2
    beta <- round(che_res$avg_effect, 2)
    cil <- round(che_res$LL, 2)
    ciu <- round(che_res$UL, 2)
    
    studies <- dat |> pull(study) |> n_distinct()
    n_es <- nrow(dat)
    
    metafor_dat <- 
      escalc(yi = gt, vi = vgt, data = dat) |> 
      mutate(n = n(), .by = study)
    
    reframed_dat <-   
      escalc(yi = gt, vi = vgt, data = dat) |> 
      mutate(n = n(), .by = study) |> 
      aggregate(cluster = study, rho = rho) |> 
      reframe(
        yi = rep(yi, n),
        vi = rep(vi, n),
        .by = study
      ) |> 
      select(-study)
    
    
    dat <- bind_cols(dat, reframed_dat)
    
    forest_dat <- 
      dat |>  
      mutate(
        Est = gt,
        SE = sqrt(vgt),
        
        CI_L = Est - SE * qnorm(.975),
        CI_U = Est + SE * qnorm(.975),
        
        #rma_mean = as.numeric(rma(gt, vgt, data = pick(dplyr::everything()))$b)
        rma_mean = round(yi, 2),
        rma_cil = round(yi - sqrt(vi) * qnorm(.975), 2),
        rma_ciu = round(yi + sqrt(vi) * qnorm(.975), 2),
        
        kj = n(),
        
        sigma2j = mean(vgt),
        
        es_weight = ((kj*tau2 + omega2 + ((kj-1)*rho)*sigma2j) + sigma2j )^-1,
        
        .by = study
        
      ) |> 
      arrange(rma_mean, study) |> 
      mutate(
        study = factor(study, levels = rev(unique(study))),
        weight_prop = round((es_weight/sum(es_weight)) * 100, 2),
      )
    
      
    forest_dat2 <- 
      forest_dat |> 
      add_row(rma_mean = max(forest_dat$gt) + 0.01) |> 
      add_row(study = tabel_label) |> 
      mutate(
        study = replace_na(study, ""),
        study = factor(study, levels = rev(unique(study))),
        analysis_plan = if_else(is.na(analysis_plan), outcome_group, analysis_plan)
      ) 
    
    kj_label <- 
      forest_dat2 |> 
      summarise(
        Est = Est[1],
        CI_L = CI_L[1],
        CI_U = CI_U[1],
        
        mean_label = paste0(rma_mean[1], " [", rma_cil[1], ", ", rma_ciu[1], "], " ),
        
        label = paste0(mean_label, "(", kj[1], ") ", weight_prop[1], "%"),
        .by = c(analysis_plan, study)
      ) |> 
      mutate(
        label = case_when(
          study == "" ~ "",
          study == tabel_label ~ paste0(beta, " [", cil, ", ", ciu, "], ", studies, " (", n_es, ")"),
          .default = label
        )
      ) |> 
      arrange(study)
    
    mean_label_dat <- 
      forest_dat2 |> 
      mutate(
        mean_es = round(che_res$avg_effect, 2)
      )
    
    max_ciu <- forest_dat2$CI_U |> max(na.rm = TRUE)
    
    # Forest plot with all effect sizes
    r_diam_x <- r_diam_y_post <- forest_dat2 |> nrow() - 4
    sum.y <- c(1, 0.7, 1, 1.3, rep(NA, r_diam_y_post ))
    sum.x <- c(cil, beta, ciu, beta, rep(NA, r_diam_x))
    
    plot <- forest_dat2 |>
      ggplot(
        aes(x = Est, y = study, xmin = CI_L, xmax = CI_U,
            color = study, alpha = 0.5)
      ) + 
      geom_pointrange(position = position_dodge2(width = 0.5, padding = 0.5)) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      facet_grid(~analysis_plan) +
      geom_text(data = kj_label, aes(x = max_ciu + 0.6, label = label), size=3.3, color = "black") +
      geom_vline(data = mean_label_dat, aes(xintercept = mean_es), color = "black", linetype = 4) +
      geom_blank(aes(max_ciu + 0.6 + 0.4)) +
      geom_polygon(aes(x=sum.x, y=sum.y), color = "black", alpha = 1) +
      theme_light() + 
      theme(
        legend.position = "none",
        strip.text = element_text(color = "black"),
        axis.title.y=element_blank(),
        plot.caption = element_text(hjust = 0)
      ) + 
      scale_x_continuous(breaks = seq(-3, 3, 0.5)) +
      labs(
        x = "Hedges' g (95% CI)", 
        caption = paste0(
          "Average study effects and CIs in square brackets are listed in the right side of ",
          "the figure together with the number of effect sizes\n(in parentheses), and percent ",
          "weight that each effect size/point wihtin the study gets in the average estimation. ",
          "The overall average\nmean effect is given the lower right corner together with ",
          "the total number studies and effect sizes (in parenthesis) used for the analysis."
        )
      )
    
      
    suppressWarnings(plot)
     
  }

#outcome_group <- reintergation_dat$analysis_plan |> unique()
#map(1:4, ~ forest_plot_de(outcome_group = outcome_group[.x], data = reintergation_dat, sce_data = reintegration_sce_res))
#forest_plot_de(data = reintergation_dat, che_data = reintegration_che_res)

#reintegration_sce_res |> 
#  filter(str_detect(Moderator, base::regex(outcome_group[3], ignore_case = TRUE)))
