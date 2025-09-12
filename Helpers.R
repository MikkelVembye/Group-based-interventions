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
    yi = gt_pop,
    vi = vgt_pop,
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
  
  # Change studyid to study
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

# FINAL VERSIONS OF MODEL FUNCTIONS
.rma_arg_tbl <- 
  function(
    yi, vi, covars, r, model, data, type
  ){
    
    covariates <- if (str_detect(covars, ";")) stringr::str_split_1(covars, pattern = ";") else covars 
    
    
    if (stringr::str_detect(model, "SCE")) {
      
      formula <- reformulate(covariates, response = yi, intercept = FALSE)
      
      main_pred <- labels(terms(formula))[1]
      
      outer_form <- 
        substitute(
          ~ moderator | study, 
          list(moderator = as.name(main_pred))
        ) |> 
        as.formula()
      
      inner_form <- 
        substitute(
          ~ moderator | esid, 
          list(moderator = as.name(main_pred))
        ) |> 
        as.formula()
      
      random <-  if (model == "SCEp") list(outer_form, inner_form) else list(outer_form)
      
      struct <- rep("DIAG", length(random))
      
      res <- 
        tibble::tibble(
          formula = list(formula),
          es = yi, 
          var = vi,
          rand = list(random),
          structure = list(struct),
          rho = r,
          data = list(data),
          model = model,
          table = type
        )
      
    } else if (model == "CHE") {
      
      formula <- reformulate(covariates, response = yi, intercept = TRUE)
      
      res <- 
        tibble::tibble(
          formula = list(formula),
          es = yi, 
          var = vi,
          rand = list(~ 1 | study / esid),
          rho = r,
          data = list(data),
          model = model,
          table = type
        )
      
    }
    
    return(res)
    
  }


.Wald_test_cwb_fun <- 
  function(rma_fun_obj, seq_c, reps, seed_num){
    
    # Thanks to Rasmus Klokker for providing this solution
    #rma_fun_obj$call$yi <- as(rma_fun_obj$call$yi, Class = "call") 
    #rma_fun_obj$call$data <- as(rma_fun_obj$call$data, Class = "call") 
    
    V_mat <- rma_fun_obj$V
    assign("V_mat", V_mat, envir = .GlobalEnv)
    
    reint_ma_dat <- rma_fun_obj$data
    assign("reint_ma_dat", reint_ma_dat, envir = .GlobalEnv)
    
    auxiliary_dist <- c("Rademacher", "Mammen", "Webb six", "uniform", "standard normal")
    cwb_res <- list()
    i <- 1L
    
    while (!inherits(cwb_res, "Wald_test_wildmeta") & i <= length(auxiliary_dist)) {
      cwb_res <- try( 
        wildmeta::Wald_test_cwb(
          full_model = rma_fun_obj,
          constraints = wildmeta::constrain_equal(seq_c),
          R = reps,
          auxiliary_dist = auxiliary_dist[i],
          # "we see it as reasonable to use CWB without adjustment because it" 
          # "is conceptually and algorithmically simpler than CWB Adjusted." Joshi et al. 2022, p. 474
          adjust = "CR0",
          seed = seed_num,
          future_args = list(future.stdout = FALSE, future.conditions = character(0L))
        )
      )
      
      i <- i + 1L
      
    }
    
    if (inherits(cwb_res, 'try-error')) {
      
      cwb_res <- 
        data.frame(
          Test = "Non-converged", 
          Adjustment = as.character(attr(cwb_res, "condition")), 
          CR_type = NA_character_, 
          Statistic = NA_character_,
          R = NA_real_,
          p_val = NA_real_
        )
      
    }
    
    rm(V_mat, envir = .GlobalEnv)
    #rm(reint_ma_dat, envir = .GlobalEnv)
    
    return(cwb_res)
    
  }

#plan(multisession)
#.Wald_test_cwb_fun(
#  rma_fun_obj = x_test[[1]],
#  seq_con = 1:2
#)
#plan(sequential)

################################################################################
## Making SCE RVE function
################################################################################
.PESCE_RVE <- 
  function(formula, es, var, rand, structure, rho, data, model, table, R, seed, return_rma_obj = FALSE, CWB = FALSE){
    
    if (!stringr::str_detect(model, "SCE")) stop("This function only fits SCE models")
    
    data$vi <- data[[var]]
    data_name <- attr(data, "data_name")
    
    #V_mat <- metafor::vcalc(vi = vi, cluster = study, obs = esid, data = data, rho = rho)
    #  data = reint_ma_dat,
    #  vi = vgt_pop, 
    #  cluster = study,
    #  type = outcome_time, 
    #  grp1 = trt_name,
    #  w1 = N_t, 
    #  grp2 = control,
    #  w2 = N_c, 
    #  rho = rho
    
    # Variance-Covariance Matrix
    V_mat <- 
      metafor::vcalc(
        data = data,
        vi = vi, 
        cluster = study,
        type = outcome_time,
        grp1 = trt_name,
        w1 = N_t,
        grp2 = control,
        w2 = N_c, 
        rho = rho
      )
    
    # Strategy for overcoming non-convergence 
    optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
    raw_res <- "Non-converged"
    i <- 1L
    
    # Fitting the main model
    while (!inherits(raw_res, "rma.mv") & i <= 4L) {
      
      raw_res <- tryCatch(
        suppressWarnings(
          metafor::rma.mv(
            formula,
            V = V_mat,
            random = rand, 
            struct = structure,
            data = data,
            sparse = TRUE,
            control = list(optimizer=optimizers[i])
          )
        ),
        error = function(e) "Non-converged"
      )
      i <- i + 1L
      
    }
    
    struct_lang <- if(model == "SCEp") str2lang('c("DIAG", "DIAG")') else str2lang('"DIAG"')
    random_lang <- paste0("list(", paste0(rand, collapse = ", "), ")") |> str2lang()
    
    raw_res$call <- 
      rlang::call2(
        "rma.mv", 
        yi = formula, 
        V = as.name("V_mat"), 
        data = as.name(data_name), 
        random = random_lang, 
        struct = struct_lang, 
        sparse = TRUE,
        .ns = "metafor"
      )
    
    raw_res$call$yi <- methods::as(raw_res$call$yi, Class = "call") 
    
    # Returning main rma.mv object which can later be used with wald_test_cwb()
    if (return_rma_obj) return(raw_res)
    
    # Getting robust results
    robu_res <- raw_res |> metafor::robust(cluster = study, clubSandwich = TRUE)
    
    # Making character variable with all covariates 
    all_covariates <- all.vars(delete.response(terms(formula)))
    
    # Model control info
    if (length(all_covariates) > 1) {
      
      controlled <- "Yes"
      control_vars <- paste0(all_covariates[-1], collapse = ";")
      
    } else {
      
      controlled <- "No"
      control_vars <- "None"
      
    }
    
    # Getting name of main predictor variables
    mod_string <- all_covariates[1]
    # Getting name of each category of the main predictor variables
    moderators <- robu_res$g.levels.f[[1]]
    
    # Wald test comparison sequence
    seq_con <- 1:length(moderators)
    
    if(CWB){
      
      wald_cwb_res <-
        .Wald_test_cwb_fun(
          rma_fun_obj = raw_res,
          seq_c = seq_con,
          reps = R,
          seed_num = seed
        )
      
      wald_pval <- wald_cwb_res$p_val
      last_val_string <- NA_character_
      
      #res <- wald_cwb_res
      
      wald_type <- "Wald test (CWB)"
      
    } else {
      
      wald_htz_res <- 
        clubSandwich::Wald_test(
          raw_res,
          constraints = clubSandwich::constrain_equal(c(seq_con)),
          vcov = "CR2"
        )
      
      # Obtaning HTZ wald test p values
      wald_pval <- wald_htz_res$p_val 
      
      last_val_string <- paste0(
        "F(", round(wald_htz_res$df_num, 2), ", ", 
        round(wald_htz_res$df_denom, 2), ") = ", 
        round(wald_htz_res$Fstat, 2)
      )
      
      wald_type <- "Wald test (HTZ)"
      
    }
    
    
    # Readable moderator name removing _ and making upper-case letter for first word
    mod_string_table <- stringr::str_replace_all(mod_string, pattern = "_", replacement = " ")
    mod_string_table <- sub("^(\\w)(\\w*)", "\\U\\1\\L\\2", mod_string_table, perl = TRUE)
    
    # Moderator effects and CIs
    mod_effects <- round(as.numeric(robu_res$b[1:length(moderators)]), 2)
    mod_cil <- round(robu_res$ci.lb[1:length(moderators)], 2)
    mod_ciu <- round(robu_res$ci.ub[1:length(moderators)], 2)
    
    # Results in ready to publish format
    res <- 
      tibble(
        Characteric = mod_string,
        Moderator = c(mod_string_table, moderators, wald_type),
        studies = c(robu_res$n, robu_res$g.levels.k, NA_real_),
        effects = c(robu_res$k, robu_res$h.levels.k, NA_real_),
        avg_effect_ci = 
          c(
            NA_character_, 
            paste0(mod_effects, " [", mod_cil, ", ", mod_ciu, "]"), 
            last_val_string
          ),
        pval = round(c(NA_real_, robu_res$pval[1:length(moderators)], wald_pval), 3),
        df_satt = round(c(NA_real_, robu_res$dfs[1:length(moderators)], NA_real_), 1),
        SD_total = round(c(NA_real_, sqrt(robu_res$tau2 + robu_res$gamma2), NA_real_), 2),
        rho = rho,
        wald_compared = c(rep(NA_real_, length(moderators) + 1), paste(seq_con, collapse = ",")),
        controls = controlled,
        control_vars = control_vars,
        optimizer = raw_res$control$optimizer,
        avg_effect = round(c(NA_real_, as.numeric(robu_res$b[1:length(moderators)]), NA_real_), 2),
        LL = round(c(NA_real_, robu_res$ci.lb[1:length(moderators)], NA_real_), 2),
        UL = round(c(NA_real_, robu_res$ci.ub[1:length(moderators)], NA_real_), 2),
        tau2 = round(c(NA_real_, robu_res$tau2, NA_real_), 2),
        omega2 = round(c(NA_real_, robu_res$gamma2, NA_real_), 2),
        t_val = c(NA_real_, robu_res$zval[1:length(moderators)], NA_real_),
        table = table,
        effect_size = es
      )
    
    res
    
  }


