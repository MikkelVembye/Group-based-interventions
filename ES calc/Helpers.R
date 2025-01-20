# Function designed to perform a robust variance estimation meta-analysis. 
# For overall average effect modeling
.CHE_RVE <- function(data, rho = 0.7, study_out){
  
  if (missing(study_out)) {
    dat <- data
  } else{
    dat <- data |> 
      filter(Author != study_out)
  }
  
  V_mat <- vcalc(vi = vgt, cluster = studyid, obs = es_id, data = dat, rho = rho) 
  
  overall_res <- 
    rma.mv(
      gt, 
      V = V_mat,
      random = ~ 1 | studyid / es_id,
      data = dat
    ) |> 
    robust(cluster = studyid, clubSandwich = TRUE) 
  
  # I2 calcution
  # See https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate?s[]=i2
  W <- solve(V_mat)
  X <- model.matrix(overall_res)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  I2_tot <- round(100 * sum(overall_res$sigma2)/(sum(overall_res$sigma2) + 
                                                   (overall_res$k - overall_res$p)/sum(diag(P))), 2)
  
  pred <- predict(overall_res)
  
  res <- 
    tibble(
      rho = rho, 
      studies = overall_res$n,
      effects = overall_res$k,
      b = as.numeric(overall_res$b),
      se = overall_res$se,
      lb = overall_res$ci.lb,
      ub = overall_res$ci.ub,
      pi_lb = pred$pi.lb,  
      pi_ub = pred$pi.ub,  
      pval = overall_res$pval,
      df_satt = overall_res$dfs,
      tau = sqrt(overall_res$sigma2[1]), 
      omega = sqrt(overall_res$sigma2[2]), 
      sd_total = sqrt(sum(overall_res$sigma2)),
      QE = overall_res$QE,
      I2 = I2_tot
    )
  
  if (!missing(study_out)) res <- res |> mutate(omitted_study = study_out)
  
  res
  
}
