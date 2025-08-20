---
title: "Main analysis (group-based)"
author: "Mikkel H. Vembye"
subtitle: ""
date: "2025-08-20"
format:
  html: 
    keep-md: true
    self-contained: true
    grid: 
      margin-width: 350px
    code-fold: true
    code-summary: "Show the code"
    toc: true
    toc-location: left
pdf-engine: pdflatex
execute: 
  echo: fenced
  warning: false
  message: false
knitr:
  opts_chunk: 
    fig.pos: "H"
    fig.retina: 2
    cache: FALSE
    R.options:
      knitr.graphics.auto_pdf: true
      width: 100
      knitr.kable.NA: ""
      dplyr.summarise.inform: FALSE
reference-location: margin
citation-location: margin
bibliography: bibliography.bib 
---




## Loading R packages and data

Below, we load the R package and data used for you main analyses. You can find the generated datasets and the corresponding variables in the PRIMED workflow document. 



::: {.cell}

````{.cell-code}
```{{r packages-and-source-file}}
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)
library(tidyr)
library(rempsyc)
library(flextable)
library(purrr)
library(metafor)
library(patchwork)
library(clubSandwich)
library(knitr)
library(kableExtra)
library(forcats)
library(future)
library(furrr)
library(gt)

source("Helpers.R")

reintegration_dat <- readRDS("reintegation_dat.rds")
mental_health_dat <- readRDS("mental_health_dat.rds")
```
````
:::




## Moderators and control variables manipulation

In this section, we manipulate all variables used in meta-regression analyses. 



::: {.cell}

````{.cell-code}
```{{r, manipulate-data}}
# List all relevant moderator variables here: 
## Type of outcome: analysis_plan
## 

# Read: reint_ma_dat = reintegrational meta-analysis data
reint_ma_dat <- 
  reintegration_dat |> 
  select(
    # Various types of effect size estimates 
    study, gt_pop, vgt_pop, Wgt_pop, gt, vgt, Wgt, g, vg, d, vd,
    
    # Categorical moderators and control variables 
    analysis_plan, schizophrenia, CBT_intervention = CBT_int, test_type, analysis_strategy,
    QES_design, control, D1:D7, overall_rob, prereg_chr,
    
    # Continuous moderators and control variables
    age_mean, male_pct, total_number_of_sessions, sessions_per_week, duration_in_weeks,
    time_after_end_intervention_weeks, time_from_baseline_weeks
    
  ) |> 
  mutate(
    esid = 1:n(),
    
    # Outcome variables
    outcome_type = case_match(
      analysis_plan, 
      c("Employment", "Physical health", "Psychiatric hospitalization") ~ "Other",
      .default = analysis_plan
    ),
    
    outcome_type = fct_relevel(outcome_type, sort),
    outcome_type = fct_relevel(outcome_type, "Other", after = Inf),
    
    
    
    sessions_per_week = if_else(is.na(sessions_per_week), mean(sessions_per_week, na.rm = TRUE), sessions_per_week),
    male_pct = if_else(is.na(male_pct), mean(male_pct, na.rm = TRUE), male_pct),
    
    age_c = age_mean - 40,
    male_c = male_pct/100 - 0.5,
    sessions_c = sessions_per_week - 1,
    duration_c = duration_in_weeks - 12,
    fu_time_c = time_after_end_intervention_weeks - 1
    
    
  ) 
```
````
:::




## Outcome


::: {.cell}

````{.cell-code}
```{{r outcome-test}}
rho <- 0.8

V_mat_outcome <- 
  vcalc(
    vi = vgt_pop, 
    cluster = study, 
    obs = esid, 
    data = reint_ma_dat, 
    rho = rho
  )

outcome_obj <- 
  rma.mv(
    yi = gt_pop ~ outcome_type - 1,
    V = V_mat_outcome, 
    random = list(~ outcome_type | study, ~ outcome_type | esid),
    struct = c("DIAG", "DIAG"),
    data = reint_ma_dat,
    sparse=TRUE
  )


outcome_obj_robu <- 
  outcome_obj |> 
  metafor::robust(cluster = study, clubSandwich = TRUE)

outcome_obj_robu

club_wald_test <- Wald_test(outcome_obj, constraints = constrain_equal(1:3), vcov = "CR2")
club_wald_test


# Continuous model

age_obj <- 
  rma.mv(
    yi = gt_pop ~ age_c + prereg_chr - 1,
    V = V_mat_outcome, 
    random = ~ 1 | study / esid,
    data = reint_ma_dat,
    sparse = TRUE
  ) |> 
  metafor::robust(cluster = study, clubSandwich = TRUE)

age_obj

prereg_obj <- 
    rma.mv(
    yi = gt_pop ~ prereg_chr + male_c - 1,
    V = V_mat_outcome, 
    random = list(~ prereg_chr | study, ~ prereg_chr | esid),
    struct = c("DIAG", "DIAG"),
    data = reint_ma_dat,
    sparse=TRUE
  ) |> 
  metafor::robust(cluster = study, clubSandwich = TRUE)

prereg_obj
```
````

::: {.cell-output .cell-output-stdout}

```

Multivariate Meta-Analysis Model (k = 202; method: REML)

Variance Components:

outer factor: study        (nlvls = 45)
inner factor: outcome_type (nlvls = 7)

            estim    sqrt  k.lvl  fixed                                      level 
tau^2.1    0.0000  0.0000      7     no              Alcohol and drug abuse/misuse 
tau^2.2    0.0139  0.1178     12     no          Hope, empowerment & self-efficacy 
tau^2.3    0.0662  0.2573      4     no                                 Loneliness 
tau^2.4    0.0000  0.0000      5     no                                Self-esteem 
tau^2.5    0.0249  0.1577     16     no  Social functioning (degree of impairment) 
tau^2.6    0.0044  0.0662     24     no              Wellbeing and quality of life 
tau^2.7    0.0000  0.0000      4     no                                      Other 

outer factor: esid         (nlvls = 202)
inner factor: outcome_type (nlvls = 7)

              estim    sqrt  k.lvl  fixed                                      level 
gamma^2.1    0.0465  0.2156     31     no              Alcohol and drug abuse/misuse 
gamma^2.2    0.0280  0.1675     32     no          Hope, empowerment & self-efficacy 
gamma^2.3    0.0103  0.1017      5     no                                 Loneliness 
gamma^2.4    0.0243  0.1559     14     no                                Self-esteem 
gamma^2.5    0.0242  0.1557     47     no  Social functioning (degree of impairment) 
gamma^2.6    0.0282  0.1678     67     no              Wellbeing and quality of life 
gamma^2.7    0.0000  0.0000      6     no                                      Other 

Test for Residual Heterogeneity:
QE(df = 195) = 1133.4973, p-val < .0001

Number of estimates:   202
Number of clusters:    45
Estimates per cluster: 1-28 (mean: 4.49, median: 3)

Test of Moderators (coefficients 1:7):¹
F(df1 = 7, df2 = 7.64) = 6.9951, p-val = 0.0078

Model Results:

                                                       estimate      se¹    tval¹     df¹    pval¹ 
outcome_typeAlcohol and drug abuse/misuse                0.0775  0.0678   1.1430    4.55   0.3096  
outcome_typeHope, empowerment & self-efficacy            0.2615  0.0639   4.0940   10.83   0.0018  
outcome_typeLoneliness                                   0.0310  0.1619   0.1916    2.66   0.8618  
outcome_typeSelf-esteem                                  0.4281  0.0709   6.0363    3.96   0.0039  
outcome_typeSocial functioning (degree of impairment)    0.1462  0.0705   2.0745   12.81   0.0588  
outcome_typeWellbeing and quality of life                0.2120  0.0452   4.6921   13.88   0.0004  
outcome_typeOther                                        0.2733  0.0453   6.0362     3.4   0.0063  
                                                         ci.lb¹   ci.ub¹      
outcome_typeAlcohol and drug abuse/misuse              -0.1021   0.2571       
outcome_typeHope, empowerment & self-efficacy           0.1207   0.4024    ** 
outcome_typeLoneliness                                 -0.5237   0.5858       
outcome_typeSelf-esteem                                 0.2304   0.6257    ** 
outcome_typeSocial functioning (degree of impairment)  -0.0063   0.2986     . 
outcome_typeWellbeing and quality of life               0.1150   0.3089   *** 
outcome_typeOther                                       0.1383   0.4083    ** 

---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1) results based on cluster-robust inference (var-cov estimator: CR2,
   approx t/F-tests and confidence intervals, df: Satterthwaite approx)

 test Fstat df_num df_denom p_val sig
  HTZ  2.14      2     5.44 0.206    

Multivariate Meta-Analysis Model (k = 202; method: REML)

Variance Components:

            estim    sqrt  nlvls  fixed      factor 
sigma^2.1  0.0032  0.0561     45     no       study 
sigma^2.2  0.0368  0.1917    202     no  study/esid 

Test for Residual Heterogeneity:
QE(df = 199) = 1217.2224, p-val < .0001

Number of estimates:   202
Number of clusters:    45
Estimates per cluster: 1-28 (mean: 4.49, median: 3)

Test of Moderators (coefficients 1:3):¹
F(df1 = 3, df2 = 17.31) = 13.0335, p-val = 0.0001

Model Results:

                             estimate      se¹    tval¹     df¹    pval¹    ci.lb¹   ci.ub¹      
age_c                          0.0081  0.0044   1.8396   13.58   0.0878   -0.0014   0.0176     . 
prereg_chrNot preregistered    0.3163  0.0595   5.3150   17.72   <.0001    0.1911   0.4415   *** 
prereg_chrPreregistered        0.1197  0.0467   2.5614    14.2   0.0224    0.0196   0.2198     * 

---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1) results based on cluster-robust inference (var-cov estimator: CR2,
   approx t/F-tests and confidence intervals, df: Satterthwaite approx)


Multivariate Meta-Analysis Model (k = 202; method: REML)

Variance Components:

outer factor: study      (nlvls = 45)
inner factor: prereg_chr (nlvls = 2)

            estim    sqrt  k.lvl  fixed              level 
tau^2.1    0.0000  0.0000     22     no  Not preregistered 
tau^2.2    0.0069  0.0830     23     no      Preregistered 

outer factor: esid       (nlvls = 202)
inner factor: prereg_chr (nlvls = 2)

              estim    sqrt  k.lvl  fixed              level 
gamma^2.1    0.0717  0.2678     61     no  Not preregistered 
gamma^2.2    0.0295  0.1717    141     no      Preregistered 

Test for Residual Heterogeneity:
QE(df = 199) = 1232.4915, p-val < .0001

Number of estimates:   202
Number of clusters:    45
Estimates per cluster: 1-28 (mean: 4.49, median: 3)

Test of Moderators (coefficients 1:3):¹
F(df1 = 3, df2 = 14.75) = 13.8960, p-val = 0.0001

Model Results:

                             estimate      se¹    tval¹     df¹    pval¹    ci.lb¹   ci.ub¹      
prereg_chrNot preregistered    0.2905  0.0510   5.6988    16.3   <.0001    0.1826   0.3984   *** 
prereg_chrPreregistered        0.1413  0.0430   3.2874   15.43   0.0048    0.0499   0.2327    ** 
male_c                         0.0457  0.1475   0.3096   12.47   0.7620   -0.2744   0.3657       

---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

1) results based on cluster-robust inference (var-cov estimator: CR2,
   approx t/F-tests and confidence intervals, df: Satterthwaite approx)
```


:::
:::



## Function building for analysis



::: {.cell}

````{.cell-code}
```{{r}}
# Read: rma_arg_tbl = rma argument tibble

.rma_arg_tbl <- 
  function(
    yi, vi, covars, r, model, data
  ){
    
    covariates <- if (str_detect(covars, ";")) str_split_1(covars, pattern = ";") else covars 
    
    formula <- reformulate(covariates, response = yi, intercept = FALSE)
    
    if (str_detect(model, "SCE")) {
      
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
        tibble(
          formula = list(formula),
          var = vi,
          rand = list(random),
          structure = list(struct),
          rho = r,
          data = list(data),
          model = model
        )
      
    } else if (model == "CHE") {
      
      res <- 
        tibble(
          formula = list(formula),
          var = vi,
          rand = list(~ 1 | study / esid),
          rho = r,
          data = list(data),
          model = model
        )
      
    }
    
    return(res)
    
}

tbl_test <- 
  .rma_arg_tbl(
    yi = "gt_pop", 
    vi = "vgt_pop", 
    covars = "outcome_type",
    model = "SCEp",
    r = 0.8, 
    data = reint_ma_dat
); tbl_test


arg_tbl <- 
  tibble(
    yi = rep(c("gt_pop", "gt"), each = 20),
    vi = rep(c("vgt_pop", "vgt"), each = 20),
    covars = rep(
      c(
        "outcome_type", "outcome_type;prereg_chr;age_c;male_c;sessions_c;duration_c;fu_time_c",
        "prereg_chr", "prereg_chr;outcome_type"
        ), each = 5, 2),
    model = "SCEp",
    r = rep(seq(0, 0.8, 0.2), 8)
  )

arg_list_tbl <- 
  pmap(.l = arg_tbl, .f = .rma_arg_tbl, data = reint_ma_dat) |> 
  list_rbind()


# Making SCE RVE function

.SCE_RVE <- 
  function(formula, var, rand, structure, rho, data, model, return_rma_obj = FALSE){
    
  if (!str_detect(model, "SCE")) stop("This function only fits SCE models")
    
  data$vi <- data[[var]]

  # Variance-Covariance Matrix
  V_mat <- vcalc(vi = vi, cluster = study, obs = esid, data = data, rho = rho, sparse = TRUE)
  
  # Strategy for overcoming non-convergence 
  optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
  raw_res <- "Non-converged"
  i <- 1L
    
  # Fitting the main model
  while (!inherits(raw_res, "rma.mv") & i <= 4L) {
    
    raw_res <- tryCatch(
      suppressWarnings(
        rma.mv(
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
  
  # Estimating HTZ wald test 
  wald_htz_res <- 
    clubSandwich::Wald_test(
      raw_res,
      constraints = clubSandwich::constrain_equal(c(seq_con)),
      vcov = "CR2"
    )
  
  # Obtaning HTZ wald test p values
  wald_pval <- wald_htz_res$p_val
  
  # F stat info
  F_value_str <- paste0(
    "F(", round(wald_htz_res$df_num, 2), ", ", 
    round(wald_htz_res$df_denom, 2), ") = ", 
    round(wald_htz_res$Fstat, 2)
  )
  
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
      Moderator = c(mod_string_table, moderators, "Wald test (HTZ)"),
      studies = c(robu_res$n, robu_res$g.levels.k, NA_real_),
      effects = c(robu_res$k, robu_res$h.levels.k, NA_real_),
      avg_effect_F = 
        c(
          NA_real_, 
          paste0(mod_effects, " [", mod_cil, ", ", mod_ciu, "]"), 
          F_value_str
      ),
      pval = round(c(NA_real_, robu_res$pval[1:length(moderators)], wald_pval), 2),
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
      t_F = c(NA_real_, robu_res$zval[1:length(moderators)], wald_htz_res$Fstat),
    )
  
  res
  
}

opts <- furrr::furrr_options(
  stdout = FALSE,        # don't forward cat/print output
  conditions = NULL      # don't forward messages/warnings
)

arg_list_tbl_rho08 <- arg_list_tbl |> filter(rho == 0.8 & var == "vgt_pop")

plan(multisession)
x_test <- future_pmap(.l = arg_list_tbl_rho08, .f = .SCE_RVE, .options = opts) |> list_rbind(); x_test
plan(sequential)
```
````

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 7
  formula   var     rand       structure   rho data                model
  <list>    <chr>   <list>     <list>    <dbl> <list>              <chr>
1 <formula> vgt_pop <list [2]> <chr [2]>   0.8 <tibble [202 × 41]> SCEp 
# A tibble: 26 × 19
   Characteric  Moderator    studies effects avg_effect_F  pval df_satt SD_total   rho wald_compared
   <chr>        <chr>          <dbl>   <dbl> <chr>        <dbl>   <dbl>    <dbl> <dbl> <chr>        
 1 outcome_type Outcome type      45     202 <NA>         NA       NA      NA      0.8 <NA>         
 2 outcome_type Alcohol and…       7      31 0.08 [-0.1,…  0.31     4.6     0.22   0.8 <NA>         
 3 outcome_type Hope, empow…      12      32 0.26 [0.12,…  0       10.8     0.2    0.8 <NA>         
 4 outcome_type Loneliness         4       5 0.03 [-0.52…  0.86     2.7     0.28   0.8 <NA>         
 5 outcome_type Self-esteem        5      14 0.43 [0.23,…  0        4       0.16   0.8 <NA>         
 6 outcome_type Social func…      16      47 0.15 [-0.01…  0.06    12.8     0.22   0.8 <NA>         
 7 outcome_type Wellbeing a…      24      67 0.21 [0.11,…  0       13.9     0.18   0.8 <NA>         
 8 outcome_type Other              4       6 0.27 [0.14,…  0.01     3.4     0      0.8 <NA>         
 9 outcome_type Wald test (…      NA      NA F(6, 7) = 1…  0.26    NA      NA      0.8 1,2,3,4,5,6,7
10 outcome_type Outcome type      45     202 <NA>         NA       NA      NA      0.8 <NA>         
# ℹ 16 more rows
# ℹ 9 more variables: controls <chr>, control_vars <chr>, optimizer <chr>, avg_effect <dbl>,
#   LL <dbl>, UL <dbl>, tau2 <dbl>, omega2 <dbl>, t_F <dbl>
```


:::
:::

::: {.cell}

````{.cell-code}
```{{r table-test}}
wider_dat <- 
  x_test |> 
  select(Characteric:SD_total, controls) |> 
  pivot_wider(names_from = controls, values_from = c(avg_effect_F:SD_total)) |> 
  relocate(contains("no",  ignore.case = TRUE), .after = effects) |>          
  relocate(contains("yes", ignore.case = TRUE), .after = last_col())



wider_dat |> 
  select(-1) |> 
  gt() |> 
    sub_missing(
    columns = everything(),   
    missing_text = ""         
  )
```
````

::: {.cell-output-display}


```{=html}
<div id="vxgkajsbxx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vxgkajsbxx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vxgkajsbxx thead, #vxgkajsbxx tbody, #vxgkajsbxx tfoot, #vxgkajsbxx tr, #vxgkajsbxx td, #vxgkajsbxx th {
  border-style: none;
}

#vxgkajsbxx p {
  margin: 0;
  padding: 0;
}

#vxgkajsbxx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vxgkajsbxx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vxgkajsbxx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vxgkajsbxx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vxgkajsbxx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxgkajsbxx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxgkajsbxx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vxgkajsbxx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vxgkajsbxx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vxgkajsbxx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vxgkajsbxx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vxgkajsbxx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vxgkajsbxx .gt_spanner_row {
  border-bottom-style: hidden;
}

#vxgkajsbxx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vxgkajsbxx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vxgkajsbxx .gt_from_md > :first-child {
  margin-top: 0;
}

#vxgkajsbxx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vxgkajsbxx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vxgkajsbxx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vxgkajsbxx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vxgkajsbxx .gt_row_group_first td {
  border-top-width: 2px;
}

#vxgkajsbxx .gt_row_group_first th {
  border-top-width: 2px;
}

#vxgkajsbxx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxgkajsbxx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vxgkajsbxx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vxgkajsbxx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxgkajsbxx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxgkajsbxx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vxgkajsbxx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vxgkajsbxx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vxgkajsbxx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxgkajsbxx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxgkajsbxx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxgkajsbxx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vxgkajsbxx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxgkajsbxx .gt_left {
  text-align: left;
}

#vxgkajsbxx .gt_center {
  text-align: center;
}

#vxgkajsbxx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vxgkajsbxx .gt_font_normal {
  font-weight: normal;
}

#vxgkajsbxx .gt_font_bold {
  font-weight: bold;
}

#vxgkajsbxx .gt_font_italic {
  font-style: italic;
}

#vxgkajsbxx .gt_super {
  font-size: 65%;
}

#vxgkajsbxx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vxgkajsbxx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vxgkajsbxx .gt_indent_1 {
  text-indent: 5px;
}

#vxgkajsbxx .gt_indent_2 {
  text-indent: 10px;
}

#vxgkajsbxx .gt_indent_3 {
  text-indent: 15px;
}

#vxgkajsbxx .gt_indent_4 {
  text-indent: 20px;
}

#vxgkajsbxx .gt_indent_5 {
  text-indent: 25px;
}

#vxgkajsbxx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#vxgkajsbxx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Moderator">Moderator</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="studies">studies</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="effects">effects</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="avg_effect_F_No">avg_effect_F_No</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pval_No">pval_No</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="df_satt_No">df_satt_No</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD_total_No">SD_total_No</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="avg_effect_F_Yes">avg_effect_F_Yes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pval_Yes">pval_Yes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="df_satt_Yes">df_satt_Yes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD_total_Yes">SD_total_Yes</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Moderator" class="gt_row gt_left">Outcome type</td>
<td headers="studies" class="gt_row gt_right">45</td>
<td headers="effects" class="gt_row gt_right">202</td>
<td headers="avg_effect_F_No" class="gt_row gt_left"><br /></td>
<td headers="pval_No" class="gt_row gt_right"><br /></td>
<td headers="df_satt_No" class="gt_row gt_right"><br /></td>
<td headers="SD_total_No" class="gt_row gt_right"><br /></td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left"><br /></td>
<td headers="pval_Yes" class="gt_row gt_right"><br /></td>
<td headers="df_satt_Yes" class="gt_row gt_right"><br /></td>
<td headers="SD_total_Yes" class="gt_row gt_right"><br /></td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Alcohol and drug abuse/misuse</td>
<td headers="studies" class="gt_row gt_right">7</td>
<td headers="effects" class="gt_row gt_right">31</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.08 [-0.1, 0.26]</td>
<td headers="pval_No" class="gt_row gt_right">0.31</td>
<td headers="df_satt_No" class="gt_row gt_right">4.6</td>
<td headers="SD_total_No" class="gt_row gt_right">0.22</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.2 [-0.04, 0.45]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.09</td>
<td headers="df_satt_Yes" class="gt_row gt_right">12.9</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.22</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Hope, empowerment &amp; self-efficacy</td>
<td headers="studies" class="gt_row gt_right">12</td>
<td headers="effects" class="gt_row gt_right">32</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.26 [0.12, 0.4]</td>
<td headers="pval_No" class="gt_row gt_right">0.00</td>
<td headers="df_satt_No" class="gt_row gt_right">10.8</td>
<td headers="SD_total_No" class="gt_row gt_right">0.20</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.35 [0.16, 0.54]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.00</td>
<td headers="df_satt_Yes" class="gt_row gt_right">17.2</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.21</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Loneliness</td>
<td headers="studies" class="gt_row gt_right">4</td>
<td headers="effects" class="gt_row gt_right">5</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.03 [-0.52, 0.59]</td>
<td headers="pval_No" class="gt_row gt_right">0.86</td>
<td headers="df_satt_No" class="gt_row gt_right">2.7</td>
<td headers="SD_total_No" class="gt_row gt_right">0.28</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.07 [-0.16, 0.3]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.48</td>
<td headers="df_satt_Yes" class="gt_row gt_right">7.0</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.09</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Self-esteem</td>
<td headers="studies" class="gt_row gt_right">5</td>
<td headers="effects" class="gt_row gt_right">14</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.43 [0.23, 0.63]</td>
<td headers="pval_No" class="gt_row gt_right">0.00</td>
<td headers="df_satt_No" class="gt_row gt_right">4.0</td>
<td headers="SD_total_No" class="gt_row gt_right">0.16</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.52 [0.35, 0.7]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.00</td>
<td headers="df_satt_Yes" class="gt_row gt_right">6.5</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.15</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Social functioning (degree of impairment)</td>
<td headers="studies" class="gt_row gt_right">16</td>
<td headers="effects" class="gt_row gt_right">47</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.15 [-0.01, 0.3]</td>
<td headers="pval_No" class="gt_row gt_right">0.06</td>
<td headers="df_satt_No" class="gt_row gt_right">12.8</td>
<td headers="SD_total_No" class="gt_row gt_right">0.22</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.25 [0.06, 0.45]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.01</td>
<td headers="df_satt_Yes" class="gt_row gt_right">18.6</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.22</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Wellbeing and quality of life</td>
<td headers="studies" class="gt_row gt_right">24</td>
<td headers="effects" class="gt_row gt_right">67</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.21 [0.11, 0.31]</td>
<td headers="pval_No" class="gt_row gt_right">0.00</td>
<td headers="df_satt_No" class="gt_row gt_right">13.9</td>
<td headers="SD_total_No" class="gt_row gt_right">0.18</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.29 [0.11, 0.47]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.00</td>
<td headers="df_satt_Yes" class="gt_row gt_right">18.2</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.20</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Other</td>
<td headers="studies" class="gt_row gt_right">4</td>
<td headers="effects" class="gt_row gt_right">6</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.27 [0.14, 0.41]</td>
<td headers="pval_No" class="gt_row gt_right">0.01</td>
<td headers="df_satt_No" class="gt_row gt_right">3.4</td>
<td headers="SD_total_No" class="gt_row gt_right">0.00</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.36 [0.18, 0.54]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.00</td>
<td headers="df_satt_Yes" class="gt_row gt_right">4.9</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.00</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Wald test (HTZ)</td>
<td headers="studies" class="gt_row gt_right"><br /></td>
<td headers="effects" class="gt_row gt_right"><br /></td>
<td headers="avg_effect_F_No" class="gt_row gt_left">F(6, 7) = 1.67</td>
<td headers="pval_No" class="gt_row gt_right">0.26</td>
<td headers="df_satt_No" class="gt_row gt_right"><br /></td>
<td headers="SD_total_No" class="gt_row gt_right"><br /></td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">F(6, 6.39) = 2.37</td>
<td headers="pval_Yes" class="gt_row gt_right">0.15</td>
<td headers="df_satt_Yes" class="gt_row gt_right"><br /></td>
<td headers="SD_total_Yes" class="gt_row gt_right"><br /></td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Prereg chr</td>
<td headers="studies" class="gt_row gt_right">45</td>
<td headers="effects" class="gt_row gt_right">202</td>
<td headers="avg_effect_F_No" class="gt_row gt_left"><br /></td>
<td headers="pval_No" class="gt_row gt_right"><br /></td>
<td headers="df_satt_No" class="gt_row gt_right"><br /></td>
<td headers="SD_total_No" class="gt_row gt_right"><br /></td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left"><br /></td>
<td headers="pval_Yes" class="gt_row gt_right"><br /></td>
<td headers="df_satt_Yes" class="gt_row gt_right"><br /></td>
<td headers="SD_total_Yes" class="gt_row gt_right"><br /></td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Not preregistered</td>
<td headers="studies" class="gt_row gt_right">22</td>
<td headers="effects" class="gt_row gt_right">61</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.29 [0.18, 0.4]</td>
<td headers="pval_No" class="gt_row gt_right">0.00</td>
<td headers="df_satt_No" class="gt_row gt_right">16.3</td>
<td headers="SD_total_No" class="gt_row gt_right">0.27</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.14 [-0.08, 0.36]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.20</td>
<td headers="df_satt_Yes" class="gt_row gt_right">10.2</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.26</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Preregistered</td>
<td headers="studies" class="gt_row gt_right">23</td>
<td headers="effects" class="gt_row gt_right">141</td>
<td headers="avg_effect_F_No" class="gt_row gt_left">0.14 [0.04, 0.23]</td>
<td headers="pval_No" class="gt_row gt_right">0.01</td>
<td headers="df_satt_No" class="gt_row gt_right">13.5</td>
<td headers="SD_total_No" class="gt_row gt_right">0.19</td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">0.03 [-0.17, 0.24]</td>
<td headers="pval_Yes" class="gt_row gt_right">0.70</td>
<td headers="df_satt_Yes" class="gt_row gt_right">5.0</td>
<td headers="SD_total_Yes" class="gt_row gt_right">0.18</td></tr>
    <tr><td headers="Moderator" class="gt_row gt_left">Wald test (HTZ)</td>
<td headers="studies" class="gt_row gt_right"><br /></td>
<td headers="effects" class="gt_row gt_right"><br /></td>
<td headers="avg_effect_F_No" class="gt_row gt_left">F(1, 26.91) = 4.91</td>
<td headers="pval_No" class="gt_row gt_right">0.04</td>
<td headers="df_satt_No" class="gt_row gt_right"><br /></td>
<td headers="SD_total_No" class="gt_row gt_right"><br /></td>
<td headers="avg_effect_F_Yes" class="gt_row gt_left">F(1, 26.8) = 2.24</td>
<td headers="pval_Yes" class="gt_row gt_right">0.15</td>
<td headers="df_satt_Yes" class="gt_row gt_right"><br /></td>
<td headers="SD_total_Yes" class="gt_row gt_right"><br /></td></tr>
  </tbody>
  
  
</table>
</div>
```


:::
:::
