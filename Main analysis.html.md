---
title: "Main analysis (group-based)"
author: "Mikkel H. Vembye"
subtitle: ""
date: "2025-08-15"
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
      knitr.kable.NA: "-"
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
  mutate(
    esid = 1:n(),
    
    # Outcome variables
    outcome_type = case_match(
      analysis_plan, 
      c("Employment", "Physical health", "Psychiatric hospitalization") ~ "Other",
      .default = analysis_plan
    )
    
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
    rho = 0.8
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
tau^2.4    0.0000  0.0000      4     no                                      Other 
tau^2.5    0.0000  0.0000      5     no                                Self-esteem 
tau^2.6    0.0249  0.1577     16     no  Social functioning (degree of impairment) 
tau^2.7    0.0044  0.0662     24     no              Wellbeing and quality of life 

outer factor: esid         (nlvls = 202)
inner factor: outcome_type (nlvls = 7)

              estim    sqrt  k.lvl  fixed                                      level 
gamma^2.1    0.0465  0.2156     31     no              Alcohol and drug abuse/misuse 
gamma^2.2    0.0280  0.1675     32     no          Hope, empowerment & self-efficacy 
gamma^2.3    0.0103  0.1017      5     no                                 Loneliness 
gamma^2.4    0.0000  0.0000      6     no                                      Other 
gamma^2.5    0.0243  0.1559     14     no                                Self-esteem 
gamma^2.6    0.0242  0.1557     47     no  Social functioning (degree of impairment) 
gamma^2.7    0.0282  0.1678     67     no              Wellbeing and quality of life 

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
outcome_typeOther                                        0.2733  0.0453   6.0362     3.4   0.0063  
outcome_typeSelf-esteem                                  0.4281  0.0709   6.0363    3.96   0.0039  
outcome_typeSocial functioning (degree of impairment)    0.1462  0.0705   2.0745   12.81   0.0588  
outcome_typeWellbeing and quality of life                0.2120  0.0452   4.6921   13.88   0.0004  
                                                         ci.lb¹   ci.ub¹      
outcome_typeAlcohol and drug abuse/misuse              -0.1021   0.2571       
outcome_typeHope, empowerment & self-efficacy           0.1207   0.4024    ** 
outcome_typeLoneliness                                 -0.5237   0.5858       
outcome_typeOther                                       0.1383   0.4083    ** 
outcome_typeSelf-esteem                                 0.2304   0.6257    ** 
outcome_typeSocial functioning (degree of impairment)  -0.0063   0.2986     . 
outcome_typeWellbeing and quality of life               0.1150   0.3089   *** 

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
          data = list(data)
        )
      
    } else if (model == "CHE") {
      
      res <- 
        tibble(
          formula = list(formula),
          var = vi,
          rand = list(~ 1 | study / esid),
          rho = r,
          data = list(data)
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
    yi = rep(c("gt_pop", "gt"), each = 10),
    vi = rep(c("vgt_pop", "vgt"), each = 10),
    covars = rep(c("outcome_type", "outcome_type;prereg_chr"), 10),
    model = "SCEp",
    r = rep(seq(0, 0.8, 0.2), 4)
  )

arg_list_tbl <- 
  pmap(.l = arg_tbl, .f = .rma_arg_tbl, data = reint_ma_dat) |> 
  list_rbind()

arg_list_tbl
```
````

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1 × 6
  formula   var     rand       structure   rho data                
  <list>    <chr>   <list>     <list>    <dbl> <list>              
1 <formula> vgt_pop <list [2]> <chr [2]>   0.8 <tibble [202 × 308]>
# A tibble: 20 × 6
   formula   var     rand       structure   rho data                
   <list>    <chr>   <list>     <list>    <dbl> <list>              
 1 <formula> vgt_pop <list [2]> <chr [2]>   0   <tibble [202 × 308]>
 2 <formula> vgt_pop <list [2]> <chr [2]>   0.2 <tibble [202 × 308]>
 3 <formula> vgt_pop <list [2]> <chr [2]>   0.4 <tibble [202 × 308]>
 4 <formula> vgt_pop <list [2]> <chr [2]>   0.6 <tibble [202 × 308]>
 5 <formula> vgt_pop <list [2]> <chr [2]>   0.8 <tibble [202 × 308]>
 6 <formula> vgt_pop <list [2]> <chr [2]>   0   <tibble [202 × 308]>
 7 <formula> vgt_pop <list [2]> <chr [2]>   0.2 <tibble [202 × 308]>
 8 <formula> vgt_pop <list [2]> <chr [2]>   0.4 <tibble [202 × 308]>
 9 <formula> vgt_pop <list [2]> <chr [2]>   0.6 <tibble [202 × 308]>
10 <formula> vgt_pop <list [2]> <chr [2]>   0.8 <tibble [202 × 308]>
11 <formula> vgt     <list [2]> <chr [2]>   0   <tibble [202 × 308]>
12 <formula> vgt     <list [2]> <chr [2]>   0.2 <tibble [202 × 308]>
13 <formula> vgt     <list [2]> <chr [2]>   0.4 <tibble [202 × 308]>
14 <formula> vgt     <list [2]> <chr [2]>   0.6 <tibble [202 × 308]>
15 <formula> vgt     <list [2]> <chr [2]>   0.8 <tibble [202 × 308]>
16 <formula> vgt     <list [2]> <chr [2]>   0   <tibble [202 × 308]>
17 <formula> vgt     <list [2]> <chr [2]>   0.2 <tibble [202 × 308]>
18 <formula> vgt     <list [2]> <chr [2]>   0.4 <tibble [202 × 308]>
19 <formula> vgt     <list [2]> <chr [2]>   0.6 <tibble [202 × 308]>
20 <formula> vgt     <list [2]> <chr [2]>   0.8 <tibble [202 × 308]>
```


:::
:::
