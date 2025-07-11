# Building list function

library(rlang)
library(tidyverse)
library(metafor)
library(clubSandwich)
library(wildmeta)

reint_dat <- readRDS("reintegation_dat.rds") |> 
  filter(str_detect(analysis_plan, "Alco|Hope|Social|Well"))

attr(reint_dat, "data_name") <- "reint_dat"

mental_health_dat <- readRDS("mental_health_dat.rds")
attr(mental_health_dat, "data_name") <- "mental_health_dat"



.arg_list <- 
  function(
    yi, vi, covars, r, model, data
  ){
  
  covariates <- if (str_detect(covars, ";")) str_split_1(covars, pattern = ";") else covars 
    
  formula <- reformulate(covariates, response = yi, intercept = FALSE)
  
  if (str_detect(model, "SCE")) {
    
    main_pred <- labels(terms(formula))[1]
    
    outer_form <- substitute(~ moderator | study, list(moderator = as.name(main_pred))) |> as.formula()
    inner_form <- substitute(~ moderator | esid, list(moderator = as.name(main_pred))) |> as.formula()
    
    random <-  if (model == "SCEp") list(outer_form, inner_form) else list(outer_form)
    struct <- rep("DIAG", length(random))
    
    res <- 
      list(
        formula = formula,
        var = vi,
        rand = random,
        structure = struct,
        rho = r,
        data = data
    )
    
  } else if (model == "CHE") {
    
    res <- 
      list(
        formula = formula,
        var = vi,
        rand = ~ 1 | study / esid,
        rho = r,
        data = data
      )
    
  }
  
  res
  
}

list_test <- 
  .arg_list(
    yi = "gt_pop", 
    vi = "vgt_pop", 
    covars = "prereg_chr", 
    model = "SCEp", 
    r = 0.8,
    data = mental_health_dat
)

.arg_tbl <- 
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
        list(
          formula = list(formula),
          var = vi,
          rand = list(~ 1 | study / esid),
          rho = r,
          data = list(data)
        )
      
    }
    
    res
    
  }

.arg_tbl(
  yi = "gt_pop", 
  vi = "vgt_pop", 
  covars = "prereg_chr", 
  model = "SCEp", 
  r = 0.8,
  data = mental_health_dat
)


tib_test <- tibble(
  formula = list(gt_pop ~ prereg_chr - 1),
  var = "vgt_pop",
  rand = list(list(~prereg_chr | study, ~prereg_chr | esid)),
  structure = list(c("DIAG", "DIAG")),
  rho = 0.8, 
  data = list(mental_health_dat)
)


#tib_x <- 
#  tibble(
#   yi = rep(c("gt_pop", "gt"), each = 4, 2),
#   vi = paste0("v", yi),
#   covars = rep(c("analysis_plan", "analysis_plan;time_after_end_intervention_weeks"), each = 4, 2),
#   model = "SCEp",
#   r = rep(c(0, 0.2, 0.5, 0.8), 4),
#   data = rep(c(list(reint_dat), list(mental_health_dat)), each = 8)
#)
#
#long_list <- pmap(.l = tib_x, .f = .arg_list)


rma_fun <- function(formula, var, rand, structure, rho, data){
  
  suppressPackageStartupMessages(require(metafor))
  
  data$vi <- data[[var]]
  
  V_mat <- vcalc(vi = vi, cluster = study, obs = esid, data = data, rho = rho)
  assign("V_mat", V_mat, envir = .GlobalEnv)
  
  data_name <- attr(data, "data_name")
  
  # res_test1
  #as.formula(formula)
  
  # res_test2
  #substitute(structure)
  
  # res_test3 
  #rand
  
  res <- rma.mv(
    formula,
    V = V_mat, 
    rand = rand,
    struct = structure,
    data = data,
    sparse=TRUE
  )
 
  
  res$call <- as.call(call2(
    "rma.mv", 
    yi = formula, 
    V = as.name("V_mat"), 
    data = as.name(data_name), 
    random = rand, 
    struct = structure, 
    sparse = TRUE, 
    .ns = "metafor"
  ))
  
  res
  
  #clubSandwich::Wald_test(
  #  res,
  #  constraints = constrain_equal(1:2),
  #  vcov = "CR2"
  #)
  

  #cwb_res <- Wald_test_cwb(full_model = res,
  #              constraints = constrain_equal(1:2),
  #              R = 1999,
  #              seed = 080725)
  #
  #rm(V_mat, envir = .GlobalEnv)
  #
  #return(cwb_res)
  
}

plan(multisession)
rma_fun(
  formula = gt_pop ~ prereg_chr - 1, 
  var = "vgt_pop",
  rand = list(~prereg_chr | study, ~prereg_chr | esid),
  structure = c("DIAG", "DIAG"),
  rho = 0.8,
  data = mental_health_dat
)
plan(sequential) 

V_mat <- vcalc(vi = vgt_pop, cluster = study, obs = esid, data = mental_health_dat, rho = 0.8)

rma_obj <- metafor::rma.mv(
  yi = gt_pop ~ prereg_chr - 1,
  V = V_mat, 
  rand = list(~ prereg_chr | study, ~ prereg_chr | esid),
  struct = c("DIAG", "DIAG"),
  data = mental_health_dat,
  sparse=TRUE
)


plan(multisession)
cbt_obj <- Wald_test_cwb(
  full_model = rma_obj,
  constraints = constrain_equal(1:2),
  R = 1999,
  seed = 080725
)
plan(sequential) 

cbt_obj


tib_test <- tibble(
  formula = list(gt_pop ~ prereg_chr - 1),
  var = "vgt_pop",
  rand = list(list(~prereg_chr | study, ~prereg_chr | esid)),
  structure = list(c("DIAG", "DIAG")),
  rho = 0.8, 
  data = list(mental_health_dat)
)

pmap_single_res <- pmap(.l = tib_test, .f = rma_fun)

plan(multisession)
pmap(.l = tib_test, .f = rma_fun)
plan(sequential)

plan(multisession)
res_test <- do.call(rma_fun, list_test); res_test
plan(sequential)

res_test1 <- do.call(rma_fun, list_test)
res_test2 <- do.call(rma_fun, list_test)
res_test3 <- do.call(rma_fun, list_test)

#res_test$call <- as.call(call2("rma.mv", yi = res_test1, V = as.name("V_mat"), data = as.name(data_name), #random = res_test3, struct = res_test2, sparse = TRUE, .ns = "metafor"))

res_test$call <- rma_obj$call 

plan(multisession)
res_list_test <- future_map(long_list, ~ do.call(rma_fun, .x))
plan(sequential)

#rma_model <- rma.mv(yi = d ~ 0 + study_type + hrs + test,
#                    V = V,
#                    random = ~ study_type | study,
#                    data = SATcoaching,
#                    subset = !is.na(hrs) & !is.na(test))
#
#
#
#Wald_test_cwb(full_model = rma_model,
#              constraints = constrain_equal(1:3),
#              R = 19,
#              seed = 20210314)



# Add to function
x_fun <- function(x){
  
  V_mat <- x + 2
  
  assign("V_mat", V_mat, envir = .GlobalEnv)
  
  V_mat_next <- data.frame(V = V_mat, b = 1)
  rm(V_mat, envir = .GlobalEnv)
  V_mat_next
}





