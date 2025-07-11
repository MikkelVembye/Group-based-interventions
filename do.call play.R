library(tidyverse)
library(future)
library(furrr)

reintegration_dat <- readRDS("reintegation_dat.rds")
mental_health_dat <- readRDS("mental_health_dat.rds")

reint_dat <- 
  reintegration_dat |> 
  filter(str_detect(analysis_plan, "Alco|Hope|Social|Well")) |> 
  mutate(
    esid = 1:n()
  )

metafor_func <- 
  function(
    formula, 
    data = reint_dat, 
    rand,
    struc
  ) {
    
    require(metafor) |> 
      suppressPackageStartupMessages()
    
    rma.mv(
      formula,
      V = vgt_pop, 
      random =  rand,
      struct = struc,
      data = data,
      sparse=TRUE
    )
    
  }

metafor_func(
  gt_pop ~ -1 + analysis_plan + time_after_end_intervention_weeks, 
  rand = list(~analysis_plan | study, ~analysis_plan | esid),
  struc = c("DIAG", "DIAG")
)


rma_res <- 
  metafor::rma.mv(
  gt_pop ~ 1,
  V = vgt_pop, 
  rand =  ~ 1 | studyid / esid,
  #struct = NULL,
  data = mental_health_dat ,
  sparse=TRUE
)

x <- list(
  formula = gt_pop ~ -1 + analysis_plan + time_after_end_intervention_weeks,
  rand = list(~analysis_plan | study, ~analysis_plan | esid),
  struc = c("DIAG", "DIAG")
)

do.call(metafor_func, x)

y <- list(
  formula = gt_pop ~ -1 + analysis_plan,
  rand = list(~analysis_plan | study, ~analysis_plan | esid),
  struc = c("DIAG", "DIAG")
)

z <- list(
  formula = gt_pop ~ -1 + analysis_plan,
  rand = list(~analysis_plan | study),
  struc = "DIAG"
)


list_fun <- function(x){
  list(
    formula = x,
    rand = list(~analysis_plan | study, ~analysis_plan | esid),
    struc = c("DIAG", "DIAG")
  )
}

map_list <- map(list("WO" = gt_pop ~ -1 + analysis_plan + time_after_end_intervention_weeks, "WC" = gt_pop ~ -1 + analysis_plan), ~ list_fun(.x))



rma_args <- list(
  `Without mod` = x,
  `With mod` = y,
  `No withtin-study var` = z
)

plan(multisession)
map_res <- future_map(map_list, ~ do.call(metafor_func, .x))
plan(sequential)

#fun <- c("metafor_func")
#
#fun_arg_tib <- tibble(f = fun, fa = rma_args)
#
#pmap(fun_arg_tib, ~ do.call(..1, ..2))


###############################################################################

rma_arg_fun <- 
  function(formula, structure = c("DIAG", "DIAG"), model = "SCE") {
    
    if (model == "SCE"){
      
      main_pred <- labels(terms(formula))[1]
      
      outer_form <- substitute(~ moderator | studyid, list(moderator = as.name(main_pred))) |> as.formula()
      inner_form <- substitute(~ moderator | esid, list(moderator = as.name(main_pred))) |> as.formula()
      
      res <- 
        list(
          formula = formula,
          rand = list(outer_form, inner_form),
          struc = structure
        )
      
    } else if (model == "CHE") {
      
      res <- list(
        formula = formula,
        rand =  ~ 1 | study / esid
      )
      
    }
    
    res
    
}

list1 <- rma_arg_fun(gt_pop ~ -1 + analysis_plan)

res_rma <- do.call(metafor_func, list1)
res_rma
