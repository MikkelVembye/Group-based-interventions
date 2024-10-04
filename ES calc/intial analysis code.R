GBD_effect <- group_based_dat |> 
  filter(variable_type != "Binary") |>
  mutate(
    es_id = 1:n(),
    gt = if_else(!is.na(gt_post), gt_post, NA_real_),
    gt = if_else(!is.na(gt_DD), gt_DD, gt),
    gt = if_else(!is.na(gt_adj), gt_adj, gt),
    gt = if_else(!is.na(gt_reg), gt_reg, gt),
    
    vgt = if_else(!is.na(vgt_post), vgt_post, NA_real_),
    vgt = if_else(!is.na(vgt_DD), vgt_DD, vgt),
    vgt = if_else(!is.na(vgt_adj), vgt_adj, vgt),
    vgt = if_else(!is.na(vgt_reg), vgt_reg, vgt),
    
  )

covariates$variable_type |> unique()


dat |> select(contains("gt")) |> glimpse()

GBD_effect |> select(contains("gt")) |> View()

GBD_effect |> select(contains("analysis")) |> glimpse()



GBD_effect$analysis_plan |> str_detect("mental")

GBD_all_mental <- GBD_effect |> 
  filter(str_detect(analysis_plan, "mental"))
  

# str_detect(outcome_measure, "All_mental")    

# Overall avg mean ----
library(metafor)

V_mat <- vcalc(vi = vgt, cluster = authors, obs = es_id, data = GBD_effect, rho = 0.7)

main_employ_model <- 
  rma.mv(
    gt,
    V = V_mat,
    random = ~ 1 | authors / es_id,
    data = GBD_effect
  ) |> 
  robust(cluster = authors, clubSandwich = TRUE)

# All mental health ----

V_mat_all <- vcalc(vi = vgt, cluster = authors, obs = es_id, data = GBD_all_mental, rho = 0.7)

main_employ_model_all <- 
  rma.mv(
    gt,
    V = V_mat_all,
    random = ~ 1 | authors / es_id,
    data = GBD_all_mental
  ) |> 
  robust(cluster = authors, clubSandwich = TRUE)

# Singel vs. TAU ----


