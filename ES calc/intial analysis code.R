group_based_dat <- readRDS("ES calc/Group-based interventions data.RDS")

library(tidyverse)

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


GBD_effect |> select(contains("gt")) |> View()

GBD_effect |> select(contains("analysis")) |> glimpse()

  

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

GBD_effect$analysis_plan |> str_detect("mental")

GBD_all_mental <- GBD_effect |> 
  filter(str_detect(analysis_plan, "mental"))

V_mat_all <- vcalc(vi = vgt, cluster = authors, obs = es_id, data = GBD_all_mental, rho = 0.7)

main_model_all <- 
  rma.mv(
    gt,
    V = V_mat_all,
    random = ~ 1 | authors / es_id,
    data = GBD_all_mental
  ) |> 
  robust(cluster = authors, clubSandwich = TRUE)

# Singel vs. TAU ----
GBD_all_mental_ctr <- 
  GBD_all_mental |> 
  mutate(
    ctr_binary = case_when(
      str_detect(control, "Individual|(EST)") ~ "Single treatment",
      TRUE ~ "TAU"
    )
  )

# SCE+
V_mat_ctr <- 
  vcalc(
    vi = vgt, 
    cluster = authors, 
    obs = es_id, 
    subgroup = ctr_binary, 
    data = GBD_all_mental_ctr, 
    rho = 0.7
  )

ctr_sce_model <- 
  rma.mv(
    gt ~ 0 + ctr_binary,
    V = V_mat_ctr,
    random = list(~ ctr_binary | authors, ~ ctr_binary | es_id), 
    struct = c("DIAG", "DIAG"),
    data = GBD_all_mental_ctr,
    sparse = TRUE
  ) |> 
  robust(cluster = authors, clubSandwich = TRUE); ctr_sce_model
  
GBD_all_mental_ctr |> 
  filter(ctr_binary == "TAU")

sample(unique(GBD_all_mental_ctr$authors), 3)

