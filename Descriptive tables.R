library(dplyr)
library(purrr)
library(stringr)
library(gt)


################################################################################
#
# Reintegration descriptive tables
#
################################################################################

reintegration_dat <- 
  readRDS("reintegration_dat.rds") |> 
  mutate(
    schizophrenia = factor(schizophrenia, levels = c("Schizophrenia", "Other")),
    short_term_es =  if_else(time_after_end_intervention_weeks <= 52, "Short-term measure (< 52 weeks)", "Follow-up measure (> 52 weeks)")
  ) 

.study_avg <- function(category, data = reintegration_dat) {
  
  n_studies <- data |> pull(study) |> dplyr::n_distinct()
  n_es <- data |> nrow()
  
  res <- 
    data |> 
    group_by({{ category }}, study) |> 
    dplyr::summarise(
      n_es = dplyr::n(), 
      .groups = "drop"
    ) |> 
    group_by({{ category }}) |> 
    dplyr::summarise(
      J = n_distinct(study),
      K = sum(n_es),
      .groups = "drop"
    ) |> 
    mutate(
      perc_studies = (J/n_studies) * 100,
      perc_es = (K/n_es) * 100
    ) |> 
    rename(Category = {{ category }}, studies = J, effect_sizes = K)
  
  
  return(res)
  
}




x <- dplyr::vars(
  cnt,
  analysis_plan,
  schizophrenia,
  CBT_int,
  prereg_chr, 
  study_outlet, 
  QES_design,
  test_type,
  analysis_strategy,
  control,
  overall_rob,
  short_term_es,
  posttest_only,
  handle_multilevel
  ) 

pct_table_reint_dat <- 
  map_dfr(x, ~ .study_avg(category = !!.x)) |> 
  mutate(
    n = 1:n(),
    
    group = case_when(
      n %in% 1:15 ~ "Context",
      n %in% 16:24 ~ "Outcome measure",
      n %in% 25:26 ~ "Diagnosis in sample",
      n %in% 27:28 ~ "Intervention type",
      n %in% 29:30 ~ "Preregistration status",
      n %in% 31 ~ "Outlet",
      n %in% 32:33 ~ "Research design",
      n %in% 34:36 ~ "Test type",
      n %in% 37:38 ~ "Analysis strategy",
      n %in% 39:42 ~ "Control group",
      n %in% 43:45 ~ "Overall risk of bias",
      n %in% 46 ~ "Timing of measurement",
      n %in% 47:48 ~ "Effect size metric",
      n %in% 49:50 ~ "Handles multilevel structure",
      .default = NA_character_
    )
  ) |> 
  relocate(group) |> 
  select(-n)

pct_table_reint <- 
  pct_table_reint_dat |> 
  gt(groupname_col = "group", rowname_col = "Category") |> 
  cols_label(
    Category = "Characteristics",
    studies = "Studies (J)",
    effect_sizes = "Effects (K)",
    perc_studies = "% Studies",
    perc_es = "% Effects"
  ) |> 
  tab_stub_indent(
    rows = everything(),
    indent = 3
  ) |> 
  tab_stubhead("Characteristics") |> 
  fmt_number(columns = 5:6, decimals = 1) |>  
  tab_options(
    table_body.hlines.color = "white",
    table_body.vlines.color = "white",
    table.border.left.color = "white",    # Removes the left border
    table.border.right.color = "white",   # Removes the right border
    table.font.size = 12
  )

pct_table_reint |> gtsave("Tables/pct_table_reint.docx")

  
.mean_table <- 
  function(
    var, 
    char_name,
    digits, 
    data = reintegration_dat,
    rm_mis = TRUE
  ){
    
    
    if (char_name != "Effect sizes per study"){
    
    res <- 
      data |> 
      dplyr::filter(!is.na({{ var }})) |>
      dplyr::reframe(
        n_es = n(), 
        x = max({{ var }}),
        .by = c(study, trt_id)
      ) |> 
      summarise(
        x_mean_stud = round(mean(x, rm.na = rm_mis), digits = digits),
        n_es = sum(n_es),
        .by = study
      ) |> 
      summarise(
        Characteristic = char_name,
        N_studies = n(),
        N_ES = sum(n_es),
        Median_J = round(median(x_mean_stud, rm.na = rm_mis), digits),
        Mean_J = round(mean(x_mean_stud, rm.na = rm_mis), digits),
        SD_J = round(sd(x_mean_stud), 1),
        Range = paste0(round(min(x_mean_stud)), "-", round(max(x_mean_stud)))
      )
    
    } else {
      
      res <- 
        data |> 
        dplyr::group_by(study) |> 
        dplyr::summarise(
          kj = n(),
          .groups = "drop"
        ) |> 
        dplyr::summarise(
          Characteristic = char_name,
          N_studies = n(),
          N_ES = sum(kj),
          
          Median_J = round(median(kj, rm.na = rm_mis), digits),
          Mean_J = round(mean(kj, na.rm = rm_mis), 1),
          SD_J = round(sd(kj, na.rm = rm_mis), 1),
          Range = paste0(min(kj, na.rm = rm_mis), "-", max(kj, na.rm = rm_mis)) 
        )
      
    }
    
    res
    
  }


mean_vars <- dplyr::vars(
  N_t, N_c,
  ESS_t, ESS_c,
  age_mean,
  male_pct,
  total_number_of_sessions,
  sessions_per_week,
  duration_in_weeks,
  time_after_end_intervention_weeks,
  time_from_baseline_weeks
)

mean_var_names <- c(
  "Raw sample size of treatment group",
  "Raw sample size of control group",
  "Effective sample size of treatment group",
  "Effective sample size of control group",
  "Age",
  "% Males",
  "Total number of sessions",
  "Sessions per week",
  "Duration (in weeks)",
  "After end of intervention (in weeks)",
  "From baseline (in weeks)"
)

list_x <- tibble(
  var = mean_vars,
  char_name = mean_var_names,
  digits = rep(c(0,1), c(4,7)),
)

mean_table_reint_dat <- 
  pmap(.l = list_x, .f = .mean_table) |> 
  list_rbind() |> 
  mutate(
    n = c(1:2, 4:5, 7:13)
  )


# Handling total sample size so that all treatment groups are added together

N_stud_trtid <- 
  reintegration_dat |> 
  reframe(
    n_es = n(), 
    N_treat = max(N_t),
    N_ess = max(ESS_t),
    .by = c(study, trt_id)
  )  

N_t_total <- 
  N_stud_trtid |> 
  summarise(N_es = sum(n_es), N_t_total = sum(N_treat), ESS_t_total = sum(N_ess), .by = study)

N_c_total <- 
  reintegration_dat |> 
  summarise(N_es = n(), N_c_total = max(N_c), ESS_c_total = max(ESS_c), .by = study)

N_total_dat <- 
  left_join(N_t_total, N_c_total, by = join_by(study, N_es)) |> 
  mutate(
    N_total = N_t_total + N_c_total,
    ESS_total = ESS_t_total +  ESS_c_total
  )

N_total_reint <- 
  N_total_dat |> 
  summarise(
    Characteristic = "Total sample size",
    N_studies = n(),
    N_ES = sum(N_es),
    Median_J = round(median(N_total)),
    Mean_J = round(mean(N_total)),
    SD_J = round(sd(N_total),1),
    Range = paste0(round(min(N_total)), "-", round(max(N_total))),
    n = 3
  )

N_ESS_reint <- 
  N_total_dat |> 
  summarise(
    Characteristic = "Total effective sample size",
    N_studies = n(),
    N_ES = sum(N_es),
    Median_J = round(median(ESS_total)),
    Mean_J = round(mean(ESS_total)),
    SD_J = round(sd(ESS_total),1),
    Range = paste0(round(min(ESS_total)), "-", round(max(ESS_total))),
    n = 6
  )

# list all mean variables 
mean_table_reint_dat <- 
  mean_table_reint_dat |> 
  bind_rows(N_total_reint, N_ESS_reint) |> 
  arrange(n) |> 
  select(-n)


# Average pre-posttest correlation reintegration

ppcor_dat_reint <- 
  reintegration_dat |> 
  filter(
    str_detect(
      ppcor_method, regex("Cal", ignore_case = TRUE), 
    ) |
      str_detect(
        ppcor_method, regex("From study", ignore_case = TRUE), 
      ) 
  ) |> 
  summarise(
    Characteristic = "Pre-posttest correlation (emp. calc)",
    N_studies = n_distinct(study),
    N_ES = n(),
    Median_J = median(ppcor),
    Mean_J = mean(ppcor),
    SD_J = sd(ppcor),
    Range = paste0(round(min(ppcor), 3), "-", round(max(ppcor), 3))
    
  )

mean_table_reint_dat <- 
  mean_table_reint_dat |> 
  bind_rows(ppcor_dat_reint) |> 
  mutate(
    group = rep(
      c("Sample characteristics", "Intervention characteristics", "Measurement timing", "Methodological features"), 
      c(8,3,2,1)
      )
  )

mean_table_reint <- 
  mean_table_reint_dat |> 
  gt(groupname_col = "group", rowname_col = "Characteristic") |> 
  cols_label(
    N_studies = "J",
    N_ES = "K",
    Median_J = "Median",
    Mean_J = "Average",
    SD_J = "SD"
  ) |> 
  tab_stub_indent(
    rows = everything(),
    indent = 3
  ) |> 
  tab_stubhead("Characteristics") |> 
  fmt_number(columns = 4:6, decimals = 2) 

mean_table_reint |> gtsave("Tables/mean_table_reint.docx")

################################################################################
#
# Mental health descriptive tables
#
################################################################################

mental_health_dat <- 
  readRDS("mental_health_dat.rds") |> 
  mutate(
    schizophrenia = factor(schizophrenia, levels = c("Schizophrenia", "Other")),
    short_term_es =  if_else(time_after_end_intervention_weeks <= 52, "Short-term measure (< 52 weeks)", "Follow-up measure (> 52 weeks)")
  )


x <- dplyr::vars(
  cnt,
  analysis_plan,
  schizophrenia,
  CBT_int,
  prereg_chr, 
  study_outlet, 
  QES_design,
  test_type,
  analysis_strategy,
  control,
  overall_rob,
  short_term_es,
  posttest_only,
  handle_multilevel
) 

pct_table_mental_dat <- 
  map(.x = x, .f = .study_avg, data = mental_health_dat) |> 
  list_rbind() |> 
  mutate(
    n = 1:n(),
    
    group = case_when(
      n %in% 1:15 ~ "Context",
      n %in% 16:19 ~ "Outcome measure",
      n %in% 20:21 ~ "Diagnosis in sample",
      n %in% 22:23 ~ "Intervention type",
      n %in% 24:25 ~ "Preregistration status",
      n %in% 26 ~ "Outlet",
      n %in% 27:28 ~ "Research design",
      n %in% 29:30 ~ "Test type",
      n %in% 31:32 ~ "Analysis strategy",
      n %in% 33:36 ~ "Control group",
      n %in% 37:39 ~ "Overall risk of bias",
      n %in% 40 ~ "Timing of measurement",
      n %in% 41 ~ "Effect size metric",
      n %in% 42:43 ~ "Handles multilevel structure",
      .default = NA_character_
    )
  ) |> 
  relocate(group) |> 
  select(-n)

pct_table_mental <- 
  pct_table_mental_dat |> 
  gt(groupname_col = "group", rowname_col = "Category") |> 
  cols_label(
    Category = "Characteristics",
    studies = "Studies (J)",
    effect_sizes = "Effects (K)",
    perc_studies = "% Studies",
    perc_es = "% Effects"
  ) |> 
  tab_stub_indent(
    rows = everything(),
    indent = 3
  ) |> 
  tab_stubhead("Characteristics") |> 
  fmt_number(columns = 5:6, decimals = 1) |>  
  tab_options(
    table_body.hlines.color = "white",
    table_body.vlines.color = "white",
    table.border.left.color = "white",    # Removes the left border
    table.border.right.color = "white",   # Removes the right border
    table.font.size = 12
  )

#pct_table_mental |> gtsave("Tables/pct_table_mental.docx")



mean_table_mental_dat <- 
  pmap(.l = list_x, .f = .mean_table, data = mental_health_dat) |> 
  list_rbind() |> 
  mutate(
    n = c(1:2, 4:5, 7:13)
  )


# Handling total sample size so that all treatment groups are added together

N_stud_trtid_mental <- 
  mental_health_dat |> 
  reframe(
    n_es = n(), 
    N_treat = max(N_t),
    N_ess = max(ESS_t),
    .by = c(study, trt_id)
  )  

N_t_total_mental <- 
  N_stud_trtid_mental |> 
  summarise(N_es = sum(n_es), N_t_total = sum(N_treat), ESS_t_total = sum(N_ess), .by = study)

N_c_total_mental <- 
  mental_health_dat |> 
  summarise(N_es = n(), N_c_total = max(N_c), ESS_c_total = max(ESS_c), .by = study)

N_total_dat_mental <- 
  left_join(N_t_total_mental, N_c_total_mental, by = join_by(study, N_es)) |> 
  mutate(
    N_total = N_t_total + N_c_total,
    ESS_total = ESS_t_total +  ESS_c_total
  )

N_total_mental <- 
  N_total_dat_mental |> 
  summarise(
    Characteristic = "Total sample size",
    N_studies = n(),
    N_ES = sum(N_es),
    Median_J = round(median(N_total)),
    Mean_J = round(mean(N_total)),
    SD_J = round(sd(N_total),1),
    Range = paste0(round(min(N_total)), "-", round(max(N_total))),
    n = 3
  )

N_ESS_mental <- 
  N_total_dat_mental |> 
  summarise(
    Characteristic = "Total effective sample size",
    N_studies = n(),
    N_ES = sum(N_es),
    Median_J = round(median(ESS_total)),
    Mean_J = round(mean(ESS_total)),
    SD_J = round(sd(ESS_total),1),
    Range = paste0(round(min(ESS_total)), "-", round(max(ESS_total))),
    n = 6
  )

# list all mean variables 
mean_table_mental_dat <- 
  mean_table_mental_dat |> 
  bind_rows(N_total_mental, N_ESS_mental) |> 
  arrange(n) |> 
  select(-n)


# Average pre-posttest correlation reintegration

ppcor_dat_mental <- 
  mental_health_dat |> 
  filter(
    str_detect(
      ppcor_method, regex("Cal", ignore_case = TRUE), 
    ) |
      str_detect(
        ppcor_method, regex("From study", ignore_case = TRUE), 
      ) 
  ) |> 
  summarise(
    Characteristic = "Pre-posttest correlation (emp. calc)",
    N_studies = n_distinct(study),
    N_ES = n(),
    Median_J = median(ppcor),
    Mean_J = mean(ppcor),
    SD_J = sd(ppcor),
    Range = paste0(round(min(ppcor), 3), "-", round(max(ppcor), 3))
    
  )

mean_table_mental_dat <- 
  mean_table_mental_dat |> 
  bind_rows(ppcor_dat_mental) |> 
  mutate(
    group = rep(
      c("Sample characteristics", "Intervention characteristics", "Measurement timing", "Methodological features"), 
      c(8,3,2,1)
    )
  )

mean_table_mental <- 
  mean_table_mental_dat |> 
  gt(groupname_col = "group", rowname_col = "Characteristic") |> 
  cols_label(
    N_studies = "J",
    N_ES = "K",
    Median_J = "Median",
    Mean_J = "Average",
    SD_J = "SD"
  ) |> 
  tab_stub_indent(
    rows = everything(),
    indent = 3
  ) |> 
  tab_stubhead("Characteristics") |> 
  fmt_number(columns = 4:6, decimals = 2) 

#mean_table_mental |> gtsave("Tables/mean_table_mental.docx")





