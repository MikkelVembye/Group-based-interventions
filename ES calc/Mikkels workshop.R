
library(tidyverse)
library(robvis)
library(purrr)
library(metafor)
library(patchwork)
library(clubSandwich)
library(wildmeta)
library(gt)
library(metaselection)

library(puniform)
library(tidyverse) # for tidying
library(janitor)   # for tidying variable names
library(boot)      # for bootstrapping
library(tictoc) 

# Loading in helper function used to calculate effect size and conduct the analysis
source_path <- if (isTRUE(getOption('knitr.in.progress'))) "Helpers.R" else "ES calc/Helpers.R"
source_path2 <- if (isTRUE(getOption('knitr.in.progress'))) "pub-bias-test-helpers.R" else "ES calc/pub-bias-test-helpers.R"

source(source_path)
source(source_path2)


# Loading data set
reint_dat <- 
  if (isTRUE(getOption('knitr.in.progress'))) {
    readRDS("reintergation_dat.rds")
  } else {
    readRDS("ES calc/reintergation_dat.rds")
  }

# Data
reint_dat <- readRDS("ES calc/reintergation_dat.rds") 
mental_health_dat <- readRDS("ES calc/mental_health_dat.rds") 

## Select relevant variables


reintergation_dat <- 
  reint_dat |> 
  mutate(esid = 1:n()) |> 
  select(
    study, eppi_id, esid, N_t, N_c, N_total, inv_sample_size, gt, vgt, Wgt, Wse, 
    prereg_chr, conventional, analysis_plan, rob_tool:Overall, timing
  )
 

## Overall average effect

# CHE-ISCW
V_mat <- metafor::vcalc(vi = Wgt, cluster = study, obs = esid, data = reintergation_dat, rho = 0.8)

W <- solve(V_mat)

# CHE
che <- 
  rma.mv(
    yi = gt,
    V = V_mat,
    random = ~ 1 | study / esid,
    data = reintergation_dat,
    sparse = TRUE
  ) |> 
  robust(cluster = study, clubSandwich = TRUE)

# ISCW

# CHE-ISCW-RVE
che_iscw <- rma.mv(
  yi = gt,
  V = V_mat,
  W = W,
  mods = ~ Wse,
  random = ~ 1 | study / esid,
  data = reintergation_dat,
  sparse = TRUE
) |> 
  robust(cluster = study, clubSandwich = TRUE)


## Preregistered vs. not preregistered 


prereg_dat <-  
  reintergation_dat |> 
  filter(conventional == 0)

V_mat_prereg <- metafor::vcalc(vi = Wgt, cluster = study, obs = esid, data = prereg_dat, rho = 0.8)

W_prereg <- solve(V_mat_prereg)

egg_prereg <-
  rma.mv(
    yi = gt,
    V = V_mat_prereg,
    W = W_prereg,
    mods = ~ Wse,
    random = ~ 1 | study / esid,
    data = prereg_dat,
    sparse = TRUE
  ) |> 
  robust(cluster = study, clubSandwich = TRUE)

egg_prereg_res <- 
  tibble(
    subgroup = "Preregistered",
    egg_intercept = as.numeric(egg_prereg$b[1]),
    egg_slope = as.numeric(egg_prereg$b[2])
  )

notprereg_dat <-  
  reintergation_dat |> 
  filter(conventional == 1)


V_mat_notprereg <- metafor::vcalc(vi = Wgt, cluster = study, obs = esid, data = notprereg_dat, rho = 0.8)

W_notprereg <- solve(V_mat_notprereg)

egg_notprereg <-
  rma.mv(
    yi = gt,
    V = V_mat_notprereg,
    W = W_notprereg,
    mods = ~ Wse,
    random = ~ 1 | study / esid,
    data = notprereg_dat,
    sparse = TRUE
  ) |> 
  robust(cluster = study, clubSandwich = TRUE)


egg_notprereg_res <- 
  tibble(
    subgroup = "Not preregistered",
    egg_intercept = as.numeric(egg_notprereg$b[1]),
    egg_slope = as.numeric(egg_notprereg$b[2])
  )

egg_res_subgrouped <- bind_rows(egg_prereg_res, egg_notprereg_res)
egg_res_subgrouped

# SCEp+ 
subgroup_means <- .SCEp(mod = prereg_chr, data = reintergation_dat)

subgroup_dat <- 
  reintergation_dat |> 
  summarise(
    gt = mean(gt),
    Wse = mean(Wse),
    analysis_plan = analysis_plan[1],
    .by = prereg_chr
  ) |> 
  bind_cols(subgroup_means[2:3,], egg_res_subgrouped) |> 
  mutate(slope_low = qnorm(0.025), slope_high = qnorm(0.975), level = "Effect size level") 



y_lim_exp1 <- max(reintergation_dat$Wse) + 0.02 

funnel_exp1 <-  tribble(
  ~ x90, ~ x95, ~ x99, ~ y,
  0,     0,     0,     0,
  qnorm(0.05) * y_lim_exp1, qnorm(0.025) * y_lim_exp1, qnorm(0.005) * y_lim_exp1, y_lim_exp1,
  qnorm(0.95) * y_lim_exp1, qnorm(0.975) * y_lim_exp1, qnorm(0.995) * y_lim_exp1, y_lim_exp1,
  0,     0,     0,     0
) 



alpha_line <- 0.5
polygon_fill <- c("grey", "grey10", "lightcyan")
mean_line <- "dashed"
reg_test <- TRUE
reg_line <- "longdash"
reg_color <- "blue"
breaks_y <- seq(-3, 3, 0.5)


es_level_fp <- 
  reintergation_dat |> 
  mutate(
    level = "Effect size level",
    report_bias = case_when(
      rob_tool == "RoB2" & D5 == "Low" ~ "Low",
      rob_tool == "RoB2" & str_detect(D5, "Some") ~ "Moderate",
      rob_tool == "RoB2" & str_detect(D5, "High") ~ "Serious",
      .default = D7
    ),
    
    report_bias = factor(report_bias, levels =  c("Low", "Moderate", "Serious"))
    
  ) |> 
  ggplot() + 
  geom_polygon(data = funnel_exp1, aes(x = y, y = x99), fill = polygon_fill[1], alpha = 0.5) + 
  geom_polygon(data = funnel_exp1, aes(x = y, y = x95), fill = polygon_fill[2], alpha = 0.5) + 
  geom_polygon(data = funnel_exp1, aes(x = y, y = x90), fill = polygon_fill[3], alpha = 0.7) + 
  geom_abline(data = subgroup_dat, aes(slope = slope_high, intercept = avg_effect), linetype = mean_line, alpha = alpha_line) + 
  geom_hline(data = subgroup_dat, aes(yintercept = avg_effect), linetype = mean_line, alpha = alpha_line) +  
  geom_abline(data = subgroup_dat, aes(slope = slope_low, intercept = avg_effect), linetype = mean_line, alpha = alpha_line) + 
  geom_abline(data = subgroup_dat, aes(slope = -egg_slope, intercept = egg_intercept), linetype = reg_line, color = reg_color) +
  geom_point(aes(Wse, gt, color = report_bias), alpha = 1, size = 1.5) +
  coord_flip() +
  facet_grid(level~prereg_chr) +
  scale_x_reverse(limits = c(y_lim_exp1, 0.0), expand = c(0,0)) + 
  scale_y_continuous(breaks = breaks_y) + 
  scale_color_manual(
    values = c("Low" = "green3", "Moderate" = "yellow", "Serious" = "red")
  ) + 
  theme_bw() + 
  labs(x = "Standard error (modified)", 
       y = "Standardized mean difference (Hedges' g)", 
       color = "", shape = "") +
  theme(
    legend.position = "bottom",
    strip.text.x = element_blank()
  ) +
  labs(color = "Risk of bias") +
  guides(col = guide_legend(nrow = 1))

# Make aggregate plot

reintergation_dat_agg <- 
  reintergation_dat |> 
  escalc(measure = "SMD", yi = gt, vi = Wgt, data = _) |> 
  aggregate.escalc(cluster = study, rho = 0.8) |> 
  mutate(
    Wse = sqrt(vi)
  )

prereg_dat_agg <-  
  reintergation_dat_agg |> 
  as_tibble() |> 
  dplyr::filter(conventional == 0)

egg_prereg_agg <- 
  rma(yi = yi, vi = vi, data = prereg_dat_agg, control=list(stepadj=0.5, maxiter=1000)) |> 
  regtest()

egg_prereg_agg_res <- 
  tibble(
    subgroup = "Preregistered",
    egg_intercept = as.numeric(egg_prereg_agg$fit$b[1]),
    egg_slope = as.numeric(egg_prereg_agg$fit$b[2])
  )

notprereg_dat_agg <-  
  reintergation_dat_agg |> 
  as_tibble() |> 
  dplyr::filter(conventional == 1)

egg_notprereg_agg <- 
  rma(yi = yi, vi = vi, data = notprereg_dat_agg) |> 
  regtest()

egg_notprereg_agg_res <- 
  tibble(
    subgroup = "Not preregistered",
    egg_intercept = as.numeric(egg_notprereg_agg$fit$b[1]),
    egg_slope = as.numeric(egg_notprereg_agg$fit$b[2])
  )

egg_res_agg_subgrouped <- bind_rows(egg_notprereg_agg_res, egg_prereg_agg_res)
egg_res_agg_subgrouped

means_agg <- 
  rma(yi, vi, mods = ~ -1 +prereg_chr, data = reintergation_dat_agg) |> 
  robust(cluster = study, clubSandwich = TRUE)

subgroup_means_agg <- 
  tibble(
    Moderator = c("Not preregistered", "Preregistered"),
    avg_effect = as.numeric(means_agg$b), 
    LL = as.numeric(means_agg$ci.lb), 
    UL = as.numeric(means_agg$ci.ub)
  )

subgroup_dat_agg <- 
  reintergation_dat_agg |> 
  summarise(
    gt = mean(gt),
    Wse = mean(Wse),
    analysis_plan = analysis_plan[1],
    .by = prereg_chr
  ) |> 
  arrange(prereg_chr) |> 
  bind_cols(subgroup_means_agg, egg_res_agg_subgrouped) |> 
  mutate(slope_low = qnorm(0.025), slope_high = qnorm(0.975), level = "Study level") 


#subgroup_dat <- 
#  reintergation_dat |> 
#  summarise(
#    gt = mean(gt),
#    Wse = mean(Wse),
#    analysis_plan = analysis_plan[1],
#    .by = prereg_chr
#  ) |> 
#  bind_cols(subgroup_means[2:3,], egg_res_subgrouped) |> 
#  mutate(slope_low = qnorm(0.025), slope_high = qnorm(0.975), level = "Effect size level") 



y_lim_exp2 <- max(reintergation_dat_agg$Wse) + 0.02 
y_lim_exp2  

funnel_exp2 <-  tribble(
  ~ x90, ~ x95, ~ x99, ~ y,
  0,     0,     0,     0,
  qnorm(0.05) * y_lim_exp2, qnorm(0.025) * y_lim_exp2, qnorm(0.005) * y_lim_exp2, y_lim_exp2,
  qnorm(0.95) * y_lim_exp2, qnorm(0.975) * y_lim_exp2, qnorm(0.995) * y_lim_exp2, y_lim_exp2,
  0,     0,     0,     0
) 

study_level_fp <- 
  reintergation_dat_agg |> 
  mutate(level = "Study level") |> 
  ggplot() + 
  geom_polygon(data = funnel_exp2, aes(x = y, y = x99), fill = polygon_fill[1], alpha = 0.5) + 
  geom_polygon(data = funnel_exp2, aes(x = y, y = x95), fill = polygon_fill[2], alpha = 0.5) + 
  geom_polygon(data = funnel_exp2, aes(x = y, y = x90), fill = polygon_fill[3], alpha = 0.7) + 
  geom_abline(data = subgroup_dat_agg, aes(slope = slope_high, intercept = avg_effect), linetype = mean_line, alpha = alpha_line) + 
  geom_hline(data = subgroup_dat_agg, aes(yintercept = avg_effect), linetype = mean_line, alpha = alpha_line) +  
  geom_abline(data = subgroup_dat_agg, aes(slope = slope_low, intercept = avg_effect), linetype = mean_line, alpha = alpha_line) + 
  geom_abline(data = subgroup_dat_agg, aes(slope = -egg_slope, intercept = egg_intercept), linetype = reg_line, color = reg_color) +
  geom_point(aes(Wse, gt), alpha = 1, size = 1.5) +
  scale_color_brewer(type = "qual", palette = 2) + 
  coord_flip() +
  facet_grid(level~prereg_chr) +
  scale_x_reverse(limits = c(y_lim_exp2, 0.0), expand = c(0,0)) + 
  scale_y_continuous(breaks = breaks_y) + 
  theme_bw() +
  theme(
    axis.title = element_blank()
  )

ylab <- es_level_fp$labels$x

study_level_fp$labels$x <- es_level_fp$labels$x <- "" 

#png("plots/funnel plots (overall effect) across prereg.png", width = 8, height = 5.5, res = 300, unit = "in")
study_level_fp / es_level_fp
grid::grid.draw(grid::textGrob(ylab, y = 0.6, x = 0.02, rot = 90))
#dev.off()


### Investigating the impact of Cano-Vindel et al.


prereg_dat_cano <-  
  reintergation_dat |> 
  filter(conventional == 0) |>
  mutate(
    cano_vindel = if_else(str_detect(study, "Cano"), 1, 0),
    cano_vindel = factor(cano_vindel)
  )

V_mat_prereg_cano <- metafor::vcalc(vi = Wgt, cluster = study, obs = esid, data = prereg_dat_cano, rho = 0.8)

W_prereg_cano <- solve(V_mat_prereg_cano)

egg_prereg_cano <-
  rma.mv(
    yi = gt,
    V = V_mat_prereg_cano,
    W = W_prereg_cano,
    mods = ~ Wse + cano_vindel,
    random = ~ 1 | study / esid,
    data = prereg_dat_cano,
    sparse = TRUE
  ) |> 
  robust(cluster = study, clubSandwich = TRUE)

egg_prereg_res_cano <- 
  tibble(
    subgroup = "Preregistered",
    egg_intercept = as.numeric(egg_prereg_cano $b[1]),
    egg_slope = as.numeric(egg_prereg_cano$b[2])
  )

subgroup_means <- .SCEp(mod = prereg_chr, data = reintergation_dat)

subgroup_dat_cano <- 
  prereg_dat_cano |> 
  summarise(
    gt = mean(gt),
    Wse = mean(Wse),
    analysis_plan = analysis_plan[1],
    .by = prereg_chr
  ) |> 
  bind_cols(subgroup_means[3,], egg_prereg_res_cano) |> 
  mutate(slope_low = qnorm(0.025), slope_high = qnorm(0.975)) 



y_lim_exp_cano <- max(prereg_dat_cano$Wse) + 0.02 
y_lim_exp_cano  

funnel_exp_cano <-  tribble(
  ~ x90, ~ x95, ~ x99, ~ y,
  0,     0,     0,     0,
  qnorm(0.05) * y_lim_exp_cano, qnorm(0.025) * y_lim_exp_cano, qnorm(0.005) * y_lim_exp_cano, y_lim_exp_cano,
  qnorm(0.95) * y_lim_exp_cano, qnorm(0.975) * y_lim_exp_cano, qnorm(0.995) * y_lim_exp_cano, y_lim_exp_cano,
  0,     0,     0,     0
) 



alpha_line <- 0.5
polygon_fill <- c("grey", "grey10", "lightcyan")
mean_line <- "dashed"
reg_test <- TRUE
reg_line <- "longdash"
reg_color <- "blue"
breaks_y <- seq(-3, 3, 0.5)


cano_fp <- 
  prereg_dat_cano |> 
  mutate(alpha_val = if_else(cano_vindel == 1, 0.9, 1)) |>
  ggplot() + 
  geom_polygon(data = funnel_exp_cano, aes(x = y, y = x99), fill = polygon_fill[1], alpha = 0.5) + 
  geom_polygon(data = funnel_exp_cano, aes(x = y, y = x95), fill = polygon_fill[2], alpha = 0.5) + 
  geom_polygon(data = funnel_exp_cano, aes(x = y, y = x90), fill = polygon_fill[3], alpha = 0.7) + 
  geom_abline(data = subgroup_dat_cano, aes(slope = slope_high, intercept = avg_effect), linetype = mean_line, alpha = alpha_line) + 
  geom_hline(data = subgroup_dat_cano, aes(yintercept = avg_effect), linetype = mean_line, alpha = alpha_line) +  
  geom_abline(data = subgroup_dat_cano, aes(slope = slope_low, intercept = avg_effect), linetype = mean_line, alpha = alpha_line) + 
  geom_abline(data = subgroup_dat_cano, aes(slope = -egg_slope, intercept = egg_intercept), linetype = reg_line, color = reg_color) +
  geom_point(aes(Wse, gt, col = cano_vindel, alpha = alpha_val), size = 1.5) +
  scale_color_brewer(type = "qual", palette = 2) + 
  coord_flip() +
  facet_grid(~prereg_chr) +
  scale_x_reverse(limits = c(y_lim_exp_cano, 0.0), expand = c(0,0)) + 
  scale_y_continuous(breaks = breaks_y) + 
  theme_bw() + 
  labs(x = "Standard error (modified)", 
       y = "Standardized mean difference (Hedges' g)", 
       color = "", shape = "") +
  theme(
    legend.position = "bottom"
  ) +
  labs(color = "Cano-vindel") +
  guides(col = "none", alpha = "none")

#png("plots/funnel plots (overall effect) without Cano-Vindel.png", width = 8, height = 5.5, res = 300, unit = "in")
cano_fp
#dev.off()


# van Aert methods
#See van Aert [(2025)](https://psycnet.apa.org/fulltext/2025-81751-001.html)
## Overall effect correcting for publication bias in not-preregistered studies only



## Subgroup effects correcting for publication bias in not-preregistered studies only


# Checking variance estimate vs. sample size

reintergation_dat |> 
  mutate(
    gt_above_05 = if_else(gt > 0.5, "g > 0.5", "g < 0.5"),
    gt_above_05 = factor(gt_above_05, levels = c("g > 0.5", "g < 0.5"))
  ) |> 
  ggplot(aes(Wgt, vgt, color = gt_above_05)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  theme(
    plot.caption=element_text(hjust = 0, size = 10),
    legend.position= "bottom",
    legend.title = element_blank(),
    panel.spacing.x = unit(5, "mm"), 
    panel.spacing.y = unit(5, "mm"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(expand=c(0,0), breaks = seq(0L, 1L, 0.1)) + 
  scale_y_continuous(expand=c(0,0), breaks = seq(0L, 1L, 0.1)) + 
  expand_limits(x = c(0L, 0.5), y = c(0L, 0.5))


reintergation_dat |> 
  ggplot(aes(N_total, Wse)) + 
  geom_point() + 
  theme_bw()


# Selection modeling



mod_3PSM_mod <- 
  metaselection::selection_model(
    data = reintergation_dat, 
    yi = gt,
    sei = Wse,
    cluster = study,
    selection_type = "step",
    steps = 0.025,
    CI_type = "large-sample",
    mean_mods = ~ - 1 + analysis_plan + prereg_chr
  )

mod_3PSM_mod

# BUILDING ES DIST FUNCTION

.es_dist <- function(data, color = "cornflowerblue"){
    
}

.es_dist_sub <- 
  function(
    data,
    g, 
    subgroup = analysis_plan,
    x_lab_title = "Effect size estimate",
    scale = "free",
    n_col = 2
  ) {
    
    dat <- data 
    dat$yi <- data |> dplyr::pull({{ g }})
    dat$subgrp <- data |> dplyr::pull({{ subgroup }})
    
    sub_fig_dat <- 
      dat |> 
      dplyr::reframe(
        qrtl = quantile(gt_pop,  c(.25, .75)),
        fence = qrtl + 3 * diff(qrtl) * c(-1,1),
        
        .by = subgrp
      )
    
    subgrp_plot <- 
      dat |> 
      ggplot(aes(x = yi, fill = subgrp)) + 
      geom_density(alpha = 0.7) +
      geom_vline(data = sub_fig_dat, aes(xintercept = qrtl), linetype = "solid")+
      geom_vline(data = sub_fig_dat, aes(xintercept = fence), linetype = "dashed")+
      geom_rug(alpha = 0.25) +
      facet_wrap(~subgrp, ncol = n_col, scales = scale) +
      theme_bw() +
      theme(legend.position = "none", axis.title.y = element_blank()) +
      labs(x = x_lab_title)
    
    subgrp_plot
    
    
}


mental_fig_dat <- 
  mental_health_dat |> 
  reframe(
    quantile = scales::percent(c(0.25, 0.75)),
    qrtl = quantile(gt_pop, c(.25, .75)),
    fence = qrtl + 3 * diff(qrtl) * c(-1, 1)
  )

density_mental_overall <- 
  mental_health_dat |> 
  mutate(es_group = "Overall - Mental Health Outcomes") |> 
  ggplot(aes(gt_pop)) + 
  geom_density(fill = "Light Pink") + 
  geom_vline(data = mental_fig_dat, aes(xintercept = qrtl), linetype = "solid") + 
  geom_vline(data = mental_fig_dat, aes(xintercept = fence), linetype = "dashed") + 
  geom_rug(alpha = 0.25) + 
  facet_grid(~es_group) +
  theme_bw() + 
  theme(axis.title.y = element_blank()) +
  labs(x = "Effect size estimate (including population-based SDs)")

density_mental_overall 

mental_sub_fig_dat <- 
  mental_health_dat |> 
  reframe(
    qrtl = quantile(gt_pop,  c(.25, .75)),
    fence = qrtl + 3 * diff(qrtl) * c(-1,1),
    
    .by = analysis_plan
  )

mental_outcome_plot <- 
  mental_health_dat |> 
  ggplot(aes(x = gt_pop, fill = analysis_plan)) + 
  geom_density(alpha = 0.7) +
  geom_vline(data = mental_sub_fig_dat, aes(xintercept = qrtl), linetype = "solid")+
  geom_vline(data = mental_sub_fig_dat, aes(xintercept = fence), linetype = "dashed")+
  geom_rug(alpha = 0.25) +
  facet_wrap(~analysis_plan, ncol = 2, scales = "free") +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(x = "Effect size estimate (including population-based SDs)")

mental_outcome_plot

# RIDGE PLOT
.cat_ridge <- function(data, es, v, variable, var_name) {
  require(dplyr)
  require(rlang)
  require(tidyr)
  require(ggplot2)
  
  es_exp <- enquo(es)
  var_exp <- enquo(variable)
  v_exp <- enquo(v)
  
  suppressWarnings(
    data |> 
      mutate(
        !!var_exp := fct_rev(!!var_exp),
        var_group := !!var_name,
      ) |> 
      ggplot(aes(
        x = !!es_exp,
        y = !!var_exp,
        fill = !!var_exp
      )) +
      geom_density_ridges(
        aes(
          point_colour = !!var_exp,
          point_size = 1 / !!v_exp
        ),
        alpha = .2,
        point_alpha = 0.5,
        jittered_points = TRUE
      ) +
      facet_grid(~var_group) +
      theme_minimal() +
      labs(y = "", x = "Standardized Mean Difference (Hedges' g)") +
      theme(
        legend.position = "none",
        strip.background = element_rect(colour = "black", fill = "white")
      )
  )
}

reint_ridge <- 
  .cat_ridge(
    data = reintergation_dat, 
    es = gt_pop, 
    variable = analysis_plan, 
    v = vgt_pop, 
    var_name = "Reintegrational Outcomes"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(-1, 2, 1), limits = c(-1, 2))

mental_ridge <- .cat_ridge(
  data = mental_health_dat, 
  es = gt_pop, 
  variable = analysis_plan, 
  v = vgt_pop, 
  var_name = "Mental Health Outcomes"
) 

reint_ridge + mental_ridge
