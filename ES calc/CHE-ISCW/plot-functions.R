# graphing functions

dropLeadingZero <- function(l){str_replace(l, '0(?=.)', '')}

# plot for each criteria
# this function does not work if using different criteria for rowfacet

plot_SR <- function(dat, x, y, rowfacet = NULL, colfacet = NULL, scale = FALSE) {
  
  criteria <- dplyr::enexpr(y)
  rowfacet <- dplyr::enexpr(rowfacet)
  colfacet <- dplyr::enexpr(colfacet)
  
  if (criteria == "bias") {
    ylab <- "Bias"
    hline <- geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed")
  } else if (criteria == "rmse") {
    ylab <- "RMSE"
    hline <- geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed")
  } else if (criteria == "coverage") {
    ylab <- "Coverage"
    hline <- geom_hline(aes(yintercept = 0.95), color = "black", linetype = "dashed")
  } else if (criteria == "width") {
    ylab <- "Width"
    hline <- NULL
  }
  
  if (scale == FALSE) {
    scales <- NULL
  } else {
    scales <- "free_y"
  }
  
  # need to improve the function for scenario when the col facet is method
  if (rowfacet == "mu" & colfacet == "method") {
    p <- 
      dat |> 
      ggplot(aes(x = {{x}}, y = {{y}}, fill = dependence, color = dependence)) +
      geom_boxplot(alpha = .3, coef = Inf) +
      facet_grid(paste0("mu == ", mu) ~ method, scales = scales, labeller = label_parsed
                 # switch = "y"
                 ) +
      scale_x_discrete(breaks = c(.05, .125, .25, .5, .8, 1),
                       labels = dropLeadingZero)
    
  } else if (rowfacet == "tau" & colfacet == "method") {
    p <- 
      dat |> 
      ggplot(aes(x = {{x}}, y = {{y}}, fill = dependence, color = dependence)) +
      geom_boxplot(alpha = .3, coef = Inf) +
      facet_grid(paste0("tau == ", tau) ~ method, scales = scales, labeller = label_parsed) +
      scale_x_discrete(breaks = c(.05, .125, .25, .5, .8, 1),
                       labels = dropLeadingZero)
  } else if (rowfacet == "tau" & colfacet == "k") {
    # To show general pattern so include bias, RMSE, coverage,and width on the rows
    p <-
      dat |>
      mutate(k = factor(k, levels = c(10, 30, 60, 100),
                        labels = c("10", "30", "60", "100"))) |>
      ggplot(aes(x = {{x}}, y = {{y}}, fill = method, color = method)) +
      geom_boxplot(alpha = .3, coef = Inf) +
      facet_grid(paste0("tau == ", tau) ~ paste0("k == ", k), scales = scales, labeller = label_parsed) +
      labs(linetype = "")
  } else if (rowfacet == "tau" & colfacet == "mu") {
    
    if (length(unique(dat$method)) <= 4 & length(unique(dat$method)) > 1) {
      p <- 
        dat |> 
        ggplot(aes(x = {{x}}, y = {{y}}, color = method, group = method, shape = method)) +
        geom_point() +
        geom_line(aes(linetype = method), linewidth = 0.7) +
        facet_grid(paste0("tau == ", tau) ~ paste0("mu == ", mu), 
                   scales = scales, labeller = label_parsed) +
        labs(linetype = "", shape = "") +
        scale_x_discrete(breaks = c(.05, .125, .25, .5, .8, 1),
                         labels = dropLeadingZero)
    } else if (length(unique(dat$method)) > 4) {
      p <- 
        dat |> 
        ggplot(aes(x = {{x}}, y = {{y}}, color = method, group = method, shape = method)) +
        geom_point() +
        scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7)) +
        geom_line(linewidth = 0.7) + 
        facet_grid(paste0("tau == ", tau) ~ paste0("mu == ", mu), 
                   scales = scales, labeller = label_parsed) +
        labs(shape = "") +
        scale_x_discrete(breaks = c(.05, .125, .25, .5, .8, 1),
                         labels = dropLeadingZero)
    } else {
      p <- 
        dat |> 
        ggplot(aes(x = {{x}}, y = {{y}}, fill = dependence, color = dependence)) +
        geom_boxplot(alpha = .3, coef = Inf) +
        facet_grid(paste0("tau == ", tau) ~ paste0("mu == ", mu), 
                   scales = scales, labeller = label_parsed)
    }
  } else {
    stop("Please check your specifications. Note that this function is not ready for column facet of weights.")
  }
  
  p +
    hline + 
    labs(x = "Selection weight", y = ylab, fill = "", color = "") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
}

plot_reg <- function(dat) {
  
  yints <- data.frame(criteria = c("Bias", "Coverage"), yint = c(0, 0.95))
  yints <- yints %>%  
    mutate(criteria = factor(criteria, levels = c("Bias", "Coverage")))
  
  dat %>% 
    select(wts, bias, rmse, coverage, width, method, dependence) |> 
    mutate(dependence = ifelse(dependence == "MV", "Multivariate", "Univariate")) |> 
    pivot_longer(cols = bias:width, names_to = "criteria") |>
    mutate(criteria = factor(criteria, 
                             levels = c("bias", "rmse", "coverage", "width"),
                             labels = c("Bias", "RMSE", "Coverage", "Width"))) |> 
    ggplot(aes(x = wts, y = value, color = dependence, fill = dependence)) +
    geom_boxplot(alpha = .3, coef = Inf) +
    facet_grid(criteria ~ method, scales = "free_y") +
    geom_hline(data = yints, aes(yintercept = yint), col = "black", linetype = "dashed") +
    labs(x = "Selection weight", y = "", fill = "", color = "") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_x_discrete(breaks = c(.05, .125, .25, .5, .8, 1),
                     labels = dropLeadingZero)
  
}

plot_che_iscw <- function(dat) {
  yints <- data.frame(criteria = c("Bias", "Coverage"), yint = c(0, 0.95))
  yints <- yints %>%  
    mutate(criteria = factor(criteria, levels = c("Bias", "Coverage")))
  
  dat %>% 
    filter(method %in% c("CHE", "CHE-ISCW"),
           k_stop %in% c("NA", 7)) %>% 
    select(mu, wts, sel_mechanism, bias, rmse, coverage, width, method) %>% 
    pivot_longer(cols = bias:width, names_to = "criteria") %>%
    mutate(criteria = factor(criteria, 
                             levels = c("bias", "rmse", "coverage", "width"),
                             labels = c("Bias", "RMSE", "Coverage", "Width"))) %>% 
    ggplot(aes(x = wts, y = value, color = method, fill = method)) +
    geom_boxplot(alpha = .3, coef = Inf) +
    facet_grid(criteria ~ sel_mechanism, scales = "free_y") +
    geom_hline(data = yints, aes(yintercept = yint), col = "black", linetype = "dashed") +
    labs(x = "Selection weight", y = "", fill = "", color = "") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_x_discrete(breaks = c(.05, .125, .25, .5, .8, 1),
                     labels = dropLeadingZero)
  
}

