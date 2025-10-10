tau2_within  <- 0.2^2         # level-2 variance (within-study)
tau2_between <- 0.1^2         # level-3 variance (between-study)
tau_within   <- sqrt(tau2_within)
tau_between  <- sqrt(tau2_between)

mu_hat  <- 0.195
se_mu   <- 0.02

# ------------------------------------------------------------
# 3A) Predictive distribution of the TRUE effect (θ) for:
#     - New effect in a NEW study: Var = tau3^2 + tau2^2
#     - New effect within an EXISTING study: Var = tau2^2
# We’ll visualize both on the SMD scale.
# ------------------------------------------------------------
S_newstudy_true_sd <- sqrt(tau2_between + tau2_within)
S_within_true_sd   <- sqrt(tau2_within)

z80 <- qnorm(0.90)                      # 1.28155...
pi80_lb <- mu_hat - z80 * S_newstudy_true_sd
pi80_ub <- mu_hat + z80 * S_newstudy_true_sd
c(pi80_lb, pi80_ub)

Ndraw <- 10000
theta_new_study <- rnorm(Ndraw, mean = mu_hat, sd = S_newstudy_true_sd)
theta_within    <- rnorm(Ndraw, mean = mu_hat, sd = S_within_true_sd)

dens <- density(theta_new_study, adjust = 2)

# Convert to data frame
dens_df <- data.frame(x = dens$x, y = dens$y)

# Split into two regions: x ≤ 0 and x > 0
dens_df$region <- ifelse(dens_df$x > 0, "Above 0", "Below 0")

# Probability mass above 0 (just for annotation)
prop_above0 <- mean(theta_new_study > 0)

ggplot(dens_df, aes(x = x, y = y)) +
  geom_area(data = subset(dens_df, x > 0),
            fill = "steelblue", alpha = 0.5, show.legend = FALSE) +
  geom_line(linewidth = 1, color = "black") +
  geom_vline(xintercept = 0, linewidth = 0.6) +
  geom_vline(xintercept = c(pi80_lb, pi80_ub), linetype = "dashed") +
  labs(
    title = "Predictive distribution",
    subtitle = sprintf("P(θ > 0) ≈ %.2f", prop_above0),
    x = "Effect size estimate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) 

