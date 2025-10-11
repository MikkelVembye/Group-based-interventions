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
S_newstudy_true_sd <- sqrt(tau2_between + tau2_within + se_mu^2)
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

dens_df |> 
dplyr::mutate(outcome = "Reintegration") |> 
ggplot(aes(x = x, y = y)) +
  geom_area(data = subset(dens_df, x > 0),
            fill = "steelblue", alpha = 0.5, show.legend = FALSE) +
  geom_line(linewidth = 1, color = "black") +
  geom_vline(xintercept = 0, linewidth = 0.6) +
  geom_vline(xintercept = c(pi80_lb, pi80_ub), linetype = "dashed") +
  facet_grid(~outcome) + 
  labs(
    title = "Predictive distribution",
    caption = sprintf("P(θ > 0) ≈ %.2f", prop_above0),
    x = "Effect size estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) 


library(ggplot2)

# -----------------------------
# Parameters
# -----------------------------
mu_hat  <- 0.195          # pooled mean
tau2_within  <- 0.2^2
tau2_between <- 0.1^2
S_newstudy_true_sd <- sqrt(tau2_between + tau2_within)

k <- 20                   # number of studies
df <- k - 1               # degrees of freedom

# -----------------------------
# Predictive distribution: t(df)
# -----------------------------
x_vals <- seq(mu_hat - 4*S_newstudy_true_sd,
              mu_hat + 4*S_newstudy_true_sd,
              length.out = 2000)

dens_df <- data.frame(
  x = x_vals,
  y = dt((x_vals - mu_hat) / S_newstudy_true_sd, df = df) / S_newstudy_true_sd
)

# 95% t-based prediction interval
alpha <- 0.05
tcrit <- qt(1 - alpha/2, df)
pi_lb <- mu_hat - tcrit * S_newstudy_true_sd
pi_ub <- mu_hat + tcrit * S_newstudy_true_sd

# Heights of the curve at those bounds
y_pi <- approx(dens_df$x, dens_df$y, xout = c(pi_lb, pi_ub))$y

# Probability that theta > 0 under t(df)
# P(θ > 0) = 1 - CDF_t( (0 - μ) / σ )
prop_above0 <- 1 - pt((0 - mu_hat) / S_newstudy_true_sd, df = df)

# -----------------------------
# Plot
# -----------------------------
ggplot(dens_df, aes(x, y)) +
  # Shade area above 0
  geom_area(data = subset(dens_df, x > 0), fill = "steelblue", alpha = 0.5) +
  geom_line(linewidth = 1) +
  # Dashed PI lines that stop at curve
  geom_segment(aes(x = pi_lb, xend = pi_lb,
                   y = 0, yend = y_pi[1]),
               linetype = "dashed") +
  geom_segment(aes(x = pi_ub, xend = pi_ub,
                   y = 0, yend = y_pi[2]),
               linetype = "dashed") +
  # Reference line at 0
  geom_vline(xintercept = 0, linewidth = 0.6) +
  labs(
    title = "Predictive distribution (t-distribution)",
    subtitle = sprintf("df = %d,  P(θ > 0) ≈ %.2f", df, prop_above0),
    x = "Effect size estimate", y = "Density"
  ) +
  theme_minimal()

dens_df |> 
  dplyr::mutate(outcome = "Reintegration") |> 
  ggplot(aes(x = x, y = y)) +
  geom_area(data = subset(dens_df, x > 0),
            fill = "steelblue", alpha = 0.5, show.legend = FALSE) +
  geom_line(linewidth = 1, color = "black") +
  geom_vline(xintercept = 0, linewidth = 0.6) +
  geom_vline(xintercept = c(pi80_lb, pi80_ub), linetype = "dashed") +
  facet_grid(~outcome) + 
  labs(
    title = "Predictive distribution",
    caption = sprintf("P(θ > 0) ≈ %.2f", prop_above0),
    x = "Effect size estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  )