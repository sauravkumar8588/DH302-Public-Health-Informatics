# Load necessary libraries
library(ggplot2)
library(dplyr)

# --- Gaussian Distribution (PDF of X and X') ---
mu_X <- 0
sigma_X <- 1
x <- seq(mu_X - 4 * sigma_X, mu_X + 4 * sigma_X, length.out = 1000)
pdf_X <- dnorm(x, mean = mu_X, sd = sigma_X)

# Scaled X'
x_prime <- 4 * x
pdf_X_prime <- dnorm(x_prime, mean = 0, sd = 4) # Adjusted for variance

# Create data frames
df_X <- data.frame(observed = x, density = pdf_X, variable = "X ~ N(0,1)")
df_X_prime <- data.frame(observed = x_prime, density = pdf_X_prime, variable = "X' ~ N(0,16)")
df_combined_X <- bind_rows(df_X, df_X_prime)

# Plot PDF comparison
ggplot(df_combined_X, aes(x = observed, y = density, color = variable)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "PDF Comparison of X ~ N(0,1) and X' ~ N(0,16)",
    x = "x",
    y = "Density",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

# --- Poisson Distribution (PMF of Y and Y') ---
lambda_Y <- 5
y_vals <- 0:20 # Reasonable range for Poisson(5)
pmf_Y <- dpois(y_vals, lambda = lambda_Y)

# Scaled Y'
y_prime_vals <- 5 * y_vals
pmf_Y_prime <- pmf_Y # Same probability values, but rescaled outcomes

# Create data frames
df_Y <- data.frame(outcome = y_vals, probability = pmf_Y, variable = "Y ~ Poisson(5)")
df_Y_prime <- data.frame(outcome = y_prime_vals, probability = pmf_Y_prime, variable = "Y' = 5Y")
df_combined_Y <- bind_rows(df_Y, df_Y_prime)

# Plot PMF comparison
ggplot(df_combined_Y, aes(x = outcome, y = probability, color = variable)) +
  geom_segment(aes(xend = outcome, yend = 0), linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "PMF Comparison of Y ~ Poisson(5) and Y' = 5Y",
    x = "Outcomes",
    y = "Probability",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )
