# Load required libraries
library(tidyverse)
library(fitdistrplus)

# Read the data (adjust the file path as needed)
df <- read_csv("./NTA_all_marks_2024.csv")

# Remove any rows with missing marks
df <- df %>% filter(!is.na(Marks))

# Define offset for distributions requiring strictly positive values
offset <- abs(min(df$Marks)) + 1
df <- df %>% mutate(Marks_offset = Marks + offset)

# Fit distributions
fit_norm  <- fitdist(df$Marks, "norm")
fit_lnorm <- fitdist(df$Marks_offset, "lnorm")
fit_gamma <- fitdist(df$Marks_offset, "gamma")
fit_pois  <- fitdist(round(df$Marks_offset), "pois")

# AIC values for comparison
AIC_norm  <- 2 * length(fit_norm$estimate)  - 2 * fit_norm$loglik
AIC_lnorm <- 2 * length(fit_lnorm$estimate) - 2 * fit_lnorm$loglik
AIC_gamma <- 2 * length(fit_gamma$estimate) - 2 * fit_gamma$loglik
AIC_pois  <- 2 * length(fit_pois$estimate)  - 2 * fit_pois$loglik

# Print AIC values
cat("AIC values:\n")
cat("Normal:", AIC_norm, "\n")
cat("Lognormal:", AIC_lnorm, "\n")
cat("Gamma:", AIC_gamma, "\n")
cat("Poisson:", AIC_pois, "\n")

# Plot histogram and fitted densities
ggplot(df, aes(x = Marks)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "lightblue", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = fit_norm$estimate[1], sd = fit_norm$estimate[2]), 
                color = "red", size = 1.2, linetype = "solid") +
  stat_function(fun = dlnorm, args = list(meanlog = fit_lnorm$estimate[1], sdlog = fit_lnorm$estimate[2]), 
                color = "blue", size = 1.2, linetype = "dashed") +
  stat_function(fun = dgamma, args = list(shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]), 
                color = "green", size = 1.2, linetype = "dotted") +
  labs(title = "Distribution of NEET-UG 2024 Marks with Fitted Densities",
       x = "Marks",
       y = "Density") +
  theme_minimal() +
  scale_color_manual(name = "Fitted Distributions",
                     values = c("red" = "Normal", "blue" = "Lognormal", "green" = "Gamma"))
