# Load required libraries
library(MASS)
library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(readr)

# Function to fit models and calculate AIC
DoFit <- function(x) {
  # Fit normal distribution
  fit_norm <- fitdistr(x$abortion_deaths, densfun = "normal")
  
  # Fit Poisson distribution
  fit_poisson <- fitdistr(x$abortion_deaths, densfun = "poisson")
  
  # Fit Gamma distribution
  fit_gamma <- fitdistr(x$abortion_deaths, densfun = "gamma")
  
  # Calculate AIC for each model
  aic_values <- c(
    Normal = AIC(fit_norm),
    Poisson = AIC(fit_poisson),
    Gamma = AIC(fit_gamma)
  )
  
  # Prepare data for plotting the distributions
  norm_pdf <- dnorm(x$abortion_deaths, mean = fit_norm$estimate[["mean"]], sd = fit_norm$estimate[["sd"]])
  pois_pdf <- dpois(x$abortion_deaths, lambda = fit_poisson$estimate)
  gamma_pdf <- dgamma(x$abortion_deaths, shape = fit_gamma$estimate[["shape"]], rate = fit_gamma$estimate[["rate"]])
  
  # Create data frames for plotting
  df_fit_norm <- data.frame(x = x$abortion_deaths, probability = norm_pdf)
  df_fit_pois <- data.frame(x = x$abortion_deaths, probability = pois_pdf)
  df_fit_gamma <- data.frame(x = x$abortion_deaths, probability = gamma_pdf)
  
  df_merged <- bind_rows(list(Normal = df_fit_norm, Poisson = df_fit_pois, Gamma = df_fit_gamma), .id = "Type")
  
  # Plot the distributions
  plot <- ggplot(x, aes(x = abortion_deaths)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "gray", color = "black") +
    geom_line(data = df_merged, aes(x = x, y = probability, color = Type), linewidth = 1) +
    theme_minimal() +
    labs(title = "Fit of Normal, Poisson, and Gamma Models")
  
  return(list(plot = plot, aic = aic_values))
}

# Load the dataset
df_abortions <- read_tsv("India_abortion_deaths.tsv")

# Filter data for the years 2009-2010 and 2019-2020
deaths_2009_2010 <- df_abortions %>% filter(year %in% c(2009, 2010))
deaths_2019_2020 <- df_abortions %>% filter(year %in% c(2019, 2020))

# Fit models for both periods
fit_2009_2010 <- DoFit(deaths_2009_2010)
fit_2019_2020 <- DoFit(deaths_2019_2020)

# Plot the results for 2009-2010 and 2019-2020
fit_2009_2010$plot
fit_2019_2020$plot

# AIC values for both periods
fit_2009_2010$aic
fit_2019_2020$aic
