# Define the log-likelihood function for Poisson
log_likelihood_poisson <- function(lambda, x) {
  if (lambda <= 0) {
    return(-Inf)  # Log-likelihood is undefined for non-positive lambda
  }
  sum(dpois(x, lambda, log = TRUE))  # Compute log-likelihood
}

# Generate sample data from Poisson distribution
set.seed(123)  # For reproducibility
n <- 100  # Number of observations
true_lambda <- 4  # True Poisson parameter
x <- rpois(n, true_lambda)  # Simulate Poisson random variables
plot(x)
# Compute the MLE for lambda (sample mean)
plot(lambda_mle)
# Print results
cat("The MLE for lambda is:", lambda_mle, "\n")

# Plot log-likelihood function
lambda_values <- seq(2, 6, length.out = 100)  # Lambda values to evaluate
log_likelihood_values <- sapply(lambda_values, log_likelihood_poisson, x = x)

plot(lambda_values, log_likelihood_values, type = "l", col = "blue", lwd = 2,
     xlab = "Lambda", ylab = "Log-Likelihood", main = "Log-Likelihood Function for Poisson")
abline(v = lambda_mle, col = "red", lty = 2)  # Vertical line at MLE
legend("bottomright", legend = c("Log-Likelihood", "MLE Estimate"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 2))

