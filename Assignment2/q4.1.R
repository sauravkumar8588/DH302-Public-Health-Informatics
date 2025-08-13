# Simulating two normal samples and applying t-test
set.seed(42)
alpha <- 0.05
n_rejections <- 0
N_tries <- 10000
n_sample_size <- 200

for (i in 1:N_tries) {
  sample1 <- rnorm(n_sample_size, mean = 10, sd = 1)
  sample2 <- rnorm(n_sample_size, mean = 10, sd = 1)
  t_test_result <- t.test(sample1, sample2)
  if (t_test_result$p.value < alpha) {
    n_rejections <- n_rejections + 1
  }
}

# Output the proportion of rejections
n_rejections / N_tries