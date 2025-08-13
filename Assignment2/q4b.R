set.seed(42)
alpha <- 0.05
n_rejections <- sum(replicate(10000, 
                              t.test(rnorm(100, 10, 1), rnorm(100, 10, 2.5))$p.value < alpha))
n_rejections / 10000
