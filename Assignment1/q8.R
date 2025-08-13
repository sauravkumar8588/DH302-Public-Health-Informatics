# Load required libraries
library(tidyverse)
library(ggplot2)
library(entropy)  # For KL divergence

# Read the data (adjust path if necessary)
df <- read_csv("NTA_2023_2024_score_bins.csv")

# Ensure correct column names
colnames(df) <- c("Score_bin", "Frequency_2023", "Frequency_2024")

# Normalize frequencies to account for different total candidates
df <- df %>%
  mutate(Freq_2023_norm = Frequency_2023 / sum(Frequency_2023),
         Freq_2024_norm = Frequency_2024 / sum(Frequency_2024))

# 📌 1. Plot the score distributions
ggplot(df, aes(x = Score_bin)) +
  geom_bar(aes(y = Freq_2023_norm), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = Freq_2024_norm), stat = "identity", fill = "red", alpha = 0.5) +
  labs(title = "Comparison of Score Distributions: NEET 2023 vs 2024",
       x = "Score Bin", y = "Normalized Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(name = "Year", values = c("blue" = "2023", "red" = "2024"))

# 📌 2. Chi-Square Test (Are distributions significantly different?)
chi_test <- chisq.test(df$Frequency_2023, df$Frequency_2024)
cat("Chi-square test p-value:", chi_test$p.value, "\n")

# 📌 3. KL Divergence (Measures deviation from expected distribution)
kl_div <- KL.empirical(df$Freq_2023_norm, df$Freq_2024_norm)
cat("KL Divergence:", kl_div, "\n")

# 📌 4. High-score anomaly detection (700+ scores)
df_high_scores <- df %>% filter(as.numeric(str_extract(Score_bin, "[0-9]+")) >= 700)
high_score_diff <- sum(df_high_scores$Frequency_2024) - sum(df_high_scores$Frequency_2023)
cat("Excess students scoring 700+ in 2024 compared to 2023:", high_score_diff, "\n")

# 📌 5. Conclusion based on statistical tests
if (chi_test$p.value < 0.05 || kl_div > 0.1 || high_score_diff > 1000) {
  cat("\n🚨 Potential anomalies detected in NEET-UG 2024 scores! Further investigation recommended.\n")
} else {
  cat("\n✅ No strong statistical evidence of widespread cheating in NEET-UG 2024.\n")
}
