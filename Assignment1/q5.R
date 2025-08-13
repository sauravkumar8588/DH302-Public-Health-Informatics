# Load necessary libraries
library(tidyverse)

# Read the dataset
df_abortions <- read_tsv("India_abortion_deaths.tsv")

# Filter data for years 2009 and 2019
deaths_2009 <- df_abortions %>% filter(year == 2009)
deaths_2019 <- df_abortions %>% filter(year == 2019)

# Combine the data
df_2009_and_2019 <- bind_rows(deaths_2009, deaths_2019)

# Transform to wide format
df_wide <- df_2009_and_2019 %>%
  dplyr::select(month, year, abortion_deaths) %>%
  pivot_wider(names_from = year, values_from = abortion_deaths)

# Rename columns for clarity
colnames(df_wide) <- c("Month", "Deaths_2009", "Deaths_2019")

# Convert to matrix for chi-square test
death_matrix <- as.matrix(df_wide[, 2:3])

# Perform chi-square test
chi_test <- chisq.test(death_matrix)

# Output the results
print(chi_test)

# Check p-value
if (chi_test$p.value < 0.01) {
  print("Reject the null hypothesis: The distribution of deaths has changed significantly.")
} else {
  print("Fail to reject the null hypothesis: No significant change in the distribution of deaths.")
}
