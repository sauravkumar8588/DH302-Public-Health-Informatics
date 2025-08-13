# Load necessary libraries
library(ggplot2)

# Read data (assuming it is a TSV file)
df <- read.table("Assignment02_grip_strength.tsv", header=TRUE, sep="\t")

# Calculate difference in grip strength
df$diff <- df$baseline - df$measured_6weekslater

# Shapiro-Wilk test for normality
shapiro.test(df$diff)

# Q-Q plot for normality check
qqnorm(df$diff)
qqline(df$diff)

# Boxplot to detect outliers
boxplot(df$diff, main="Boxplot of Grip Strength Differences")

# Perform paired t-test
t_test_result <- t.test(df$baseline, df$measured_6weekslater, paired=TRUE)
t_test_result$p.value

# Perform Wilcoxon Signed-Rank Test
wilcox.test(df$baseline, df$measured_6weekslater, paired=TRUE)


# Remove rows where baseline or measured_6weekslater is zero or negative
df <- df[df$baseline > 0 & df$measured_6weekslater > 0, ]

# Remove missing values
df <- na.omit(df)

# Apply log transformation
df$log_baseline <- log(df$baseline)
df$log_measured_6weekslater <- log(df$measured_6weekslater)
df$log_diff <- df$log_baseline - df$log_measured_6weekslater

# Normality check on log-transformed differences
shapiro.test(df$log_diff)

# Q-Q plot for log-transformed differences
qqnorm(df$log_diff)
qqline(df$log_diff)

# Perform paired t-test on log-transformed data
t.test(df$log_baseline, df$log_measured_6weekslater, paired=TRUE)
